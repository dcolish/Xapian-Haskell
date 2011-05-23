module Search.Xapian.Document
     ( -- * Constructor
       emptyDocument
     
       -- * Terms, Fields, and Postings
     , setData
     , addTerm, addTerms, getTerms
     , addPosting, addPostings
     , addRawText
     , addField, addFields, getField
     -- , getValue, setValue
     -- , clearValues, clearTerms
     , fieldsFromTerms

       -- * Low level
     , applyAccChanges
     ) where

import Foreign

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.ByteString.Char8 (ByteString, useAsCString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence ((|>), ViewL (..))
import qualified Data.Sequence as Seq
import Data.Serialize

import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.Internal.Utils
import Search.Xapian.Internal.FFI

emptyDocument :: Document fields dat
emptyDocument = Document Nothing Nothing Nothing Nothing Nothing Nothing Nothing Seq.empty

-- | @setData@ adds data to a document
setData :: (Serialize dat, Prefixable fields) => 
           dat -> Document fields dat -> Document fields dat
setData dat = queueDiff (SetData dat)

queueDiff :: (Serialize dat, Prefixable fields) 
             => DocumentDiff dat
             -> (Document fields dat -> Document fields dat)
queueDiff diff =
  \ doc@Document{documentDiffs = queue} -> doc {documentDiffs = queue |> diff}


-- FIXME: this does not return terms manually added ... for now
getTerms :: Document fields dat -> Maybe [Term]
getTerms = documentLazyTerms

addTerm :: (Serialize dat, Prefixable fields)
           => ByteString 
           -> Document fields dat 
           -> Document fields dat
addTerm term = queueDiff $ AddTerm term 1

addTerms :: (Serialize dat, Prefixable fields) 
            => [ByteString]
            -> Document fields dat
            -> Document fields dat
addTerms terms = queueDiff $ AddTerms (map (\x->(x,1)) terms)

-- clearTerms = queueDiff ClearTerms

addPosting :: (Serialize dat, Prefixable fields) 
              => ByteString
              -> Word32
              -> Document fields dat
              -> Document fields dat
addPosting term pos = queueDiff $ AddPosting term pos 1

addPostings :: (Serialize dat, Prefixable fields) 
               => [(ByteString, Word32)]
               -> Document fields dat
               -> Document fields dat
addPostings terms = queueDiff $ AddPostings (map (\(term,pos) -> (term,pos,1)) terms)

addRawText :: (Serialize dat, Prefixable fields)
              => ByteString
              -> Maybe Stemmer
              -> Document fields dat
              -> Document fields dat
addRawText bs stemmer = queueDiff $ AddRawText bs stemmer

addField :: (Serialize dat, Prefixable fields, Prefixable field)
            => field
            -> ByteString
            -> Document fields dat
            -> Document fields dat
addField field value = addTerm $ getPrefix field `BS.append` value

addFields :: (Serialize dat, Prefixable field, Prefixable fields)
             => Map field ByteString
             -> Document fields dat
             -> Document fields dat
addFields fieldsMap = addTerms $ map mapper $ Map.toList fieldsMap
    where
      mapper (field, value) = getPrefix field `BS.append` value

getField :: Prefixable fields => fields -> Document fields dat -> Maybe [ByteString]
getField field doc = do fieldsMap <- documentLazyFields doc
                        Map.lookup field fieldsMap

-- clearValues = queueDiff ClearValues

-- FIXME: I am a stupid algorithm :D
-- XXX:dc: go is a terrible function name
fieldsFromTerms :: Prefixable fields => [Term] -> Map fields [ByteString]
fieldsFromTerms = Map.fromListWith (++) . go
  where
    go (Term term []:rest) =
      let ll = [(f, [fromJust $ stripPrefix f term]) 
               | (f, pre) <- prefixes, pre `BS.isPrefixOf` term]
      in
       case ll of
         field:_ -> field : go rest
         _       -> go rest
    go (_:rest) = go rest
    go [] = []
    prefixes = map (id &&& getPrefix) allFields

---
-- Low Level
-- XXX:dc: Low Level should really be Internal
-- copies the internally used @DocumentPtr@ and applies the changes
applyAccChanges :: Serialize dat => Document fields dat -> IO DocumentPtr
applyAccChanges document =
 do docFPtr' <- case documentPtr document of
      Nothing      -> manage =<< cx_document_new
      Just docFPtr -> withForeignPtr docFPtr $ \docPtr -> 
        manage =<< cx_document_copy docPtr
    withForeignPtr docFPtr' $ \docPtr -> applyDiffSeq (documentDiffs document) docPtr
    return docFPtr'

applyDiffSeq :: Serialize a
                => Seq.Seq (DocumentDiff a) 
                -> Ptr CDocument 
                -> IO ()
applyDiffSeq diffSeq docFPtr =
  case Seq.viewl diffSeq of
    EmptyL       -> return ()
    diff :< diffSeq' -> applyDiff diff docFPtr >> applyDiffSeq diffSeq' docFPtr

applyDiff :: Serialize a
             => DocumentDiff a
             -> Ptr CDocument
             -> IO ()
applyDiff diff docPtr =
  case diff of
    AddTerm term wdfinc -> applyWdf cx_document_add_term term wdfinc
    DelTerm term -> applyNoPos cx_document_remove_term term
    AddPosting term pos wdfinc ->
      applyPos cx_document_add_posting term pos wdfinc
    DelPosting term pos wdfdec ->
      applyPos cx_document_remove_posting term pos wdfdec
    AddValue val bs -> applyVal cx_document_add_value val bs
    DelValue val -> cx_document_remove_value docPtr val
    AddTerms terms -> forM_ terms $ \(term,wdfinc) ->
      applyWdf cx_document_add_term term wdfinc
    DelTerms terms -> forM_ terms $ \term ->
      applyNoPos cx_document_remove_term term
    AddPostings terms ->
      forM_ terms $ \(term,pos,wdfinc) ->
      applyPos cx_document_add_posting term pos wdfinc
    DelPostings terms ->
      forM_ terms $ \(term,pos,wdfdec) ->
      applyPos cx_document_remove_posting term pos wdfdec
    AddRawText rawtext stemmer ->
      indexToDocument docPtr stemmer rawtext
    SetData dat ->
      useAsCString (unnullify $ encode dat) $ \cDat ->
      cx_document_set_data docPtr cDat
    ClearTerms -> cx_document_clear_terms docPtr
    ClearValues -> cx_document_clear_values docPtr
    where
      applyWdf f term wdf = useAsCString term $ \t -> f docPtr t wdf
      applyPos f term pos wdf = useAsCString term $ \t -> f docPtr t pos wdf
      applyNoPos f term = useAsCString term $ \t -> f docPtr t
      applyVal f val bs = useAsCString bs $ \t -> f docPtr val t