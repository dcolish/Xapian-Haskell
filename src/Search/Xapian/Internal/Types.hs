-- | this module is not intended to be made visible
module Search.Xapian.Internal.Types
       ( Error (..)
       , NativeError (..)

       , WritableDatabase (..)
       , Database (..)
       , castPtr
       , castForeignPtr

       , Query (..)
       , OpNullary (..)
       , OpUnary (..)
       , OpBinary (..)
       , OpMulti (..)
       , QueryPtr

       , DocumentPtr
       , ValueNumber
       , Value
       , Document (..)
       , SimpleDocument
       , DocumentDiff (..)
       , Prefixable (..)
       , Fieldless
       , DocumentId (..)
       , Term (..)
       , Pos

       , StemPtr
       , Stemmer (..)
       ) where

import Foreign
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Sequence (Seq)

import Search.Xapian.Internal.FFI

-- * Error types
-- --------------------------------------------------------------------

-- | Error, inspired (blatantly copied) by hdbc
--
-- The main Xapian exception object. As much information as possible is passed from
-- the database through to the application through this object.
--
-- Errors generated in the Haskell layer will have seNativeError set to
-- Nothing.
-- 



data Error = Error { xeNativeError :: Maybe NativeError
                   , seErrorMsg :: String
                   } deriving (Show)

data NativeError
    = DatabaseOpeningError
    | DocNotFoundError
    | GenericError
    deriving (Eq, Show)


-- * Database related types
-- --------------------------------------------------------------------

data Database fields dat = Database !(ForeignPtr CDatabase)
    deriving (Eq, Show)

data WritableDatabase fields dat = WritableDatabase !(ForeignPtr CWritableDatabase)
    deriving (Eq, Show)

-- * Query related types
-- --------------------------------------------------------------------

-- Internal Representation of Queries

data Query
    = MatchNothing        -- ^ does not match anything
    | MatchAll            -- ^ matches everything
    | Atom ByteString
    | Parsed  Stemmer ByteString -- ^ parsed natively by Xapian
    | Nullary OpNullary
    | Unary   OpUnary   Query
    | Binary  OpBinary  Query  Query
    | Multi   OpMulti  [Query]
    deriving (Show)

data OpNullary
    = OpValueGE {-# UNPACK #-} !ValueNumber Value
    | OpValueLE {-# UNPACK #-} !ValueNumber Value
    | OpValueRange {-# UNPACK #-} !ValueNumber [Value]
    deriving (Show)

data OpUnary
    = OpScaleWeight {-# UNPACK #-} !Double -- Xapian::InvalidArgumentError if scale is negative
    deriving (Show)

data OpBinary
    = OpOr 
    | OpEliteSet
    | OpAnd 
    | OpXor 
    | OpAndMaybe 
    | OpAndNot 
    | OpFilter 
    | OpNear Int
    deriving (Show)

data OpMulti
    = OpSynonym
    | OpPhrase {-# UNPACK #-} !Int
    deriving (Show)


-- * Document related types
-- --------------------------------------------------------------------

-- * document fields
type ValueNumber = Word32
type Value       = ByteString

class Ord fields => Prefixable fields where
    getPrefix   :: fields -> ByteString
    stripPrefix :: fields -> ByteString -> Maybe ByteString
    allFields   :: [fields]

data Fieldless = Fieldless deriving (Show, Ord, Eq)

instance Prefixable Fieldless where
    getPrefix   Fieldless = BS.empty
    stripPrefix Fieldless = const Nothing
    allFields   = []

type SimpleDocument = Document Fieldless

-- | doc_id == 0 is invalid; what is the range of
newtype DocumentId = DocId { getDocId :: Word32 }
    deriving (Show, Eq)

-- | A @Document@
data Document fields dat = Document
    { documentPtr   :: Maybe DocumentPtr
    , documentId    :: Maybe DocumentId
    , documentLazyStem  :: Maybe Stemmer
    , documentLazyValues :: Maybe (IntMap Value)
    , documentLazyTerms :: Maybe [Term]
    , documentLazyFields :: Maybe (Map fields [ByteString])
    , documentLazyData  :: Maybe dat
    , documentDiffs :: Seq (DocumentDiff dat)
    } deriving (Show)

-- FIXME: unpack stuff to save space
data DocumentDiff dat
    = AddTerm ByteString {-# UNPACK #-} !Word32
    | DelTerm ByteString
    | AddTerms [(ByteString, Word32)]
    | DelTerms [ByteString]
    | AddPosting ByteString {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
    | DelPosting ByteString {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
    | AddPostings [(ByteString, Word32, Word32)]
    | DelPostings [(ByteString, Word32, Word32)]
    | AddValue {-# UNPACK #-} !Word32 ByteString
    | DelValue {-# UNPACK #-} !Word32
    | AddRawText ByteString (Maybe Stemmer)
    | SetData dat
    | ClearTerms
    | ClearValues
    deriving (Show)

data Term = Term    ByteString [Pos] -- ^ a single term w/ or wo/
                                     --   positional information
          | RawText ByteString       -- ^ a text to be parsed as terms
  deriving (Eq, Show)

-- * Stemming related types
-- --------------------------------------------------------------------

data Stemmer = Danish
             | Dutch
             | DutchKraaijPohlmann -- ^ A different Dutch stemmer
             | English       -- ^ Martin Porter's 2002 revision of his stemmer
             | EnglishLovins -- ^ Lovin's stemmer
             | EnglishPorter -- ^ Porter's stemmer as described in his 1980 paper
             | Finnish
             | French
             | German
             | German2 -- ^ Normalises umlauts and ß
             | Hungarian
             | Italian
             | Norwegian
             | Portuguese
             | Romanian
             | Russian
             | Spanish
             | Swedish
             | Turkish
             deriving (Show, Eq)
