{-# LANGUAGE ForeignFunctionInterface #-}

module Xapian where

import Data.ByteString.Char8
import Foreign
import Foreign.C.String
import Foreign.C.Types

--
-- For testing - here's how it looks to clients
--
testStuff =
  do (Right db) <- openWritableDatabase "test.db" createOrOverwriteDB
     doc <- newDocument
     addPosting doc "A posting" 1
     addDocument db doc
     return ()

-- Public API (sort of, I need to decide what to export)
data Document = Document !(ForeignPtr XapianDocument)
                deriving (Eq, Show)

data WritableDatabase = WritableDatabase !(ForeignPtr XapianWritableDatabase)
                      deriving (Eq, Show)

newtype CreateDBOption = CreateDBOption { unCreateDBOption :: Int }
                         deriving (Show, Eq)

createOrOpenDB      = CreateDBOption 1
createDB            = CreateDBOption 2
createOrOverwriteDB = CreateDBOption 3
openDB              = CreateDBOption 4

openWritableDatabase filename options =
  useAsCString (pack filename) $ \cFilename ->
  alloca $ \errorPtr -> do
    dbHandle <- c_xapian_writable_db_new cFilename options errorPtr
    if dbHandle == nullPtr
      then do err <- peekCString =<< peek errorPtr
              return (Left err)
      else do managed <- newForeignPtr finalizerFree dbHandle
              return (Right $ WritableDatabase managed)

addDocument (WritableDatabase db) (Document doc) = do
  withForeignPtr doc $ \docptr ->
    withForeignPtr db $ \dbptr ->
    c_xapian_database_add_document dbptr docptr

newDocument = do
  document <- c_xapian_document_new
  managed <- newForeignPtr finalizerFree document
  return (Document managed)

setDocumentData (Document document) docData =
  useAsCString (pack docData) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_set_data doc_ptr dat

addPosting (Document document) term pos =
  useAsCString (pack term) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_add_posting doc_ptr dat pos


-- Private stuff

type XapianWritableDatabase = ()
type XapianDocument = ()

foreign import ccall "cxapian.h xapian_writable_db_new"
  c_xapian_writable_db_new :: CString ->
                              CreateDBOption ->
                              Ptr CString ->
                              IO (Ptr XapianWritableDatabase)

foreign import ccall "cxapian.h xapian_writable_db_add_document"
  c_xapian_database_add_document :: Ptr XapianWritableDatabase ->
                                    Ptr XapianDocument ->
                                    IO ()

foreign import ccall "cxapian.h xapian_document_new"
  c_xapian_document_new :: IO (Ptr XapianDocument)

foreign import ccall "cxapian.h xapian_document_set_data"
  c_xapian_document_set_data :: Ptr XapianDocument -> CString -> IO ()

foreign import ccall "cxapian.h xapian_document_add_posting"
  c_xapian_document_add_posting :: Ptr XapianDocument ->
                                   CString ->
                                   Int ->
                                   IO ()
