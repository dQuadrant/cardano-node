{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Helpers
  ( HelpersError(..)
  , deprecationWarning
  , ensureNewFile
  , ensureNewFileLBS
  , pPrintCBOR
  , readCBOR
  , renderHelpersError
  , validateCBOR
  , hushM
  , readAndDecodeShelleyGenesis
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, left, hoistEither, firstExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified System.Console.ANSI as ANSI
import           System.Console.ANSI
import qualified System.IO as IO

import           Cardano.Binary (Decoder, fromCBOR)
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.CLI.Types

import qualified System.Directory as IO
import Cardano.CLI.Shelley.Errors
import qualified Data.Aeson as Aeson
import Cardano.Ledger.Shelley.API (ShelleyGenesis)
import Ouroboros.Consensus.Cardano.Block (StandardShelley)
import qualified Data.ByteString.Lazy as LBS
import Cardano.Api.Shelley (FileError(FileIOError))

decodeCBOR
  :: LByteString
  -> (forall s. Decoder s a)
  -> Either HelpersError (LB.ByteString, a)
decodeCBOR bs decoder =
  first CBORDecodingError $ deserialiseFromBytes decoder bs

deprecationWarning :: String -> IO ()
deprecationWarning cmd = do
  ANSI.hSetSGR IO.stderr [SetColor Foreground Vivid Yellow]
  IO.hPutStrLn IO.stderr $ "WARNING: This CLI command is deprecated.  Please use "
                         <> cmd <> " command instead."
  ANSI.hSetSGR IO.stderr [Reset]

-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT HelpersError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ IO.doesPathExist outFile
  when exists $
    left $ OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> ByteString -> ExceptT HelpersError IO ()
ensureNewFileLBS = ensureNewFile BS.writeFile

pPrintCBOR :: LByteString -> ExceptT HelpersError IO ()
pPrintCBOR bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (remaining, decodedVal) -> do
      liftIO . putTextLn . toS . prettyHexEnc $ encodeTerm decodedVal
      unless (LB.null remaining) $
        pPrintCBOR remaining

readCBOR :: FilePath -> ExceptT HelpersError IO LByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . toS . displayException)
    (LB.readFile fp)

validateCBOR :: CBORObject -> LByteString -> Either HelpersError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron epochSlots -> do
      () <$ decodeCBOR bs (fromCBORABlockOrBoundary epochSlots)
      Right "Valid Byron block."

    CBORDelegationCertificateByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Delegation.Certificate)
      Right "Valid Byron delegation certificate."

    CBORTxByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s UTxO.Tx)
      Right "Valid Byron Tx."

    CBORUpdateProposalByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Update.Proposal)
      Right "Valid Byron update proposal."

    CBORVoteByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Update.Vote)
      Right "Valid Byron vote."

-- | Convert an Either to a Maybe and execute the supplied handler
-- in the Left case.
hushM :: forall e m a. Monad m => Either e a -> (e -> m ()) -> m (Maybe a)
hushM r f = case r of
  Right a -> return (Just a)
  Left e -> f e >> return Nothing

readAndDecodeShelleyGenesis
  :: FilePath
  -> IO (Either ShelleyGenesisCmdError (ShelleyGenesis StandardShelley))
readAndDecodeShelleyGenesis fpath = runExceptT $ do
  lbs <- handleIOExceptT (ShelleyGenesisCmdGenesisFileReadError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyGenesisCmdGenesisFileDecodeError fpath . Text.pack)
    . hoistEither $ Aeson.eitherDecode' lbs