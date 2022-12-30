{-# LANGUAGE OverloadedStrings #-}

module Cardano.CLI.Shelley.Errors where
import           Cardano.Prelude hiding (unlines)
import           Prelude (String,unlines)

import Cardano.Api
import Cardano.Api.Shelley
import qualified Ouroboros.Consensus.HardFork.Combinator as Qry
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import Ouroboros.Consensus.Cardano.Block (EraMismatch)
import qualified Data.ByteString.Lazy as LBS
import Cardano.Binary (DecoderError)
import qualified Data.Text as Text
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch(..))
import Cardano.CLI.Types (VerificationKeyFile (VerificationKeyFile))
import Cardano.CLI.Shelley.Run.Address (ShelleyAddressCmdError, renderShelleyAddressCmdError)
import Cardano.CLI.Shelley.Run.Pool (ShelleyPoolCmdError, renderShelleyPoolCmdError)
import Cardano.CLI.Shelley.Run.StakeAddress (ShelleyStakeAddressCmdError, renderShelleyStakeAddressCmdError)
import Cardano.CLI.Byron.Genesis (ByronGenesisError)
import Data.Text.Lazy.Builder (toLazyText)
import  Formatting.Buildable (build)
import Codec.CBOR.Read (DeserialiseFailure)

-- We should move all the command errors here so that modules
-- can read each other's error type without cyclic dependency

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  | ShelleyProtocolEraMismatch
  -- ^ The Shelley protocol only supports the Shelley era.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ByronProtocolNotSupportedError ->
      "The attempted local state query does not support the Byron protocol."
    ShelleyProtocolEraMismatch ->
        "The Shelley protocol mode can only be used with the Shelley era, "
     <> "i.e. with --shelley-mode use --shelley-era flag"


data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquiringFailure
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | ShelleyQueryCmdByronEra
  | ShelleyQueryCmdPoolIdError (Hash StakePoolKey)
  | ShelleyQueryCmdEraMismatch !EraMismatch
  | ShelleyQueryCmdUnsupportedMode !AnyConsensusMode
  | ShelleyQueryCmdPastHorizon !Qry.PastHorizonException
  | ShelleyQueryCmdSystemStartUnavailable
  | ShelleyQueryCmdGenesisReadError !ShelleyGenesisCmdError
  | ShelleyQueryCmdLeaderShipError !LeadershipError
  | ShelleyQueryCmdTextEnvelopeReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdTextReadError !(FileError InputDecodeError)
  | ShelleyQueryCmdColdKeyReadFileError !(FileError InputDecodeError)
  | ShelleyQueryCmdOpCertCounterReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdProtocolStateDecodeFailure !(LBS.ByteString, DecoderError)
  | ShelleyQueryCmdSlotToUtcError Text
  | ShelleyQueryCmdNodeUnknownStakePool
      FilePath
      -- ^ Operational certificate of the unknown stake pool.
  | ShelleyQueryCmdPoolStateDecodeError DecoderError

  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdAcquireFailure acquireFail -> Text.pack $ show acquireFail
    ShelleyQueryCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyQueryCmdPoolIdError poolId -> "The pool id does not exist: " <> show poolId
    ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occurred." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    ShelleyQueryCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyQueryCmdPastHorizon e -> "Past horizon: " <> show e
    ShelleyQueryCmdSystemStartUnavailable -> "System start unavailable"
    ShelleyQueryCmdGenesisReadError err' -> Text.pack $ displayError err'
    ShelleyQueryCmdLeaderShipError e -> Text.pack $ displayError e
    ShelleyQueryCmdTextEnvelopeReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdSlotToUtcError e -> "Failed to convert slot to UTC time: " <> e
    ShelleyQueryCmdTextReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdColdKeyReadFileError e -> Text.pack $ displayError e
    ShelleyQueryCmdOpCertCounterReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdProtocolStateDecodeFailure (_, decErr) ->
      "Failed to decode the protocol state: " <> toStrict (toLazyText $ build decErr)
    ShelleyQueryCmdNodeUnknownStakePool nodeOpCert ->
      Text.pack $ "The stake pool associated with: " <> nodeOpCert <> " was not found. Ensure the correct KES key has been " <>
                  "specified and that the stake pool is registered. If you have submitted a stake pool registration certificate " <>
                  "in the current epoch, you must wait until the following epoch for the registration to take place."
    ShelleyQueryCmdPoolStateDecodeError decoderError ->
      "Failed to decode PoolState.  Error: " <> Text.pack (show decoderError)


data ShelleyGenesisCmdError
  = ShelleyGenesisCmdAesonDecodeError !FilePath !Text
  | ShelleyGenesisCmdGenesisFileReadError !(FileError IOException)
  | ShelleyGenesisCmdGenesisFileDecodeError !FilePath !Text
  | ShelleyGenesisCmdGenesisFileError !(FileError ())
  | ShelleyGenesisCmdFileError !(FileError ())
  | ShelleyGenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | ShelleyGenesisCmdFilesNoIndex [FilePath]
  | ShelleyGenesisCmdFilesDupIndex [FilePath]
  | ShelleyGenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | ShelleyGenesisCmdUnexpectedAddressVerificationKey !VerificationKeyFile !Text !SomeAddressVerificationKey
  | ShelleyGenesisCmdTooFewPoolsForBulkCreds !Word !Word !Word
  | ShelleyGenesisCmdAddressCmdError !ShelleyAddressCmdError
  | ShelleyGenesisCmdNodeCmdError !ShelleyNodeCmdError
  | ShelleyGenesisCmdPoolCmdError !ShelleyPoolCmdError
  | ShelleyGenesisCmdStakeAddressCmdError !ShelleyStakeAddressCmdError
  | ShelleyGenesisCmdCostModelsError !FilePath
  | ShelleyGenesisCmdByronError !ByronGenesisError
  | ShelleyGenesisStakePoolRelayFileError !FilePath !IOException
  | ShelleyGenesisStakePoolRelayJsonDecodeError !FilePath !String
  deriving Show

instance Error ShelleyGenesisCmdError where
  displayError err =
    case err of
      ShelleyGenesisCmdAesonDecodeError fp decErr ->
        "Error while decoding Shelley genesis at: " <> fp <> " Error: " <> Text.unpack decErr
      ShelleyGenesisCmdGenesisFileError fe -> displayError fe
      ShelleyGenesisCmdFileError fe -> displayError fe
      ShelleyGenesisCmdMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
        "Mismatch between the files found:\n"
          <> "Genesis key file indexes:      " <> show gfiles <> "\n"
          <> "Delegate key file indexes:     " <> show dfiles <> "\n"
          <> "Delegate VRF key file indexes: " <> show vfiles
      ShelleyGenesisCmdFilesNoIndex files ->
        "The genesis keys files are expected to have a numeric index but these do not:\n"
          <> unlines files
      ShelleyGenesisCmdFilesDupIndex files ->
        "The genesis keys files are expected to have a unique numeric index but these do not:\n"
          <> unlines files
      ShelleyGenesisCmdTextEnvReadFileError fileErr -> displayError fileErr
      ShelleyGenesisCmdUnexpectedAddressVerificationKey (VerificationKeyFile file) expect got -> mconcat
        [ "Unexpected address verification key type in file ", file
        , ", expected: ", Text.unpack expect, ", got: ", Text.unpack (renderSomeAddressVerificationKey got)
        ]
      ShelleyGenesisCmdTooFewPoolsForBulkCreds pools files perPool -> mconcat
        [ "Number of pools requested for generation (", show pools
        , ") is insufficient to fill ", show files
        , " bulk files, with ", show perPool, " pools per file."
        ]
      ShelleyGenesisCmdAddressCmdError e -> Text.unpack $ renderShelleyAddressCmdError e
      ShelleyGenesisCmdNodeCmdError e -> Text.unpack $ renderShelleyNodeCmdError e
      ShelleyGenesisCmdPoolCmdError e -> Text.unpack $ renderShelleyPoolCmdError e
      ShelleyGenesisCmdStakeAddressCmdError e -> Text.unpack $ renderShelleyStakeAddressCmdError e
      ShelleyGenesisCmdCostModelsError fp -> "Cost model is invalid: " <> fp
      ShelleyGenesisCmdGenesisFileDecodeError fp e ->
       "Error while decoding Shelley genesis at: " <> fp <>
       " Error: " <>  Text.unpack e
      ShelleyGenesisCmdGenesisFileReadError e -> displayError e
      ShelleyGenesisCmdByronError e -> show e
      ShelleyGenesisStakePoolRelayFileError fp e ->
        "Error occurred while reading the stake pool relay specification file: " <> fp <>
        " Error: " <> show e
      ShelleyGenesisStakePoolRelayJsonDecodeError fp e ->
        "Error occurred while decoding the stake pool relay specification file: " <> fp <>
        " Error: " <>  e

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeCmdVrfSigningKeyCreationError targetPath tempPath ->
      Text.pack $ "Error creating VRF signing key file. Target path: " <> targetPath
      <> " Temporary path: " <> tempPath

    ShelleyNodeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)
    ShelleyNodeCmdQuery queryErr -> renderShelleyQueryCmdError queryErr
    ShelleyNodeCmdOperationalCertificateKESPeriodInFuture -> "Error operational certificate starting kes period is in future."
    ShelleyNodeCmdKESKeyEvolutionFailed -> "Error provided kes key KES_0 evolution failed to target period."

data ShelleyNodeCmdError
  = ShelleyNodeCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyNodeCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyNodeCmdWriteFileError !(FileError ())
  | ShelleyNodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  | ShelleyNodeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  | ShelleyNodeCmdQuery !ShelleyQueryCmdError
  | ShelleyNodeCmdOperationalCertificateKESPeriodInFuture
  | ShelleyNodeCmdKESKeyEvolutionFailed
  deriving Show


data HelpersError
  = CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

renderHelpersError :: HelpersError -> Text
renderHelpersError err =
  case err of
    OutputMustNotAlreadyExist fp -> "Output file/directory must not already exist: " <> Text.pack fp
    ReadCBORFileFailure fp err' -> "CBOR read failure at: " <> Text.pack fp <> Text.pack (show err')
    CBORPrettyPrintError err' -> "Error with CBOR decoding: " <> Text.pack (show err')
    CBORDecodingError err' -> "Error with CBOR decoding: " <> Text.pack (show err')
    IOError' fp ioE -> "Error at: " <> Text.pack fp <> " Error: " <> Text.pack (show ioE)
