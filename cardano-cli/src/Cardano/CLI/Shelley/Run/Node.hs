{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError(ShelleyNodeCmdReadFileError)
  , renderShelleyNodeCmdError
  , runNodeCmd
  , runNodeIssueOpCert
  , runNodeKeyGenCold
  , runNodeKeyGenKES
  , runNodeKeyGenVRF
  , readColdVerificationKeyOrFile
  ) where

import           Cardano.Prelude hiding ((<.>))
import           Prelude (id)

import qualified Data.ByteString.Char8 as BS
import           Data.String (fromString)
import qualified Data.Text as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT, left, hoistMaybe)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import           Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..), SigningMessageFile, NodeOperationCertFile (NodeOperationCertFile), CurrentKesPeriod (CurrentKesPeriod))

import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Crypto.KES as  KES
import qualified Data.Aeson as Aeson
import Cardano.CLI.Shelley.Orphans ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Cardano.Api as Api
{- HLINT ignore "Reduce duplication" -}

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


data ShelleyNodeCmdError
  = ShelleyNodeCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyNodeCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyNodeCmdWriteFileError !(FileError ())
  | ShelleyNodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  | ShelleyNodeCmdEnvVarSocketErr !EnvSocketError
  | ShelleyNodeCmdUnsupportedMode !AnyConsensusMode
  | ShelleyNodeCmdByronEra
  | ShelleyNodeCmdAcquireFailure !AcquiringFailure
  | ShelleyNodeCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyNodeCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | ShelleyNodeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  deriving Show

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
    ShelleyNodeCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyNodeCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode




runNodeCmd :: NodeCmd -> ExceptT ShelleyNodeCmdError IO ()
runNodeCmd (NodeKeyGenCold vk sk ctr) = runNodeKeyGenCold vk sk ctr
runNodeCmd (NodeKeyGenKES  vk sk)     = runNodeKeyGenKES  vk sk
runNodeCmd (NodeKeyGenVRF  vk sk)     = runNodeKeyGenVRF  vk sk
runNodeCmd (NodeKeyHashVRF vk mOutFp) = runNodeKeyHashVRF vk mOutFp
runNodeCmd (NodeNewCounter vk ctr out) = runNodeNewCounter vk ctr out
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out
runNodeCmd (NodeKesSign cmp nw sk sm op out) = runNodeKesSign cmp nw sk sm op out
--
-- Node command implementations
--

runNodeKeyGenCold :: VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenCold (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath)
                  (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter initialCounter vkey
  where
    skeyDesc, vkeyDesc, ocertCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Pool Operator Signing Key"
    vkeyDesc = "Stake Pool Operator Verification Key"
    ocertCtrDesc = "Next certificate issue number: "
                <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsKesKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "KES Signing Key"
    vkeyDesc = "KES Verification Key"

runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelopeWithOwnerPermissions skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"

runNodeKeyHashVRF :: VerificationKeyOrFile VrfKey
                  -> Maybe OutputFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyHashVRF verKeyOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyNodeCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsVrfKey verKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runNodeNewCounter :: ColdVerificationKeyOrFile
                  -> Word
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeNewCounter coldVerKeyOrFile counter
                  (OpCertCounterFile ocertCtrPath) = do

    vkey <- firstExceptT ShelleyNodeCmdReadFileError . newExceptT $
      readColdVerificationKeyOrFile coldVerKeyOrFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter (fromIntegral counter) vkey

    firstExceptT ShelleyNodeCmdWriteFileError . newExceptT $
      writeFileTextEnvelope ocertCtrPath Nothing ocertIssueCounter


runNodeIssueOpCert :: VerificationKeyOrFile KesKey
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> OutputFile
                   -> ExceptT ShelleyNodeCmdError IO ()
runNodeIssueOpCert kesVerKeyOrFile
                   (SigningKeyFile stakePoolSKeyFile)
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do

    ocertIssueCounter <- firstExceptT ShelleyNodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT ShelleyNodeCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsKesKey kesVerKeyOrFile

    signKey <- firstExceptT ShelleyNodeCmdReadKeyFileError
      . newExceptT
      $ readKeyFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          stakePoolSKeyFile

    (ocert, nextOcertCtr) <-
      firstExceptT ShelleyNodeCmdOperationalCertificateIssueError
        . hoistEither
        $ issueOperationalCertificate
            verKeyKes
            signKey
            kesPeriod
            ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope
        ocertCtrPath
        (Just $ ocertCtrDesc $ getCounter nextOcertCtr)
        nextOcertCtr

    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope certFile Nothing ocert
  where
    getCounter :: OperationalCertificateIssueCounter -> Word64
    getCounter (OperationalCertificateIssueCounter n _) = n

    ocertCtrDesc :: Word64 -> TextEnvelopeDescr
    ocertCtrDesc n = "Next certificate issue number: " <> fromString (show n)

    textEnvPossibleBlockIssuers
      :: [FromSomeType HasTextEnvelope
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    textEnvPossibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey)        Left
      , FromSomeType (AsSigningKey AsGenesisDelegateKey) (Left . castSigningKey)
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) Right
      ]

    bech32PossibleBlockIssuers
      :: [FromSomeType SerialiseAsBech32
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    bech32PossibleBlockIssuers =
      [FromSomeType (AsSigningKey AsStakePoolKey) Left]

-- | Read a cold verification key or file.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readColdVerificationKeyOrFile
  :: ColdVerificationKeyOrFile
  -> IO (Either (FileError TextEnvelopeError) (VerificationKey StakePoolKey))
readColdVerificationKeyOrFile coldVerKeyOrFile =
  case coldVerKeyOrFile of
    ColdStakePoolVerificationKey vk -> pure (Right vk)
    ColdGenesisDelegateVerificationKey vk ->
      pure $ Right (castVerificationKey vk)
    ColdVerificationKeyFile (VerificationKeyFile fp) ->
      readFileTextEnvelopeAnyOf
        [ FromSomeType (AsVerificationKey AsStakePoolKey) id
        , FromSomeType (AsVerificationKey AsGenesisDelegateKey) castVerificationKey
        ]
        fp

runNodeKesSign 
  :: AnyConsensusModeParams
  -> NetworkId
  -> SigningKeyFile
  -> SigningMessageFile
  -> NodeOperationCertFile
  -> Maybe OutputFile
  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKesSign (AnyConsensusModeParams cModeParams) network (SigningKeyFile kesSKeyFile) _ (NodeOperationCertFile opFile) _ = do
    (KesSigningKey kesSKey) <- firstExceptT ShelleyNodeCmdReadFileError
      (newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesSKeyFile)

    opCert <- firstExceptT ShelleyNodeCmdReadFileError
            . newExceptT $ readFileTextEnvelope AsOperationalCertificate opFile

    SocketPath sockPath <- firstExceptT ShelleyNodeCmdEnvVarSocketErr
                           $ newExceptT readEnvSocketPath
    let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

    anyE@(AnyCardanoEra era) <-
      firstExceptT ShelleyNodeCmdAcquireFailure
        . newExceptT $ determineEra cModeParams localNodeConnInfo


    let cMode = consensusModeOnly cModeParams
    sbe <- getSbe $ cardanoEraStyle era

    case cMode of
      CardanoMode -> do
        chainTip <- liftIO $ getLocalChainTip localNodeConnInfo
        eInMode <- calcEraInMode era cMode

        let genesisQinMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryGenesisParameters

        gParams <- executeQuery era cModeParams localNodeConnInfo genesisQinMode

        let curKesPeriod = currentKesPeriod chainTip gParams
        
        liftIO $ print curKesPeriod

      mode -> left . ShelleyNodeCmdUnsupportedMode $ AnyConsensusMode mode
    


    
    -- let sig :: (Keys.SignedKES StandardCrypto ByteString)
    --     sig = KES.signedKES () 0 "ok" kesSKey

    -- -- print sig
    -- print $ Aeson.encode $ serialiseToTextEnvelope Nothing sig
    

    -- -- case ssk of 
    -- --   AKesSigningKey (KesSigningKey sk) -> print sk
    -- --   _ -> throwError "Error Signing key must be KES signing key."

    -- -- withSomeSigningKey ssk $ \sk -> do
    -- --   let sig = makeShelleySignature "" sk
    -- --   firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
    -- --     writeFileTextEnvelope outFile Nothing sk

  where
    currentKesPeriod :: ChainTip -> GenesisParameters -> CurrentKesPeriod
    currentKesPeriod ChainTipAtGenesis _ = CurrentKesPeriod 0
    currentKesPeriod (ChainTip currSlot _ _) gParams =
      let slotsPerKesPeriod = fromIntegral $ protocolParamSlotsPerKESPeriod gParams
      in CurrentKesPeriod $ unSlotNo currSlot `div` slotsPerKesPeriod

-- Helpers function related to quries according to era

executeQuery
  :: forall result era mode. CardanoEra era
  -> ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> ExceptT ShelleyNodeCmdError IO result
executeQuery era cModeP localNodeConnInfo q = do
  eraInMode <- calcEraInMode era $ consensusModeOnly cModeP
  case eraInMode of
    ByronEraInByronMode -> left ShelleyNodeCmdByronEra
    _ -> liftIO execQuery >>= queryResult
 where
   execQuery :: IO (Either AcquiringFailure (Either EraMismatch result))
   execQuery = queryNodeLocalState localNodeConnInfo Nothing q

queryResult
  :: Either AcquiringFailure (Either EraMismatch a)
  -> ExceptT ShelleyNodeCmdError IO a
queryResult eAcq =
  case eAcq of
    Left acqFailure -> left $ ShelleyNodeCmdAcquireFailure acqFailure
    Right eResult ->
      case eResult of
        Left err -> left . ShelleyNodeCmdLocalStateQueryError $ EraMismatchError err
        Right result -> return result

calcEraInMode
  :: CardanoEra era
  -> ConsensusMode mode
  -> ExceptT ShelleyNodeCmdError IO (EraInMode era mode)
calcEraInMode era mode=
  hoistMaybe (ShelleyNodeCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era))
                  $ toEraInMode era mode

getSbe :: Monad m => CardanoEraStyle era -> ExceptT ShelleyNodeCmdError m (Api.ShelleyBasedEra era)
getSbe LegacyByronEra = left ShelleyNodeCmdByronEra
getSbe (Api.ShelleyBasedEra sbe) = return sbe
