{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
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

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT, left, hoistMaybe, handleIOExceptT)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import           Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..), SigningMessageFile (SigningMessageFile), NodeOperationCertFile (NodeOperationCertFile))

import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Crypto.KES as  KES
import Cardano.CLI.Shelley.Orphans ()
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Cardano.Api as Api
import qualified Data.ByteString.Lazy as LBS hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Cardano.CLI.Shelley.Errors
import Control.Monad (fail)
{- HLINT ignore "Reduce duplication" -}


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
  -> OutputFile
  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKesSign (AnyConsensusModeParams cModeParams) network (SigningKeyFile kesSKeyFile) (SigningMessageFile signMsgFile) (NodeOperationCertFile opFile) (OutputFile outFile) = do
      fail "sad"
    -- (KesSigningKey kes_0) <- firstExceptT ShelleyNodeCmdReadFileError
    --   (newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesSKeyFile)

    -- opCert <- firstExceptT ShelleyNodeCmdReadFileError
    --         . newExceptT $ readFileTextEnvelope AsOperationalCertificate opFile

    -- SocketPath sockPath <- firstExceptT ShelleyNodeCmdEnvVarSocketErr
    --                        $ newExceptT readEnvSocketPath
    -- let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

    -- AnyCardanoEra era <-
    --   firstExceptT ShelleyNodeCmdAcquireFailure
    --     . newExceptT $ determineEra cModeParams localNodeConnInfo


    -- let cMode = consensusModeOnly cModeParams
    -- sbe <- getSbe $ cardanoEraStyle era

    -- case cMode of
    --   CardanoMode -> do
    --     chainTip <- liftIO $ getLocalChainTip localNodeConnInfo
    --     eInMode <- calcEraInMode era cMode
    --     let genesisQinMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryGenesisParameters
    --     gParams <- executeQuery era cModeParams localNodeConnInfo genesisQinMode
    --     let curKesPeriodFromNode = currentKesPeriod chainTip gParams

    --     let startingKesPeriod = fromIntegral $ getKesPeriod opCert
    --     _ <- if startingKesPeriod > curKesPeriodFromNode
    --       then left ShelleyNodeCmdOperationalCertificateKESPeriodInFuture
    --     else pure ()

    --     -- Find out the target period for the KES key so that KES_0 key can be evolved to required target
    --     let targetKesPeriod = fromIntegral (curKesPeriodFromNode - startingKesPeriod)
    --     let currentKesEvol = evolveKESUntil kes_0 0 targetKesPeriod

    --     case currentKesEvol of
    --       Nothing -> left ShelleyNodeCmdKESKeyEvolutionFailed
    --       Just currentKes -> do
    --         signMsgBs <- liftIO $ BS.readFile signMsgFile
    --         let sig :: (Keys.SignedKES StandardCrypto ByteString)
    --             sig = KES.signedKES () targetKesPeriod signMsgBs currentKes
    --             sigTextEnvJson = textEnvelopeToJSON Nothing sig

    --         handleIOExceptT (ShelleyNodeCmdWriteFileError . FileIOError outFile) 
    --           $ LBS.writeFile outFile sigTextEnvJson
    --         liftIO $ LBS.putStrLn "Message is signed and saved sucessfully."
    --   mode -> left . ShelleyNodeCmdUnsupportedMode $ AnyConsensusMode mode

  where
    currentKesPeriod :: ChainTip -> GenesisParameters -> Word64
    currentKesPeriod ChainTipAtGenesis _ = 0
    currentKesPeriod (ChainTip currSlot _ _) gParams =
      let slotsPerKesPeriod = fromIntegral $ protocolParamSlotsPerKESPeriod gParams
      in unSlotNo currSlot `div` slotsPerKesPeriod

-- Helpers function related to quries according to era

-- executeQuery
--   :: forall result era mode. CardanoEra era
--   -> ConsensusModeParams mode
--   -> LocalNodeConnectInfo mode
--   -> QueryInMode mode (Either EraMismatch result)
--   -> ExceptT ShelleyNodeCmdError IO result
-- executeQuery era cModeP localNodeConnInfo q = do
--   eraInMode <- calcEraInMode era $ consensusModeOnly cModeP
--   case eraInMode of
--     ByronEraInByronMode -> left ShelleyNodeCmdByronEra
--     _ -> liftIO execQuery >>= queryResult
--  where
--    execQuery :: IO (Either AcquiringFailure (Either EraMismatch result))
--    execQuery = queryNodeLocalState localNodeConnInfo Nothing q

-- queryResult
--   :: Either AcquiringFailure (Either EraMismatch a)
--   -> ExceptT ShelleyNodeCmdError IO a
-- queryResult eAcq =
--   case eAcq of
--     Left acqFailure -> left $ ShelleyNodeCmdAcquireFailure acqFailure
--     Right eResult ->
--       case eResult of
--         Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
--         Right result -> return result

-- calcEraInMode
--   :: CardanoEra era
--   -> ConsensusMode mode
--   -> ExceptT ShelleyNodeCmdError IO (EraInMode era mode)
-- calcEraInMode era mode=
--   hoistMaybe (ShelleyNodeCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era))
--                   $ toEraInMode era mode

-- getSbe :: Monad m => CardanoEraStyle era -> ExceptT ShelleyNodeCmdError m (Api.ShelleyBasedEra era)
-- getSbe LegacyByronEra = left ShelleyNodeCmdByronEra
-- getSbe (Api.ShelleyBasedEra sbe) = return sbe

-- | Try to evolve KES key until specific KES period is reached, given the
-- current KES period.
evolveKESUntil ::
  (KES.KESAlgorithm v, KES.ContextKES v ~ ()) =>
  KES.SignKeyKES v ->
  -- | Current KES period
  Word ->
  -- | Target KES period
  Word ->
  Maybe (KES.SignKeyKES v)
evolveKESUntil = go
  where
    go _ c t | t < c = Nothing
    go sk c t | c == t = Just sk
    go sk c t = case KES.updateKES () sk c of
      Nothing -> Nothing
      Just sk' -> go sk' (c + 1) t
