{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module HttpsClient(fetchUrl) where

import qualified Network.Connection      as NC
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.TLS             as TLS
import qualified Network.TLS.Extra       as TLS (ciphersuite_all)
import qualified System.X509             as TLS

import Data.Default.Class (def)
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment
import Prelude

import Data.ByteString.Lazy.Internal(ByteString)

fetchUrl :: String -> IO ByteString
fetchUrl url = do
  let req = fromJust $ parseUrl url
  mgr <- mkHttpManager True
  res <- httpLbs req mgr
  return $ responseBody res

-- | Create an HTTP 'Manager' for running a 'Test'
mkHttpManager :: Bool  -- ^ validate ssl
              -> IO Manager
mkHttpManager validateSsl = do

    scs <- TLS.getSystemCertificateStore
    let tlsSettings = NC.TLSSettings (cp scs)
        mngrCfg = Http.mkManagerSettings tlsSettings Nothing

    Http.newManager mngrCfg
  where
    cp scs = (TLS.defaultParamsClient "" "") {
                TLS.clientSupported = def {
                    TLS.supportedCiphers        = TLS.ciphersuite_all
                  , TLS.supportedHashSignatures = hashSignatures
                  -- , TLS.supportedVersions [TLS10, TLS11, TLS12]
                  }
              , TLS.clientShared = def {
                    TLS.sharedCAStore         = scs
                  , TLS.sharedValidationCache = validationCache
                  }
              }

    hashSignatures =
        [ (TLS.HashSHA512, TLS.SignatureRSA)
        , (TLS.HashSHA384, TLS.SignatureRSA)
        , (TLS.HashSHA256, TLS.SignatureRSA)
        , (TLS.HashSHA224, TLS.SignatureRSA)
        , (TLS.HashSHA1,   TLS.SignatureRSA)
        , (TLS.HashSHA1,   TLS.SignatureDSS)
        , (TLS.HashSHA512, TLS.SignatureECDSA) -- "bad SignatureECDSA for ecdhparams"
        , (TLS.HashSHA384, TLS.SignatureECDSA) -- "bad SignatureECDSA for ecdhparams"
        , (TLS.HashSHA256, TLS.SignatureECDSA)
        , (TLS.HashSHA224, TLS.SignatureECDSA)
        , (TLS.HashSHA1,   TLS.SignatureECDSA)
        ]

    validationCache =
        if not validateSsl then
            TLS.ValidationCache
                          (\_ _ _ -> return TLS.ValidationCachePass)
                          (\_ _ _ -> return ())
        else
            def
