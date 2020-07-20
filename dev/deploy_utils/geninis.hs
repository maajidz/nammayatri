{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, TypeApplications #-}

module Main where

import Universum

import Data.Binary (Binary, encode)
import qualified Data.Vector as V (fromList)
import System.Environment (getArgs)

import qualified Data.Aeson as A
import Text.Hex (encodeHex, strictByteString)
import Crypto.Saltine.Core.Box (newKeypair)
import Crypto.Saltine.Core.SecretBox (newKey)

import Passetto.Crypto (passwordFromText, encryptKey, encryptMasterKey, sodiumInit)
import Passetto.KeysContext (mkKeysContext)
import Passetto.JsonEncryption (encryptPayload)
import Beckn.External.Encryption (DbHash(..), DbHashable(..))


main :: IO ()
main = getArgs >>= doit
  where
    doit [ns, ps, pid, pmobile]
      | Just n <- readMaybe ns = main_ n (fromString ps) (fromString pid) (fromString pmobile)
    doit _ = putStrLn @Text "Usage: geninis num_of_keys password person_id person_mobile_number"

main_ :: Int -> Text -> Text -> Text -> IO ()
main_ n p pid pmo = do
  sodiumInit
  m <- newKey
  em <- encryptMasterKey (passwordFromText p) m
  ks <- sequence $ replicate n newKeypair
  eks <- mapM (encryptKey m) ks
  writeFile "passetto_init_data.sql" $ unlines $
                ("INSERT INTO \"Passetto\".\"Master\" (key) values (decode('" <> b2hexs em <> "', 'hex'));") :
      map (\k -> "INSERT INTO \"Passetto\".\"Keys\" (encryptedKeyPair) values(decode('" <> b2hexs k <> "', 'hex'));") eks
  ek <- mkKeysContext (V.fromList ks) >>= runReaderT (encryptPayload $ "S" <> renderJSon (A.String pmo))
  let genAddEnc table = writeFile (toString table <> "_add_encrypted_phone.sql") $ "UPDATE atlas_" <> table <> ".person SET mobile_number_encrypted = '" <> ek <>
                               "', mobile_number_hash = decode('" <> encodeHex (unDbHash $ evalDbHash pmo) <> "', 'hex') where id = '" <> pid <> "';"
  genAddEnc "app"
  genAddEnc "transporter"
  where
    b2hexs :: Binary a => a -> Text
    b2hexs = encodeHex . strictByteString . encode
    renderJSon = decodeUtf8 . A.encode
