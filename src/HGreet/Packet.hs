{-# LANGUAGE  OverloadedStrings
           ,  DeriveGeneric
           ,  LambdaCase
           ,  BlockArguments
           ,  ExtendedDefaultRules
#-}

module HGreet.Packet ( Request(..), Response(..), AuthMessageType(..), ErrorType(..)
                 , encodeRequest, decodeResponse, encodeLen, decodeLen) where

import Data.Maybe
import Data.Aeson hiding (Success, Error)
import GHC.Generics
import System.Endian
import Sound.OSC.Coding.Byte (encode_u32, encode_u32_le, decode_u32, decode_u32_le)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data Request
    = CreateSession            { username :: String }
    | PostAuthMessageResponse  { respone  :: Maybe String }
    | StartSession             { cmd      :: [String] }
    | CancelSession
    deriving (Generic, Show, Eq)

data Response
    = Success
    | Error        {error_type         :: ErrorType,        description   :: String}
    | AuthMessage  {auth_message_type  :: AuthMessageType,  auth_message  :: String}
    deriving (Generic, Show, Eq)

data AuthMessageType
    = Visible
    | Secret
    | Info
    | ErrorType
    deriving (Show, Eq)

data ErrorType
    = AuthError
    | OtherError
    deriving (Show, Eq)


instance ToJSON Request where
    toJSON (CreateSession username)            = object ["type" .= "create_session",              "username" .= username]
    toJSON (PostAuthMessageResponse response)  = object ["type" .= "post_auth_message_response",  "response" .= response]
    toJSON (StartSession cmd)                  = object ["type" .= "start_session", "cmd" .= cmd]
    toJSON (CancelSession)                     = object ["type" .= "cancel_session"]

instance FromJSON Response where
    parseJSON = withObject "Response" $ \v -> do
        v .: "type" >>= withText "type" \case
            "success"       -> return Success
            "error"         -> Error        <$> v .: "error_type"         <*> v .: "description"
            "auth_message"  -> AuthMessage  <$> v .: "auth_message_type"  <*> v .: "auth_message"
            _               -> fail "Invalid response type"  

instance FromJSON ErrorType where
    parseJSON = withText "ErrorType" \case
        "auth_error"  -> return AuthError
        "error"       -> return OtherError
        _             -> fail "Invalid error type"

instance FromJSON AuthMessageType where
    parseJSON = withText "AuthMessageType" \case
        "visible" -> return Visible
        "secret"  -> return Secret
        "info"    -> return Info
        "error"   -> return ErrorType
        _         -> fail "Invalid auth message type"

encodeRequest :: Request -> B.ByteString
encodeRequest request = BL.toStrict $ packet where
    encodedRequest = encode     $ request
    encodedLength  = encodeLen  $ fromIntegral $ BL.length encodedRequest
    packet         = encodedLength `BL.append` encodedRequest

decodeResponse :: B.ByteString -> Response
decodeResponse = fromJust . decode . BL.fromStrict

encodeLen :: Int -> BL.ByteString
encodeLen len = case getSystemEndianness of
    BigEndian     -> encode_u32     $ len
    LittleEndian  -> encode_u32_le  $ len

decodeLen :: BL.ByteString -> Int
decodeLen len = case getSystemEndianness of
    BigEndian     -> decode_u32     $ len
    LittleEndian  -> decode_u32_le  $ len
