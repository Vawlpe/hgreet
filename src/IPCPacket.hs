{-# LANGUAGE  OverloadedStrings
           ,  DeriveGeneric
           ,  LambdaCase
           ,  BlockArguments
           ,  ExtendedDefaultRules
#-}

module IPCPacket ( Request(..), Response(..), AuthMessageType(..), ErrorType(..)
                 , encodeRequestPacket, decodeResponsePacket) where

import Data.Maybe
import Data.Aeson hiding (Success, Error)
import GHC.Generics
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
    toJSON (CreateSession username)            = object ["username" .= username]
    toJSON (PostAuthMessageResponse response)  = object ["response" .= response]
    toJSON (StartSession cmd)                  = object ["cmd" .= cmd]
    toJSON (CancelSession)                     = object []

instance FromJSON Response where
    parseJSON = withObject "Response" $ \v -> do
        v .: "type" >>= withText "type" \case
            "success"       -> return Success
            "error"         -> Error <$> v .: "error_type" <*> v .: "description"
            "auth_message"  -> AuthMessage <$> v .: "auth_message_type" <*> v .: "auth_message"
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

encodeRequestPacket :: Request -> B.ByteString
encodeRequestPacket request = BL.toStrict $ packet where
    encodedRequest = encode request
    encodedLength  = encode $ fromIntegral $ BL.length encodedRequest
    packet         = encodedLength `BL.append` encodedRequest

decodeResponsePacket :: B.ByteString -> Response
decodeResponsePacket packet = case decode $ BL.fromStrict packet of
    Just response -> response
    Nothing       -> Error OtherError "Invalid response packet, Nothing"