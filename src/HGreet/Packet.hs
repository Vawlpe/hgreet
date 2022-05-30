{-# LANGUAGE  OverloadedStrings
           ,  DeriveGeneric
           ,  LambdaCase
           ,  BlockArguments
           ,  ExtendedDefaultRules
#-}
{-
Module        : HGreet.Packet 
Description   : Packet data types and functions to encode these for usage with HGreet.Client
Copyright     : (c) Hazel (Vawlpe), 2022
License       : GPL-3.0-or-later
Maintainer    : vawlpe@gmail.com
Stability     : experimental
Portability   : Linux

This module provides the data types listed in greetd-ipc(7) and the functions to encode them for usage with HGreet.Client.
See the `hagreety` package for example usage of this module.
-}
module HGreet.Packet ( Request(..), Response(..), AuthMessageType(..), ErrorType(..)
                 , encodeRequest, decodeResponse, encodeLen, decodeLen) where

import Data.Maybe
import Data.Aeson hiding (Success, Error)
import GHC.Generics
import System.Endian
import Sound.OSC.Coding.Byte (encode_u32, encode_u32_le, decode_u32, decode_u32_le)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Sound.OSC as HGreet

-- * Packet data types.
-- ** Request packets (greetd-ipc(7) line 27)
data Request
    = CreateSession            { username :: String }       -- ^ Creates a session and initiates a login atempted for the given user. The session is ready to be started if a success is returned.
    | PostAuthMessageResponse  { respone  :: Maybe String } -- ^ Answers an authentication message. If the message was informative (info, error), then a response does not need to be set in this message. Tht session is ready to be started if a success is returned.
    | StartSession             { cmd      :: [String] }     -- ^ Requests for the session to be started using the provided command line. The session will start after the greeter process terminates.
    | CancelSession                                         -- ^ Cancel the session that is currently under configuration.
    deriving (Generic, Show, Eq)

-- ** Response packets (greetd-ipc(7) line 55)
data Response
    = Success                                                                        -- ^ Indicates that the request succeeded.
    | Error        {error_type         :: ErrorType,        description   :: String} -- ^ Indicates that the request failed.
    | AuthMessage  {auth_message_type  :: AuthMessageType,  auth_message  :: String} -- ^ Indicates that an authentication message needs to be answered to continue trough the authenticaltion flow. There are no limits on the number and type of messages that may be required for authentication to succeed, and a greeter should not make any assumptions about the messages. Must be answerd with either PostAuthMessageResponse or CancelSession.
    deriving (Generic, Show, Eq)

-- ** Authentication message packet type enums (greetd-ipc(7) line 76)
data AuthMessageType
    = Visible   -- ^ Indicates that the input from the user should be visible when they answer this question.
    | Secret    -- ^ Indicates that input from the user should be considered secret when they answer this question.
    | Info      -- ^ Indicates that this message is informative, not a question.
    | ErrorType -- ^ Indicates that this message is an error, not a question.
    deriving (Show, Eq)

-- ** Error message packet type enums (greetd-ipc(7) line 96)
data ErrorType
    = AuthError  -- ^ Indicates that authentication failed. THis is not a fatal error, and is likely caused by incorrect credentials. Handle as appropriate.
    | OtherError -- ^ A general error. See the error description for more information.
    deriving (Show, Eq)


-- * JSON encoding and decoding instances for the above data types.
-- ** toJSON encoding instances for Request packets.
instance ToJSON Request where
    toJSON (CreateSession username)            = object ["type" .= "create_session",              "username" .= username]
    toJSON (PostAuthMessageResponse response)  = object ["type" .= "post_auth_message_response",  "response" .= response]
    toJSON (StartSession cmd)                  = object ["type" .= "start_session", "cmd" .= cmd]
    toJSON CancelSession                       = object ["type" .= "cancel_session"]

-- ** parseJSON decoding instances for Response packets.
instance FromJSON Response where
    parseJSON = withObject "Response" $ \v -> do
        v .: "type" >>= withText "type" \case
            "success"       -> return Success
            "error"         -> Error        <$> v .: "error_type"         <*> v .: "description"
            "auth_message"  -> AuthMessage  <$> v .: "auth_message_type"  <*> v .: "auth_message"
            _               -> fail "Invalid response type"  

-- ** parseJSON decoding instances for AuthMessageType packets.
instance FromJSON AuthMessageType where
    parseJSON = withText "AuthMessageType" \case
        "visible" -> return Visible
        "secret"  -> return Secret
        "info"    -> return Info
        "error"   -> return ErrorType
        _         -> fail "Invalid auth message type"

-- ** parseJSON decoding instances for ErrorType packets.
instance FromJSON ErrorType where
    parseJSON = withText "ErrorType" \case
        "auth_error"  -> return AuthError
        "error"       -> return OtherError
        _             -> fail "Invalid error type"


-- * Encoding/Decoding functions for packets.
-- | Encode a Request packet as a UTF-8 JSON ByteString to be sent to the greetd socket.
encodeRequest :: Request      -- ^ Raw Request packet to encode.
              -> B.ByteString -- ^ Encoded Request packet.
encodeRequest request = BL.toStrict packet where
    encodedRequest = encode request
    encodedLength  = encodeLen  $ fromIntegral $ BL.length encodedRequest
    packet         = encodedLength `BL.append` encodedRequest

-- | Decode a Response packet from a ByteString received from the greetd socket.
decodeResponse :: B.ByteString -- ^ Encoded Response packet.
               -> Response     -- ^ Decoded raw Response packet.
decodeResponse = fromJust . decode . BL.fromStrict

-- | Encode a length as a 32-bit integer in native byte order encapsulated in a Lazy ByteString.
encodeLen :: Int -> BL.ByteString
encodeLen = case getSystemEndianness of
    BigEndian     -> encode_u32
    LittleEndian  -> encode_u32_le

-- | Decode a length as a 32-bit integer in native byte order from a Lazy ByteString.
decodeLen :: BL.ByteString -> Int
decodeLen = case getSystemEndianness of
    BigEndian     -> decode_u32
    LittleEndian  -> decode_u32_le
