{-
Module      : HGreet
Description : Haskell module to interface with the greetd daemon trough it's IPC protocol
Copyright   : (c) Hazel (Vawlpe), 2022
License     : GPL-3.0-or-later
Maintainer  : vawlpe@gmail.com
Stability   : experimental
Portability : Linux

This module provides ways to interface with greetd through it's IPC protocol.
You must have "greetd" installed and running for this to work, preferably "fakegreet" should also be installed for testing, so you should build greetd and fakegreet from source.
See the "hagreety" package for example usage of this module.
-}
module HGreet (Client, Packet) where
import qualified HGreet.Client as Client
import qualified HGreet.Packet as Packet 
