module Main where

import Codec.Wav ( exportFile, importFile )
import Data.Audio ( Audio(Audio) )
import Data.Array.Unboxed ( listArray, elems )
import Data.Int ( Int32 )
import Data.Maybe ( fromMaybe )
import System.IO (FilePath)
import Foreign
import Foreign.C.Types

foreign import ccall unsafe "c_init_kaldi"
               c_init_kaldi :: IO (Ptr DoAsrArgs)

foreign import ccall unsafe "c_main_wrap"
               c_main_wrap :: Int -> Ptr Int16 -> IO ()

foreign import ccall unsafe "c_doAsr"
               c_doAsr :: Ptr DoAsrArgs -> Int -> Ptr Int16 -> IO ()

data DoAsrArgs = DoAsrArgs

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  doAsrArgsPtr <- c_init_kaldi
  inMain doAsrArgsPtr "try.wav"

-- inMain :: FilePath -> IO ()
inMain doAsrArgsPtr path = do
  maybeAudio <- importFile path
  case maybeAudio :: Either String (Audio Int16) of
    Left s -> putStrLn $ "wav decoding error: " ++ s
    Right (Audio rate channels samples) -> do
      putStrLn $ "rate = " ++ show rate
      putStrLn $ "channels: " ++ show channels
      print (take 100 (elems samples))
      withArrayLen (elems samples :: [Int16]) (c_doAsr doAsrArgsPtr)
      -- unsafeUseAsCStringLen

-- withArrayLen :: Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b 
