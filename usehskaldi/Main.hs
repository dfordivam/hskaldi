{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Foreign.C.Types
import Foreign.Ptr

import HsKaldi
import HsKaldi.Vector.Template
import qualified HsKaldi.Vector.TH as TH

$(TH.genVectorInstanceFor ''CFloat "float")

main = do
  putStrLn "HsKaldiStart"
  verbosity <- getVerboseLevel
  putStrLn $ "Value=" ++ (show verbosity)

  v :: Vector CFloat <- newVector
  deleteVector v
  putStrLn "Done Delete Vector"
  
