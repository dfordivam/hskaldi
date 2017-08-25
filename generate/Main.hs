{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           FFICXX.Generate.Builder
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Char8

incs = [AddCInc "HsKaldiVector.h" (Char8.unpack $(embedFile "HsKaldiVector.h"))]

srcs = [] -- [ AddCSrc "test.cpp" "//test ok??" ]


mycabal = Cabal { cabal_pkgname = "hskaldi-interface"
                , cabal_cheaderprefix = "hskaldi"
                , cabal_moduleprefix = "HsKaldiInterface"
                , cabal_additional_c_incs = incs
                , cabal_additional_c_srcs = srcs
                }

mycabalattr = 
    CabalAttr 
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = [] -- ["kaldi-src", "openfst-src/include" ,"atlas-root/include"]
    , cabalattr_extralibdirs = [] -- ["kaldi-src/lib"]
    , cabalattr_extrafiles = []
    }

vectorFloat v = (TemplateApp vectorTmpl "CFloat" "kaldi::Vector<float>", v)

-- vectorBase :: Class
-- vectorBase = Class mycabal "FooClass" [] mempty Nothing
--     [ Constructor [] Nothing
--     , Virtual void_ "Foo" [ vectorFloat "v" ] Nothing
--     ]

myVector = Class mycabal "HsKaldiVector" [] mempty Nothing
  [Constructor [] Nothing
  , NonVirtual void_ "HsKaldiVectorCopyFromPtr" [cstar (CTDouble) "ptr", int "size" ] Nothing
  , Destructor Nothing
  ]

myclasses = [myVector] -- [ a, b]

toplevelfunctions = [] -- [ getVerboseLevel ]

vectorTmpl = TmplCls mycabal "Vector" "Vector" "real"
  [
    TFunNew []
  -- /// Constructor that takes no arguments.  Initializes to empty.
  -- Vector(): VectorBase<Real>() {}
  -- , TFunNew [cint "s", int "resize_type"]
  -- /// Constructor with specific size.  Sets to all-zero by default
  -- /// if set_zero == false, memory contents are undefined.
  -- explicit Vector(const MatrixIndexT s,
  --                 MatrixResizeType resize_type = kSetZero)
  --     : VectorBase<Real>() {  Resize(s, resize_type);  }
  , TFunDelete
  ]

templates = [(vectorTmpl, HdrName "hskaldi_vector.h")]

-- inline int32 GetVerboseLevel() { return g_kaldi_verbose_level; }
getVerboseLevel = TopLevelFunction (CT CTInt NoConst) "GetVerboseLevel" [] Nothing

headerMap = [("HsKaldiVector", ([], [HdrName "HsKaldiVector.h"]))]

extraDep =  []--[ ("HsKaldi", ["HsKaldi.Vector.Template"]) ]
extralibs = [] -- ["kaldi-matrix", "kaldi-base" ]

main :: IO ()
main = do 
  simpleBuilder "HsKaldiInterface" headerMap
    (mycabal,mycabalattr,myclasses,toplevelfunctions,
       templates) extralibs extraDep

