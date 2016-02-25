module Tuura.Formula (Formula, generateController, generateCPOG,
                      freeFormulae) where

import Foreign.C.String

data Formula

foreign import ccall unsafe "get_formulae"
    getFormulae :: CString -> Int -> IO Int

foreign import ccall unsafe "free_formulae"
    freeFormulae :: IO ()

-- To use once Formula entity will be defined
--generateFormulae :: FilePath -> IO [Formula]
--generateFormulae espressoPath = undefined

generateController :: FilePath-> IO Int
generateController abcPath = do
    abc <- newCString abcPath
    getFormulae abc 1

generateCPOG :: FilePath -> IO Int
generateCPOG abcPath = do
    abc <- newCString abcPath
    getFormulae abc 0
