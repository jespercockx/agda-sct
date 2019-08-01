module Main where

import Prelude hiding ( null , empty )

import Agda.Compiler.Backend
import Agda.Compiler.Common

import Agda.Interaction.Options.IORefs

import Agda.Main ( runAgda )

import Agda.SCT.Syntax
import Agda.SCT.FromAgda
import Agda.SCT.ToXML

import Agda.TypeChecking.Pretty

import Agda.Utils.Either
import Agda.Utils.Except
import Agda.Utils.Functor
import Agda.Utils.Null
import Agda.Utils.Pretty ( prettyShow )

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Function
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Text.XML.Light as XML

main :: IO ()
main = runAgda [backend]

backend :: Backend
backend = Backend backend'

data SctOptions = SctOptions

backend' :: Backend' SctOptions SctOptions () () ()
backend' = Backend'
  { backendName           = "sct"
  , options               = SctOptions
  , commandLineFlags      = []
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ _ _ _ -> return $ Recompile ()
  , compileDef            = \ _ _ _ _ -> return ()
  , postModule            = \ _ _ _ _ _ -> sctPostModule
  , backendVersion        = Nothing
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return False
  }

sctPostModule :: TCM ()
sctPostModule = do
  -- make sure we don't generate any unicode
  liftIO $ writeIORef unicodeOrAscii AsciiOnly

  [agdaPrimInter] <- filter (("Agda.Primitive"==) . prettyShow . iModuleName)
    . map miInterface . Map.elems
      <$> getVisitedModules
  let primSig = iSignature agdaPrimInter
  sig <- curSig
  trs0 <- fromAgda [primSig , sig]
    & (`evalStateT` (empty :: FromAgdaState))
    & (`runReaderT` (empty :: FromAgdaEnv))
    & fst <.> runWriterT
    & runExceptT
    & fromRightM (\err -> typeError . GenericDocError =<< err)
  let trs = cleanupTRS trs0
  liftIO $ writeFile "test.xml" $ XML.ppTopElement $ sctProblem trs
  return ()
