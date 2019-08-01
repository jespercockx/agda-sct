module Agda.SCT.FromAgda where

import Prelude hiding ( null , empty )

import Agda.Compiler.Common

import Agda.Syntax.Common
import Agda.Syntax.Concrete.Name ( nextStr )
import Agda.Syntax.Internal

import Agda.TypeChecking.Level ( unLevel )
import Agda.TypeChecking.Monad.Base hiding ( freshName )
import Agda.TypeChecking.Monad.Builtin
import Agda.TypeChecking.Monad.Context
import Agda.TypeChecking.Monad.Debug
import Agda.TypeChecking.Monad.Signature
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Rewriting.NonLinPattern
import Agda.TypeChecking.Substitute

import qualified Agda.SCT.Syntax as SCT

import Agda.Utils.Except
import Agda.Utils.Lens
import Agda.Utils.List
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Null

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Char ( isAscii )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data FromAgdaState = FromAgdaState
  { _stDefNames   :: Map QName SCT.DefName
  , _stTakenNames :: Set String
  , _stVarTypes   :: Map String SCT.Type -- we cannot reuse the same variable name with a different type
  }

instance Null FromAgdaState where
  empty = FromAgdaState empty empty empty
  null (FromAgdaState x y z) = null x && null y && null z

defNames :: Lens' (Map QName SCT.DefName) FromAgdaState
defNames f st =
  f (_stDefNames st) <&>
  \x -> st { _stDefNames = x }

takenNames :: Lens' (Set String) FromAgdaState
takenNames f st =
  f (_stTakenNames st) <&>
  \x -> st { _stTakenNames = x }

varTypes :: Lens' (Map String SCT.Type) FromAgdaState
varTypes f st =
  f (_stVarTypes st) <&>
  \x -> st { _stVarTypes = x }

data FromAgdaEnv = FromAgdaEnv
  { _envVarNames :: [SCT.VarName]
  , _envRewriteRules :: Set QName
  }

instance Null FromAgdaEnv where
  empty = FromAgdaEnv empty empty
  null  = null . _envVarNames

varNames :: Lens' [SCT.VarName] FromAgdaEnv
varNames f env =
  f (_envVarNames env) <&>
  \x -> env { _envVarNames = x }

envRewriteRules :: Lens' (Set QName) FromAgdaEnv
envRewriteRules f env =
  f (_envRewriteRules env) <&>
  \x -> env { _envRewriteRules = x }

type FromAgdaOut = [SCT.Variable]

type FromAgdaM m =
  ( MonadState FromAgdaState m
  , MonadReader FromAgdaEnv m
  , MonadWriter FromAgdaOut m
  , MonadError (TCM Doc) m
  , MonadReduce m
  , MonadAddContext m
  , HasBuiltins m
  , HasConstInfo m
  , MonadDebug m
  )

freshName :: (String -> Bool) -> String -> String
freshName goodName s0 =
  let s1 = filter isAscii s0
      s2 | null s1 || isUnderscore s1 = "x"
         | otherwise                  = s1
  in head $ filter goodName $ iterate nextStr s2

freshDefName :: FromAgdaM m => QName -> m SCT.DefName
freshDefName q = do
  takenDefs <- use takenNames
  takenVars <- use varTypes
  let goodName x = not $ x `Set.member` takenDefs || x `Map.member` takenVars
      s' = freshName goodName $ show $ qnameName q
  takenNames %= Set.insert s'
  return $ SCT.DefName s'

freshVar :: FromAgdaM m => SCT.Type -> String -> m SCT.VarName
freshVar a s = do
  reportSDoc "sct" 10 $ "Generating fresh name for variable" <+> text s <+> ":" <+> text (show a)
  takenDefs <- use takenNames
  takenVars <- use varTypes
  localVars <- view varNames
  let goodName x
        | x `Set.member` takenDefs = False
        | otherwise = case Map.lookup x takenVars of
            Just a' -> a == a' && not (SCT.VarName x `elem` localVars)
            Nothing -> True
      s' = freshName goodName s
  unless (s' `Map.member` takenVars) $ do
    varTypes %= Map.insert s' a
    tellVariable (SCT.VarName s') a
  return $ SCT.VarName s'

tellVariable :: FromAgdaM m => SCT.VarName -> SCT.Type -> m ()
tellVariable x a = tell [SCT.Variable x a]

bindVar :: FromAgdaM m => Dom Type -> String -> (SCT.VarName -> m a) -> m a
bindVar a s cont = do
  a' <- fromAgda a
  x <- freshVar a' s
  locally varNames (x:) $ addContext (SCT.unVarName x , a) $ cont x

bindTel :: FromAgdaM m => Telescope -> m a -> m a
bindTel EmptyTel          cont = cont
bindTel (ExtendTel a tel) cont =
  bindVar a (absName tel) $ \_ -> bindTel (unAbs tel) cont

class FromAgda a b where
  fromAgda :: FromAgdaM m => a -> m b

instance FromAgda [Signature] SCT.TRS where
  fromAgda []     = return mempty
  fromAgda (x:xs) = do
    reportSDoc "sct" 5 $ "translating interface" <+> prettyList_ (map (text . show . fst) $ Map.toList $ _sigSections x)
    mappend <$> fromAgda x <*> fromAgda xs

instance FromAgda Signature SCT.TRS where
  fromAgda sig = do

    let allRules = Set.fromList $ map rewName $ concat $ HashMap.elems $ sig ^. sigRewriteRules
    ((defs,rules),vars) <- runWriterT $ locally envRewriteRules (Set.union allRules) $ do
      defs <- fromAgda $ sig ^. sigDefinitions
      rules <- fromAgda $ sig ^. sigRewriteRules
      return (defs,rules)
    return $ SCT.TRS rules vars defs

instance FromAgda Definition SCT.Definition where
  fromAgda def = do
    reportSDoc "sct" 5 $ "translating declaration of" <+> prettyTCM (defName def) <+> ":" <+> prettyTCM (defType def)
    f <- freshDefName $ defName def
    defNames %= Map.insert (defName def) f
    a <- fromAgda $ defType def
    return $ SCT.Definition f a

instance FromAgda RewriteRule SCT.Rule where
  fromAgda rr = bindTel (rewContext rr) $ do
    reportSDoc "sct" 5 $ "translating rewrite rule" <+> prettyTCM rr
    lhs0 :: Term <- nlPatToTerm (PDef (rewHead rr) $ rewPats rr)
    lhs <- fromAgda (rewType rr , lhs0)
    rhs <- fromAgda (rewType rr , rewRHS rr)
    return $ SCT.Rule lhs rhs

instance FromAgda Definitions [SCT.Definition]  where
  fromAgda defs = do
    defs <- map snd <$> filterM shouldTranslate (sortDefs $ defs)
    forM defs fromAgda

    where
      isRewriteRel q = (Just (Def q []) ==) <$> getBuiltin' builtinRewrite
      isRewriteRule q = Set.member q <$> view envRewriteRules
      shouldTranslate (q,def)
        | GeneralizableVar{} <- theDef def = return False
        | otherwise                        = not <$> orM [ isRewriteRel q , isRewriteRule q ]


instance FromAgda RewriteRules [SCT.Rule]  where
  fromAgda rules = forM rules fromAgda

instance FromAgda RewriteRuleMap [SCT.Rule]  where
  fromAgda rules = concat <$> forM (HashMap.elems rules) fromAgda

instance FromAgda (Type , Term) SCT.Term where
  fromAgda (a,v) = case v of
    Var i es -> do
      x <- fromMaybeM (throwError "variable not in scope") $
        view varNames <&> (!!! i)
      b <- typeOfBV i
      es' <- fromAgda (b , Var i , es)
      return $ es' (SCT.Var x)
    Lam ai v -> undefined
    Lit l -> throwError "literals not supported"
    Def f es -> do
      f' <- fromMaybeM (throwError $ "name not declared: " <+> text (show f)) $
        use defNames <&> Map.lookup f
      b <- defType <$> getConstInfo f
      es' <- fromAgda (b , Def f , es)
      return $ es' (SCT.Def f')
    Con c ci es -> throwError "constructors not supported"
    Pi a (NoAbs x b) -> do
      a' <- fromAgda a
      b' <- fromAgda b
      return $ SCT.Arr a' b'
    Pi a (Abs x b) -> do
      a' <- fromAgda a
      bindVar a x $ \x -> do
        b' <- fromAgda b
        return $ SCT.Pi x a' b'
    Sort _ -> return SCT.Univ
    Level _ -> throwError "unexpected level"
    MetaV _ _ -> throwError "unexpected meta"
    DontCare _ -> throwError "irrelevance not supported"
    Dummy _ _ -> throwError "unexpected dummy"

instance FromAgda Type SCT.Type where
  fromAgda (El s a) = SCT.Type <$> fromAgda (sort s , a)

instance FromAgda a b => FromAgda (Arg a) b where
  fromAgda = fromAgda . unArg

instance FromAgda a b => FromAgda (Dom a) b where
  fromAgda = fromAgda . unDom

instance FromAgda (Type , Elims -> Term , Elims) (SCT.Term -> SCT.Term) where
  fromAgda (a,hd,es) = case es of
    [] -> return id
    e@(Apply u) : es -> do
      ~(Pi b c) <- reduce $ unEl a
      u' <- fromAgda (a , unArg u)
      es' <- fromAgda (absApp c (unArg u) , hd . (e:) , es)
      return $ \hd -> es' (SCT.App hd u')
    e@Proj{} : es -> throwError "projections not supported"
    e@IApply{} : es -> throwError "cubical not supported"
