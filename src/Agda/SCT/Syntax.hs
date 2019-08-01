-- | Syntax of TPDB terms and declarations

module Agda.SCT.Syntax where

import Agda.Utils.Lens
import Agda.Utils.Singleton

import Control.Monad.Reader

import Data.List
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String

-- Term rewriting systems
data TRS = TRS
  { _trsRules :: [Rule]
  , _trsVars  :: [Variable]
  , _trsDefs  :: [Definition]
  } deriving (Show)

instance Semigroup TRS where
  x <> y = TRS (x ^. trsRules <> y ^. trsRules)
               (x ^. trsVars  <> y ^. trsVars )
               (x ^. trsDefs  <> y ^. trsDefs )

instance Monoid TRS where
  mempty  = TRS mempty mempty mempty
  mappend = (<>)

trsRules :: Lens' [Rule] TRS
trsRules f trs =
  f (_trsRules trs) <&>
  \x -> trs { _trsRules = x }

trsVars :: Lens' [Variable] TRS
trsVars f trs =
  f (_trsVars trs) <&>
  \x -> trs { _trsVars = x }

trsDefs :: Lens' [Definition] TRS
trsDefs f trs =
  f (_trsDefs trs) <&>
  \x -> trs { _trsDefs = x }

data Rule = Rule Term Term
  deriving (Show)

newtype VarName = VarName { unVarName :: String }
  deriving (Eq, Show, IsString)

newtype DefName = DefName { unDefName :: String }
  deriving (Eq, Show, IsString)

data Term
  = App Term Term
  | Def DefName
  | Var VarName
  | Lam VarName Type Term
  | Arr Type Type
  | Pi  VarName Type Type
  | Univ
  deriving (Eq, Show)

data Type = Type Term
  deriving (Eq, Show)

data Variable = Variable { varName :: VarName , varType :: Type }
  deriving (Show)

data Definition = Definition { defName :: DefName , defType :: Type }
  deriving (Show)

typeOfVar :: TRS -> VarName -> Maybe Type
typeOfVar trs x = fmap varType $ find ((== x) . varName) $ trs ^. trsVars

typeOfDef :: TRS -> DefName -> Maybe Type
typeOfDef trs f = fmap defType $ find ((== f) . defName) $ trs ^. trsDefs

cleanupTRS :: TRS -> TRS
cleanupTRS trs@(TRS rules vars defs) = TRS rules vars' defs'
  where
    usedNames = allNames trs rules
    vars' = filter ((`Set.member` usedNames) . unVarName . varName) vars
    defs' = filter ((`Set.member` usedNames) . unDefName . defName) defs

class AllNames a where
  allNames :: TRS -> a -> Set String

instance AllNames VarName where
  allNames _ x = singleton $ unVarName x

instance AllNames DefName where
  allNames _ f = singleton $ unDefName f

instance AllNames Term where
  allNames trs = \case
    App u v   -> allNames trs u `mappend` allNames trs v
    Def f     -> allNames trs f
    Var x     -> allNames trs x
    Lam x a v -> allNames trs x `mappend` allNames trs a `mappend` allNames trs v
    Arr a b   -> allNames trs a `mappend` allNames trs b
    Pi x a b  -> allNames trs x `mappend` allNames trs a `mappend` allNames trs b
    Univ      -> mempty

instance AllNames Type where
  allNames trs (Type a) = allNames trs a

instance AllNames Rule where
  allNames trs (Rule lhs rhs) = allNames trs lhs `mappend` allNames trs rhs

instance AllNames a => AllNames [a] where
  allNames trs xs = mconcat $ map (allNames trs) xs
