{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Agda.SCT.ToXML where

import Agda.Utils.Lens

import Agda.SCT.Syntax

import Data.String


import qualified Text.XML.Light as XML

sctProblem :: TRS -> XML.Element
sctProblem trs = XML.blank_element
  { XML.elName    = "problem"
  , XML.elAttribs = [ XML.Attr "type" "termination" ]
  , XML.elContent = map XML.Elem $
    [ toXML trs
    , XML.node "strategy" $ XML.Text "FULL"
    ]
  }

class ToXML a where
  toXML :: a -> XML.Element

instance IsString XML.QName where
  fromString s = XML.QName s Nothing Nothing

instance IsString XML.CData where
  fromString s = XML.CData
    { XML.cdVerbatim = XML.CDataText
    , XML.cdData     = s
    , XML.cdLine     = Nothing
    }

instance IsString XML.Content where
  fromString = XML.Text . fromString

instance ToXML TRS where
  toXML trs = XML.node "trs"
    [ toXML $ trs ^. trsRules
    , XML.node "higherOrderSignature"
      [ toXML $ trs ^. trsVars
      , toXML $ trs ^. trsDefs
      ]
    ]

instance ToXML [Rule] where
  toXML rules = XML.node "rules" $ map toXML rules

instance ToXML [Variable] where
  toXML vars = XML.node "variableTypeInfo" $ map toXML vars

instance ToXML [Definition] where
  toXML defs = XML.node "functionSymbolTypeInfo" $ map toXML defs

instance ToXML Rule where
  toXML (Rule lhs rhs) = XML.node "rule"
    [ XML.node "lhs" [ toXML lhs ]
    , XML.node "rhs" [ toXML rhs ]
    ]

instance ToXML VarName where
  toXML (VarName x) = XML.node "var" (fromString x :: XML.Content)

instance ToXML DefName where
  toXML (DefName x) = XML.node "name" (fromString x :: XML.Content)

instance ToXML Term where
  toXML = \case
    App u v   -> XML.node "application" [ toXML u , toXML v ]
    Def f     -> XML.node "funapp" [ toXML f ]
    Var x     -> toXML x
    Lam x t v -> XML.node "lambda" [ toXML x , toXML t , toXML v ]
    Arr a b   -> XML.node "arrow" [ toXML a , toXML b ]
    Pi x a b  -> XML.node "arrow" [ toXML x , toXML a , toXML b ]
    Univ      -> XML.node "TYPE" ()

instance ToXML Type where
  toXML (Type a) = XML.node "type" [ toXML a ]

instance ToXML Variable where
  toXML (Variable x a) = XML.node "varDeclartion" [ toXML x , toXML a ]

instance ToXML Definition where
  toXML (Definition f a) = XML.node "funcDeclaration"
    [ toXML f
    , XML.node "typeDeclaration" [ toXML a ]
    ]
