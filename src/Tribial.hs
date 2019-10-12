{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tribial where

import GhcPlugins

import TcEvidence
import TyCoRep

hello :: EvTerm
hello = EvExpr . Lit $ LitLabel "hello" Nothing IsData

world :: EvTerm
world = EvExpr . Lit $ LitString "world"

someType :: Type
someType = LitTy $ StrTyLit "Nat"

some :: EvTerm
some = EvExpr $ Type someType

eight :: EvTerm
eight = EvExpr . Type . LitTy $ NumTyLit 8
