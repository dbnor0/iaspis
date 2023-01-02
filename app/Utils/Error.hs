{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Utils.Error where

import Data.Text as T
import Analysis.Environment.Error
import Utils.Text (showT)

class IaspisError a where
  showErr :: a -> T.Text

instance IaspisError BuildError where
  showErr = \case
    DupContract c -> "Duplicate contract identifier " <> c 
    DupProxy p -> "Duplicate proxy identifier " <> p
    DupFacet f -> "Duplicate facet identifier " <> f
    DupModule m -> "Duplicate module " <> m
    DupFn s f -> "Duplicate function " <> f <> " in scope " <> s
    DupField s f -> "Duplicate field " <> f <> " in scope " <> s
    UndefProxy p f -> "Undefined facet " <> f <> " at " <> p
    UndefFacet f p -> "Undefined proxy " <> p <> " at " <> f
    UndefField s f -> "Undefined field " <> f <> " in scope " <> s
    UndefFn s f -> "Undefined function " <> f <> " in scope " <> s
    UndefType s t -> "Undefined type " <> t <> " in scope " <> s
    UndefImport i m -> "Undefined import " <> i <> " in module " <> m
    UndefExport id i m -> "Module " <> m <> " does not export " <> i <> " in " <> id
    InvalidAssignOp f loc -> "Cannot use " <> showT loc <> " assignemnt operator to assign to " <> f
    InvalidMemoryLocationType f t -> "Invalid storage specifier for " <> f <> " of type " <> showT t
    MissingProxyMemberKind f -> "Missing proxy member kind for " <> f
    UndeclaredFacet facet field -> "Use of undeclared facet " <> facet <> " in declaration of " <> field
    IllegalStorageAssig vId fId -> "Cannot storage assign to " <> vId <> " in " <> fId
    InvalidAssigType fId t1 t2 -> "Cannot assign " <> showT t2 <> " to " <> fId <> " of type " <> showT t1
    InvalidReturnType fId t1 t2 -> "Expected " <> showT t1 <> " as return type for " <> fId <> ", got " <> showT t2
    InvalidExpressionType e t1 t2 -> "Expected " <> showT t1 <> " for " <> showT e <> ", got " <> showT t2
    InvalidArgType f t1 t2 -> "Expected " <> showT t1 <> " for " <> showT f <> ", got " <> showT t2
    InvalidFieldRef field facet -> field <> " was not declared as a proxy field for facet " <> facet
    InvalidOp -> "Internal: Invalid operator used"


