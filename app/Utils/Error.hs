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
    DupFn s f -> "Duplicate function " <> f <> " in scope " <> s
    DupField s f -> "Duplicate field " <> f <> " in scope " <> s
    UndefProxy p f -> "Undefined facet " <> f <> " at " <> p
    UndefFacet f p -> "Undefined proxy " <> p <> " at " <> f
    UndefField s f -> "Undefined field " <> f <> " in scope " <> s
    UndefFn s f -> "Undefined function " <> f <> " in scope " <> s
    InvalidAssignOp f loc -> "Cannot use " <> showT loc <> " assignemnt operator to assign to " <> f
    InvalidMemoryLocationType f t -> "Invalid storage specifier for " <> f <> " of type " <> showT t
    MissingProxyMemberKind f -> "Missing proxy member kind for " <> f
    IllegalStorageAssig vId fId -> "Cannot storage assign to " <> vId <> " in " <> fId


