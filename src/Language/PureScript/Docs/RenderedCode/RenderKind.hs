{-# LANGUAGE ScopedTypeVariables #-}
-- | Functions for producing RenderedCode values from PureScript Kind values.
--
module Language.PureScript.Docs.RenderedCode.RenderKind
  ( renderKind
  ) where

-- TODO: This is pretty much copied from Language.PureScript.Pretty.Kinds.
-- Ideally we would unify the two.

import Prelude.Compat

import Control.Arrow (ArrowPlus(..))
import Control.PatternArrows as PA

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Kinds

import Language.PureScript.Docs.RenderedCode.Types

typeLiterals :: Pattern () (Kind a) RenderedCode
typeLiterals = mkPattern match
  where
  match (KUnknown _ u) =
    Just $ typeVar $ T.cons 'k' (T.pack (show u))
  match (NamedKind _ n) =
    Just $ kind n
  match _ = Nothing

-- Row a (Kind a)
matchRow :: Pattern () (Kind a) ((), Kind a)
matchRow = mkPattern match
  where
  match (Row _ k) = Just ((), k)
  match _ = Nothing

-- FunKind a (Kind a) (Kind a)
funKind :: Pattern () (Kind a) (Kind a, Kind a)
funKind = mkPattern match
  where
  match (FunKind _ arg ret) = Just (arg, ret)
  match _ = Nothing

-- KUnknown a Int
unknownKind :: Pattern () (Kind a) ((), Int)
unknownKind = mkPattern match
  where
  match (KUnknown _ n) = Just ((), n)
  match _ = Nothing

-- NamedKind a (Qualified (ProperName 'KindName))
namedKind :: Pattern () (Kind a) ((), String)
namedKind = mkPattern match
    where
    match (NamedKind _ _) = Just ((), "a name")
    match _ = Nothing

-- | Generate RenderedCode value representing a Kind
renderKind :: forall a. Kind a -> RenderedCode
renderKind
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchKind ()
  where
  matchKind :: Pattern () (Kind a) RenderedCode
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchKind)

  operators :: OperatorTable () (Kind a) RenderedCode
  operators =
    OperatorTable [ [ Wrap matchRow $ \_ k -> syntax "#" <> sp <> k]
                  , [ AssocR funKind $ \arg ret -> arg <> sp <> syntax "->" <> sp <> ret ]
                  , [ Split unknownKind $ \_ _ -> syntax "---UNKNOWN---"]
                  , [ Split namedKind $ \_ _ -> syntax "---NAMED---"]
                  ]