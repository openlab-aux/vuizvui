{-# language RecordWildCards, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, KindSignatures, DataKinds, ScopedTypeVariables, RankNTypes, GADTs, TypeApplications, AllowAmbiguousTypes, LambdaCase #-}
{- Exports the `inputWithTypeArgs` function, which is able to read dhall files of the normalized form

@
\(CustomType: Type) ->
\(AnotherType: Type) ->
…
@

and set their actual representation on the Haskell side:

This has various advantages:

- dhall files still type check & normalize with the normal dhall
  tooling, they are standalone (and the types can be instantiated from
  dhall as well without any workarounds)
- It can be used like the default `input` function, no injection of
  custom symbols in the Normalizer is reqired
- Brings this style of dhall integration to Haskell, where it was only
  feasible in nix before, because that is untyped

The dhall types can be instantiated by every Haskell type that has an
`Interpret` instance. The “name” of the type lambda variable is
compared on the Haskell side with a type-level string that the user
provides, to prevent mixups.

TODO:
- Improve error messages (!)
- Provide a way to re-use the type mapping on the Haskell side, so
  that the returned values are not just the normal `Interpret` types,
  but the mapped ones (with name phantom type)
-}
module DhallTypedInput
( inputWithTypeArgs, TypeArg(..), TypeArgError(..), TypeArgEx(..),  typeArg
)
where

import Control.Monad.Trans.State.Strict as State
import Data.List (foldl')
import Control.Exception (Exception)
import qualified Control.Exception
import qualified Data.Text as Text

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Proxy (Proxy(Proxy))

import Dhall (Type(..), InvalidType(..), InputSettings(..), EvaluateSettings(..), rootDirectory, startingContext, normalizer, standardVersion, sourceName, defaultEvaluateSettings, Interpret(..), auto)
import Dhall.TypeCheck (X)
import Dhall.Core
import Dhall.Parser (Src(..))
import qualified Dhall.Import
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Parser

import Lens.Family (LensLike', set, view)

import Data.Text.Prettyprint.Doc (Pretty)
import qualified Data.Text.Prettyprint.Doc               as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text   as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty


-- | Information about a type argument in the dhall input
--
-- If the dhall file starts with @\(CustomType : Type) ->@,
-- that translates to @TypeArg "CustomType" interpretionType@
-- where @"CustomType"@ is a type-level string describing the
-- name of the type in the dhall file (as a sanity check) and
-- @interpretationType@ is any type which implements
-- 'Dhall.Interpret'.
--
-- This is basically a specialized 'Data.Proxy'.
data TypeArg (sym :: Symbol) t = TypeArg

-- | Existential wrapper of a 'TypeArg', allows to create a list
-- of heterogenous 'TypeArg's.
data TypeArgEx
  where TypeArgEx :: (KnownSymbol sym, Interpret t) => TypeArg sym t -> TypeArgEx

-- | Shortcut for creating a 'TypeArgEx'.
--
-- Use with @TypeApplications@:
--
-- @
-- typeArg @"CustomType" @Integer
-- @
typeArg :: forall sym t. (KnownSymbol sym, Interpret t) => TypeArgEx
typeArg = TypeArgEx (TypeArg :: TypeArg sym t)

-- | Possible errors returned when applying a 'TypeArg'
-- to a 'Dhall.Expr'.
data TypeArgError
  = WrongLabel Text.Text
  -- ^ The name (label) of the type was different,
  -- the text value is the expected label.
  | NoLambda
  -- ^ The 'Dhall.Expr' does not start with 'Dhall.Lam'.

-- | Apply a 'TypeArg' to a 'Dhall.Expr'.
--
-- Checks that the dhall file starts with the 'Dhall.Lam'
-- corresponding to 'TypeArg`, then applies @t@ (dhall type application)
-- and normalizes, effectively stripping the 'Dhall.Lam'.
applyTypeArg
  :: forall sym t. (KnownSymbol sym, Interpret t)
  => Expr Src X
  -> TypeArg sym t
  -> Either TypeArgError (Expr Src X)
applyTypeArg expr ta@(TypeArg) = case expr of
  (Lam label (Const Dhall.Core.Type) _)
    -> let expectedLabel = getLabel ta
        in if label /= getLabel ta
           then Left (WrongLabel expectedLabel)
           else let expr' = (normalize (App expr tExpect))
                  in Right expr'
    where
        Dhall.Type _ tExpect = Dhall.auto :: Dhall.Type t
  expr -> Left NoLambda

-- | Inflect the type-level string @sym@ to a text value.
getLabel :: forall sym t. (KnownSymbol sym) => TypeArg sym t -> Text.Text
getLabel _ = Text.pack $ symbolVal (Proxy :: (Proxy :: Symbol -> *) sym)

instance (KnownSymbol sym) => Show (TypeArg sym t) where
  show TypeArg =
    "TypeArg "
    ++ (symbolVal (Proxy :: (Proxy :: Symbol -> *) sym))

-- | Takes a list of 'TypeArg's and parses the given
-- dhall string, applying the given 'TypeArg's in order
-- to the opaque dhall type arguments (see 'TypeArg' for
-- how these should look).
--
-- This is a slightly changed 'Dhall.inputWith'.
--
-- Discussion: Any trace of our custom type is removed from
-- the resulting
inputWithTypeArgs
  :: InputSettings
  -> [TypeArgEx]
  -> Dhall.Type a
  -> Text.Text
  -> IO a
inputWithTypeArgs settings typeArgs (Dhall.Type {extract, expected}) txt = do
    expr <- throws (Dhall.Parser.exprFromText (view sourceName settings) txt)

    -- TODO: evaluateSettings not exposed
    -- let evSettings = view evaluateSettings settings
    let evSettings :: EvaluateSettings = defaultEvaluateSettings

    -- -vvv copied verbatim from 'Dhall.inputWith' vvv-
    let transform =
               set Dhall.Import.standardVersion
               (view standardVersion evSettings)
            .  set Dhall.Import.normalizer
               (view normalizer evSettings)
            .  set Dhall.Import.startingContext
               (view startingContext evSettings)

    let status = transform (Dhall.Import.emptyStatus
                            (view rootDirectory settings))

    expr' <- State.evalStateT (Dhall.Import.loadWith expr) status
    -- -^^^ copied verbatim ^^^-

    let
      -- | if there’s a note, run the transformation and rewrap with the note
      skipNote e f = case e of
          Note n e -> Note n $ f e
          e -> f e

    let
      -- | strip one 'TypeArg'
      stripTypeArg :: Expr Src X -> TypeArgEx -> Expr Src X
      stripTypeArg e (TypeArgEx ta) = skipNote e $ \e' -> case e' of
          (Lam label _ _) ->
            case applyTypeArg e' ta of
              Right e'' -> e''
              -- TODO obvously improve error messages
              Left (WrongLabel l) ->
                error $ "Wrong label, should have been `" ++ Text.unpack l ++ "` but was `" ++ Text.unpack label ++ "`"
              Left NoLambda -> error $ "I expected a lambda of the form λ(" ++ Text.unpack label ++ ": Type) → but got: " ++ show e
          e' -> error $ show e'

    -- strip all 'TypeArg's
    let expr'' = foldl' stripTypeArg expr' typeArgs

    -- -vvv copied verbatim as well (expr' -> expr'') vvv-
    let suffix = prettyToStrictText expected
    let annot = case expr'' of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expr'' expected)
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expr'' expected

    _ <- throws (Dhall.TypeCheck.typeWith (view startingContext settings) annot)
    case extract (Dhall.Core.normalizeWith (Dhall.Core.getReifiedNormalizer (view normalizer settings)) expr'') of
        Just x  -> return x
        Nothing -> Control.Exception.throwIO InvalidType


-- copied from Dhall.Pretty.Internal
prettyToStrictText :: Pretty a => a -> Text.Text
prettyToStrictText = docToStrictText . Pretty.pretty

-- copied from Dhall.Pretty.Internal
docToStrictText :: Pretty.Doc ann -> Text.Text
docToStrictText = Pretty.renderStrict . Pretty.layoutPretty options
  where
   options = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.Unbounded }

-- copied from somewhere in Dhall
throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r


-- TODO: add errors like these
-- data WrongTypeLabel = WrongTypeLabel deriving (Typeable)

-- _ERROR :: String
-- _ERROR = "\ESC[1;31mError\ESC[0m"

-- instance Show WrongTypeLabel where
--     show WrongTypeLabel =
--         _ERROR <> ": Mislabelled type lambda
--         \                                                                                \n\
--         \Expected your t provide an extract function that succeeds if an expression      \n\
--         \matches the expected type.  You provided a Type that disobeys this contract     \n"
