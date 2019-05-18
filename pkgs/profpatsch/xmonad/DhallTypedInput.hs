{-# language RecordWildCards, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, KindSignatures, DataKinds, ScopedTypeVariables, RankNTypes, GADTs, TypeApplications, AllowAmbiguousTypes, LambdaCase #-}
module DhallTypedInput
( inputWithTypeArgs, TypeArg(..), TypeArgError(..), TypeArgEx(..),  typeArg
)
where

import Control.Monad.Trans.State.Strict as State
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Proxy (Proxy(Proxy))
import Data.List (foldl')

import Control.Exception (Exception)
import qualified Control.Exception

import Dhall (Type(..), InvalidType(..), InputSettings(..), EvaluateSettings(..), rootDirectory, startingContext, normalizer, standardVersion, sourceName, defaultEvaluateSettings, Interpret(..), auto)
import Dhall.TypeCheck (X)
import Dhall.Core
import Dhall.Parser (Src(..))
import qualified Dhall.Import
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Parser

import Lens.Family (LensLike', set, view)

import qualified Data.Text as Text

import Data.Text.Prettyprint.Doc (Pretty)
import qualified Data.Text.Prettyprint.Doc               as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text   as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty


data TypeArg (sym :: Symbol) t = TypeArg

-- | existential wrapper of a type arg
data TypeArgEx
  where TypeArgEx :: (KnownSymbol sym, Interpret t) => TypeArg sym t -> TypeArgEx

typeArg :: forall sym t. (KnownSymbol sym, Interpret t) => TypeArgEx
typeArg = TypeArgEx (TypeArg :: TypeArg sym t)

data TypeArgError
  = WrongLabel Text.Text
  | NoLambda

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

getLabel :: forall sym t. (KnownSymbol sym) => TypeArg sym t -> Text.Text
getLabel _ = Text.pack $ symbolVal (Proxy :: (Proxy :: Symbol -> *) sym)

instance (KnownSymbol sym) => Show (TypeArg sym t) where
  show TypeArg =
    "TypeArg "
    ++ (symbolVal (Proxy :: (Proxy :: Symbol -> *) sym))

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

    let skipNote e f = case e of
          Note n e -> Note n $ f e
          e -> f e

    let
      stripTypeArg :: Expr Src X -> TypeArgEx -> Expr Src X
      stripTypeArg e (TypeArgEx ta) = skipNote e $ \e' -> case e' of
          (Lam label _ _) ->
            case applyTypeArg e' ta of
              Right e'' -> e''
              -- TODO obvously
              Left (WrongLabel l) ->
                error $ "Wrong label, should have been `" ++ Text.unpack l ++ "` but was `" ++ Text.unpack label ++ "`"
              Left NoLambda -> error $ "I expected a lambda of the form λ(" ++ Text.unpack label ++ ": Type) → but got: " ++ show e
          e' -> error $ show e'

    let expr'' = foldl' stripTypeArg expr' typeArgs

    -- - check first arg is \(label: Type) lambda
    -- - type-check expression
    -- - apply the type we label to signify to expression
    -- - type-check the new normalized expression
    --   - TODO: check if the error message makes sense to the user
    let suffix = prettyToStrictText expected
    let annot = case expr'' of
            Note (Src begin end bytes) _ ->
                -- Note (Src begin end bytes') (Annot expr'' expected)
                Annot expr'' expected
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expr'' expected
    print annot
    _ <- throws (Dhall.TypeCheck.typeWith (view startingContext settings) annot)
    case extract (Dhall.Core.normalizeWith (Dhall.Core.getReifiedNormalizer (view normalizer settings)) expr'') of
        Just x  -> return x
        Nothing -> Control.Exception.throwIO InvalidType

prettyToStrictText :: Pretty a => a -> Text.Text
prettyToStrictText = docToStrictText . Pretty.pretty

docToStrictText :: Pretty.Doc ann -> Text.Text
docToStrictText = Pretty.renderStrict . Pretty.layoutPretty options
  where
   options = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.Unbounded }

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r

-- data WrongTypeLabel = WrongTypeLabel deriving (Typeable)

-- _ERROR :: String
-- _ERROR = "\ESC[1;31mError\ESC[0m"

-- instance Show WrongTypeLabel where
--     show WrongTypeLabel =
--         _ERROR <> ": Mislabelled type lambda
--         \                                                                                \n\
--         \Expected your t provide an extract function that succeeds if an expression      \n\
--         \matches the expected type.  You provided a Type that disobeys this contract     \n"

-- instance (KnownSymbol s, D.Interpret t, D.Interpret a)
--   => D.Interpret (TypeArg s t a)
--   where
--   autoWith opts = D.Type
--     { D.extract = extr
--     -- TODO: the symbolVal is ignored for the type check;
--     -- yet, if the type check succeeds, we cannot fail
--     -- in the extract function (even if the label is wrong)
--     , D.expected = DC.Pi (T.pack expectedLabel) (DC.Const DC.Type) aExpect
--     }
--       where
--         expectedLabel = symbolVal (Proxy :: (Proxy :: Symbol -> *) s)
--         D.Type aExtr aExpect = D.autoWith opts :: D.Type a
--         D.Type tExtr tExpect = D.autoWith opts :: D.Type t
--         extr expr@(DC.Lam label (DC.Const DC.Type) _)
--           | label == T.pack (symbolVal (Proxy :: (Proxy :: Symbol -> *) s))
--               = Just (case aExtr (DC.normalize (DC.App expr tExpect)) of
--                         Just a -> TypeArg a
--                         Nothing -> error $ show (DC.normalize (DC.App expr tExpect) :: DC.Expr () DCT.X))
--           -- TODO: nice error message?!
--           | otherwise = error ("Wrong type label; expected "
--                                ++ expectedLabel ++ ", was " ++ T.unpack label)
--         extr _ = Nothing
