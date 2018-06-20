{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase, OverloadedLabels, ScopedTypeVariables  #-}

module Cotton.Type where

import Cotton.Stmt
import qualified Cotton.Stmt as P
import Cotton.Type.Type

import Data.Extensible
import Control.Lens hiding ((#))

import Control.Monad
import Data.Maybe
import Data.Text (Text, unpack)

import Data.Map.Strict (Map(..), (!?))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.State.Strict (State(..))
import qualified Control.Monad.State.Strict as S

type EnvM = State Env

newtype Env = Env { _typeOf :: Map Text Type }
    deriving (Show, Eq)

makeLenses ''Env

typeCheck :: [Stmt] -> Env
typeCheck stmts = S.execState (mapM_ typeCheckStmt stmts) preDefined
    where
    preDefined = Env $ M.fromList [ ("+",     Func [Type "I32", Type "I32"] (Type "I32"))
                                  , ("-",     Func [Type "I32", Type "I32"] (Type "I32"))
                                  , ("*",     Func [Type "I32", Type "I32"] (Type "I32"))
                                  , ("/",     Func [Type "I32", Type "I32"] (Type "I32"))
                                  , ("==",    Func [Type "I32", Type "I32"] (Type "Bool"))
                                  , ("ref",   Func [Type "I32"] (Ref $ Type "I32"))
                                  , ("unref", Func [Ref $ Type "I32"] (Type "I32"))
                                  , ("<-",    Func [Ref $ Type "I32"] (Type "I32"))
                                  ]
    typeCheckStmt :: Stmt -> EnvM Type
    typeCheckStmt (Stmt stmt) = flip matchField stmt $ shrinkAssoc
        $ #bind @= (\(r :: Bind) -> do
            updateEnv (r ^. #name) (r ^. #type) 
            type' <- typeCheckTerm (r ^. #term)
            when ((r ^. #type) /= type') $ error $ "type error.\n"++show (r ^. #type)++"\n"++show type'
            return type')
      <: #function @= (\(r :: Fun) -> do
            checkArgs (r ^. #args)
            updateEnv (r ^. #name) (Func (map (fromJust . (^. #type)) $ r ^. #args) (r ^. #type)) 
            type' <- typeCheckTerm (r ^. #term)
            when ((r ^. #type) /= type')
                (error $ "type error."
                    ++ "\nactual type: "++show type'
                    ++ "\nexpected type: "++show (r ^. #type)
                    ++ "\npos: "++show (r ^. #pos))
            return type')
      <: #term @= (\(term :: Term) -> typeCheckTerm term)
      <: nil
        where
        checkArgs :: [Arg] -> EnvM ()
        checkArgs args = forM_ args (\arg -> updateEnv (arg ^. #name) (fromJust $ arg ^. #type))

        updateEnv :: Text -> Type -> EnvM ()
        updateEnv name type' = do
            type'' <- uses typeOf (!? name)
            when (isJust type'' && fromJust type'' /= type') 
                $ error "type error."
            unless (isJust type'') $ typeOf %= M.insert name type'
    
    typeCheckTerm :: Term -> EnvM Type
    typeCheckTerm (Term term) = flip matchField term $ shrinkAssoc
          $  #nat       @= (\(_ :: Nat) -> return $ Type "I32")
          <: #var       @= (\(r :: Var) -> fromMaybe (error $ show (r ^. #name) ++ " is not defined") <$> getType (r ^. #name))
          <: #str       @= (\(_ :: Str) -> return $ Type "String")
          <: #call      @= (\(r :: Call) -> do
            argtypes <- mapM typeCheckTerm (r ^. #args)
            Func argtypes' rettype <- fromMaybe (error "this function is not defined") <$> getType (r ^. #name)
            when (argtypes /= argtypes') $ error "type error."
            return rettype)
          <: #if        @= (\(r :: If) -> do
            condType <- typeCheckTerm (r ^. #cond)
            when (condType /= Type "Bool") $ error "type error."
            thenType <- last <$> mapM typeCheckStmt (r ^. #then)
            elseType <- last <$> mapM typeCheckStmt (r ^. #else)
            when (thenType /= elseType) $ error "type error."
            return thenType)
		  <: #stmts     @= (\(stmts :: [Stmt]) ->
			last <$> mapM typeCheckStmt stmts)
          <: nil
        where
        getType var = uses typeOf (!? var)

