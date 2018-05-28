{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase #-}

module Cotton.Type where

import Cotton.Stmt
import qualified Cotton.Stmt as P
import Cotton.Type.Type

import Debug.Trace

import Control.Lens
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
typeCheck stmts = S.execState (mapM_ typeCheck' stmts) initState
    where
    initState = Env $ M.fromList [ ("+",     Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("-",     Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("*",     Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("/",     Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("==",    Func [Type "I32", Type "I32"] (Type "Bool"))
                                 , ("ref",   Func [Type "I32"] (Ref $ Type "I32"))
                                 , ("unref", Func [Ref $ Type "I32"] (Type "I32"))
                                 ]
    typeCheck' :: Stmt -> EnvM Type
    typeCheck' = \case
        Bind{..} -> do 
            updateEnv label type'
            type'' <- last <$> mapM typeCheck' stmt
            when (type' /= type'') $ error $ "type error.\n"++show type'++"\n"++show type''
            return type''
        Fun{..}  -> do 
            checkArgs args
            updateEnv label (Func (map (fromJust . P.type'') args) type') 
            types <- mapM typeCheck' stmt
            when (type' /= last types)
                (error $ "type error.\nactual type: "++show (last types)
                                ++"\nexpected type: "++show type'
                                ++"\npos: "++show pos)
            return $ last types
        (ETerm term) -> typeCheck'' term
        where
        checkArgs args = forM_ args (\arg -> updateEnv (P.argName arg) (fromJust $ P.type'' arg))
        updateEnv label type' = do
            type'' <- uses typeOf (!? label)
            when (isJust type'' && fromJust type'' /= type') 
                $ error "type error."
            unless (isJust type'') $ typeOf %= M.insert label type'
    
    typeCheck'' :: Term -> EnvM Type
    typeCheck'' = \case
        TInt{..}      -> return $ Type "I32"
        Var{..}       -> fromMaybe (error $ show var ++ " is not defined") <$> getType var 
        TStr{..}      -> return $ Type "String"
        Overwrite{..} -> do
            type' <- fromMaybe (error $ "this variable is not defined: "++show var) <$> getType var 
            type'' <- typeCheck'' term
            when (type' /= Ref type'') $ error ("type error.\n"++show var++": "++show type' ++ "\n"++show term++": "++show type'')
            return $ Type "Unit"
        Op{..}        -> do
            typeTerm  <- typeCheck'' term
            typeTerm' <- typeCheck'' term'
            when (typeTerm /= typeTerm') $ error "type error."
            Func args rettype <- fromMaybe (error $ show op ++ " is not defined") <$> getType op
            when ([typeTerm, typeTerm'] /= args) $ error "type error."
            return rettype
        Call{..}        -> do
            argTypes <- mapM typeCheck'' targs
            Func args rettype <- fromMaybe (error "this function is not defined") <$> getType var
            when (argTypes /= args) $ error "type error."
            return rettype
        SemiColon{..}   -> do
            typeCheck'' term
            typeCheck'' term'
        If{..}          -> do
            condType <- typeCheck'' cond
            when (condType /= Type "Bool") $ error "type error."
            thenType <- last <$> mapM typeCheck' tstmts
            elseType <- last <$> mapM typeCheck' tstmts'
            when (thenType /= elseType) $ error "type error."
            return thenType
        where
        getType var = uses typeOf (!? var)

