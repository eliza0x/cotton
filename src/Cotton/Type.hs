{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase #-}

module Cotton.Type where

import Cotton.Parser (Stmt(..), Term(..), Arg(..))
import qualified Cotton.Parser as P

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

data Type 
    = Type Text
    | Func [Type] Type
    | Bottom
    deriving Eq

instance Show Type where
    show (Type "I32") = "i32"
    show (Type "Bool") = "i1"
    show (Type "Unit") = "void"
    show (Type t) = unpack t
    show (Type t) = unpack t
    show (Func as t) = "("++drop 2 (concatMap (\a -> ", " ++ show a) as)++"): "++show t

type EnvM = State Env

data Env = Env { _typeOf :: Map Text Type }
    deriving (Show, Eq)

makeLenses ''Env

typeCheck :: [Stmt] -> Env
typeCheck stmts = S.execState (mapM_ typeCheck' stmts) initState
    where
    initState = Env $ M.fromList [ ("+",  Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("-",  Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("*",  Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("/",  Func [Type "I32", Type "I32"] (Type "I32"))
                                 , ("==", Func [Type "I32", Type "I32"] (Type "Bool"))
                                 ]
    typeCheck' :: Stmt -> EnvM Type
    typeCheck' = \case
        Bind{..} -> do 
            updateEnv label (Type type')
            type'' <- last <$> mapM typeCheck' stmt
            when (Type type' /= type'') $ error "type error."
            return type''
        Fun{..}  -> do 
            checkArgs args
            updateEnv label (Func (map (Type . fromJust . P.type'') args) $ Type type') 
            types <- mapM typeCheck' stmt
            when (Type type' /= last types) $  do
                (error $ "type error.\nactual type: "++show (last types)
                                ++"\nexpected type: "++show type'
                                ++"\npos: "++show pos)
            return $ last types
        (ETerm term) -> typeCheck'' term
        where
        checkArgs args = forM_ args (\arg -> updateEnv (P.argName arg) (Type . fromJust $ P.type'' arg))
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
            type' <- fromMaybe (error $ "this variable is not defined") <$> getType var 
            type'' <- typeCheck'' term
            when (type' /= type'') $ error ("type error.\n"++show var++": "++show type' ++ "\n"++show term++": "++show type'')
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

