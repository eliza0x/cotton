module Spec.Lexer where

import Test.Hspec
import Cotton.Lexer (Token(..))
import qualified Cotton.Lexer as L
import Control.Monad.IO.Class

spec :: Spec
spec = let
    simpleFun   = "def foo(): I32 = 10;"
    simpleFun'  = "def bar(a,b): I32 { 10 }"
    simpleBind  = "def baz: I32 = 10;"
    simpleBind' = "def piyo: I32 { 10 }"
    simpleIf    = "if 1 == 10 { a } else { b }"
    simpleCall  = "f(a, b, c)"
    simpleType  = "Ref<I32>"
    in describe "lexer" $ do
        it simpleFun $ let
            simpleFunToken = [def, lower, lparen, rparen, colon, upper, equal, num, semicolon]
            in L.lexer simpleFun `eqTokens` simpleFunToken
        it simpleFun' $ let
            simpleFunToken = [def, lower, lparen, lower, comma, lower, rparen, colon, upper, lbrace, num, rbrace]
            in L.lexer simpleFun' `eqTokens` simpleFunToken
        it simpleBind $ let
            simpleFunToken = [def, lower, colon, upper, equal, num, semicolon]
            in L.lexer simpleBind `eqTokens` simpleFunToken
        it simpleBind' $ let
            simpleFunToken = [def, lower, colon, upper, lbrace, num, rbrace]
            in L.lexer simpleBind' `eqTokens` simpleFunToken
        it simpleIf $ let
            simpleIfToken = [if', num, op, num, lbrace, lower, rbrace, else', lbrace, lower, rbrace]
            in L.lexer simpleIf `eqTokens` simpleIfToken
        it simpleCall $ let
            simpleCallToken = [lower, lparen, lower, comma, lower, comma, lower, rparen]
            in L.lexer simpleCall `eqTokens` simpleCallToken
        it simpleType $ let
            simpleTypeToken = [upper, langle, upper, rangle]
            in L.lexer simpleType `eqTokens` simpleTypeToken

eqTokens :: [Token] -> [Token] -> Bool
eqTokens a b = and $ zipWith eqToken a b

def        = Def        nullPos
if'        = If         nullPos
else'      = Else       nullPos
larrow     = LArrow     nullPos
rarrow     = RArrow     nullPos
equal      = Equal      nullPos
backquote  = BackQuote  nullPos
quort      = Quort      nullPos
apostrophe = Apostrophe nullPos
semicolon  = Semicolon  nullPos
colon      = Colon      nullPos
comma      = Comma      nullPos
langle     = LAngle     nullPos
rangle     = RAngle     nullPos
lparen     = LParen     nullPos
rparen     = RParen     nullPos
lbrace     = LBrace     nullPos
rbrace     = RBrace     nullPos
lbracket   = LBracket   nullPos
rbracket   = RBracket   nullPos
num        = Num        nullPos
op         = Op         nullPos
lower      = Lower      nullPos
upper      = Upper      nullPos
str        = Str        nullPos

nullPos = L.Pos $ L.AlexPn 0 0 0

eqToken (Def        _) (Def        _) = True
eqToken (If         _) (If         _) = True
eqToken (Else       _) (Else       _) = True
eqToken (LArrow     _) (LArrow     _) = True
eqToken (RArrow     _) (RArrow     _) = True
eqToken (Equal      _) (Equal      _) = True
eqToken (BackQuote  _) (BackQuote  _) = True
eqToken (Quort      _) (Quort      _) = True
eqToken (Apostrophe _) (Apostrophe _) = True
eqToken (Semicolon  _) (Semicolon  _) = True
eqToken (Colon      _) (Colon      _) = True
eqToken (Comma      _) (Comma      _) = True
eqToken (LAngle     _) (LAngle     _) = True
eqToken (RAngle     _) (RAngle     _) = True
eqToken (LParen     _) (LParen     _) = True
eqToken (RParen     _) (RParen     _) = True
eqToken (LBrace     _) (LBrace     _) = True
eqToken (RBrace     _) (RBrace     _) = True
eqToken (LBracket   _) (LBracket   _) = True
eqToken (RBracket   _) (RBracket   _) = True
eqToken (Num        _) (Num        _) = True
eqToken (Op         _) (Op         _) = True
eqToken (Lower      _) (Lower      _) = True
eqToken (Upper      _) (Upper      _) = True
eqToken (Str        _) (Str        _) = True
eqToken _ _                           = False

