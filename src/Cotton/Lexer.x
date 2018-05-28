{
{-|
Module      : Cotton.Lexer
Description : tokenizer
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX

ソースコードをトークン列に分解します。

-}

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Cotton.Lexer (
    lexer,
    AlexPosn(..),
    Token(..),
    TokenData(..),
    ) where

import Control.Monad.State
import Data.Text (Text(..))
import qualified Data.Text as T
}

%wrapper "posn"

$digit = 0-9
$opchar = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$alpha       = [A-Za-z\_]
$upper       = [A-Z]
$lower       = [a-z\_]
$labelchar   = [A-Za-z0-9\_]

tokens :-
    $white+                             ;
    "#".*                               ;
    "def"                               { tok (\p _ -> Def        (Pos p) )       }
    "if"                                { tok (\p _ -> If         (Pos p) )       }
    "else"                              { tok (\p _ -> Else       (Pos p) )       }
    "<-"                                { tok (\p _ -> LArrow     (Pos p) )       }
    "->"                                { tok (\p _ -> RArrow     (Pos p) )       }
    "<"                                 { tok (\p _ -> LAngle     (Pos p) )       }
    ">"                                 { tok (\p _ -> RAngle     (Pos p) )       }
    "`"                                 { tok (\p _ -> BackQuote  (Pos p) )       }
    "'"                                 { tok (\p _ -> Apostrophe (Pos p) )       }
    "("                                 { tok (\p _ -> LParen     (Pos p) )       }
    ")"                                 { tok (\p _ -> RParen     (Pos p) )       }
    "{"                                 { tok (\p _ -> LBrace     (Pos p) )       }
    "}"                                 { tok (\p _ -> RBrace     (Pos p) )       }
    "["                                 { tok (\p _ -> LBracket   (Pos p) )       }
    "]"                                 { tok (\p _ -> RBracket   (Pos p) )       }
    ":"                                 { tok (\p _ -> Colon      (Pos p) )       }
    ";"                                 { tok (\p _ -> Semicolon  (Pos p) )       }
    """ $labelchar* """                 { tok (\p s -> Str (StrP (T.pack s) p))   }
    $digit+                             { tok (\p s -> Num  (NumP (read s) p))    }
    $opchar+                            { tok (\p s -> Op   (StrP (T.pack s) p))  }
    $lower [$labelchar \' \_ \?]* \!?       { tok (\p s -> Lower (StrP (T.pack s) p)) }
    $upper [$labelchar \' \_ \?]* \!?       { tok (\p s -> Upper (StrP (T.pack s) p)) }

{
tok f p s = f p s

data Token
    = Def               TokenData
    | If                TokenData
    | Else              TokenData
    | LArrow            TokenData
    | RArrow            TokenData
    | Equal             TokenData
    | BackQuote         TokenData
    | Quort             TokenData
    | Apostrophe        TokenData
    | Semicolon         TokenData
    | Colon             TokenData
    | Comma             TokenData
    | LAngle            TokenData
    | RAngle            TokenData
    | LParen            TokenData
    | RParen            TokenData
    | LBrace            TokenData
    | RBrace            TokenData
    | LBracket          TokenData
    | RBracket          TokenData
    | Num               TokenData
    | Op                TokenData
    | Lower             TokenData
    | Upper             TokenData
    | Str               TokenData
    deriving (Eq, Show)

data TokenData = Pos  {pos :: AlexPosn}
               | NumP {num :: Int, pos :: AlexPosn}
               | StrP {text :: Text, pos :: AlexPosn}
               deriving (Show, Eq)

opToEqual (Op (StrP "=" p)) = Equal (Pos p)
opToEqual token         = token

lexer :: String -> [Token]
lexer = map opToEqual . alexScanTokens
}

