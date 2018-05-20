{
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Cotton.Lexer where

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
    "def"                               { tok (\p _ -> Def        p )      }
    "if"                                { tok (\p _ -> If         p )      }
    "else"                              { tok (\p _ -> Else       p )      }
    "<-"                                { tok (\p _ -> LArrow     p )      }
    "->"                                { tok (\p _ -> RArrow     p )      }
    "`"                                 { tok (\p _ -> Backquort  p )      }
    """                                 { tok (\p _ -> Quort      p )      }
    "'"                                 { tok (\p _ -> Apostrophe p )      }
    "("                                 { tok (\p _ -> LParen     p )      }
    ")"                                 { tok (\p _ -> RParen     p )      }
    "{"                                 { tok (\p _ -> LBrace     p )      }
    "}"                                 { tok (\p _ -> RBrace     p )      }
    "["                                 { tok (\p _ -> LBracket   p )      }
    "]"                                 { tok (\p _ -> RBracket   p )      }
    ":"                                 { tok (\p _ -> Colon      p )      }
    ";"                                 { tok (\p _ -> Semicolon  p )      }
    $digit+                             { tok (\p s -> Num  (read s  , p)) }
    $opchar+                            { tok (\p s -> Op   (T.pack s, p)) }
    $lower [$alpha \' \_ \?]* \!?       { tok (\p s -> Var  (T.pack s, p)) }
    $upper [$alpha]*                    { tok (\p s -> Type (T.pack s, p)) }

{
addToken :: MonadState (Stock Token) m => Token -> m ()
addToken token = modify' (\f -> f . (token:))

data LexResult
  = LexError AlexPosn
  | LexSuccess
  deriving (Eq, Show)

type Stock a = [a] -> [a]

tok f p s = f p s

-- | Token type, used to communicate between the lexer and parser
data Token
    = Def               AlexPosn
    | If                AlexPosn
    | Else              AlexPosn
    | LArrow            AlexPosn
    | RArrow            AlexPosn
    | Equal             AlexPosn
    | Backquort         AlexPosn
    | Quort             AlexPosn
    | Apostrophe        AlexPosn
    | Semicolon         AlexPosn
    | Colon             AlexPosn
    | Comma             AlexPosn
    | LParen            AlexPosn
    | RParen            AlexPosn
    | LBrace            AlexPosn
    | RBrace            AlexPosn
    | LBracket          AlexPosn
    | RBracket          AlexPosn
    | Num        (Int,  AlexPosn)
    | Op         (Text, AlexPosn)
    | Var        (Text, AlexPosn)
    | Type       (Text, AlexPosn)
    deriving (Eq, Show)

opToEqual (Op ("=", p)) = Equal p
opToEqual token         = token

lexer :: String -> [Token]
lexer = map opToEqual . alexScanTokens
}

