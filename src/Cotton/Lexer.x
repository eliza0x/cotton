{
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Cotton.Lexer where

import Control.Monad.State
import Data.Text (Text(..))
import qualified Data.Text as T
}

%wrapper "basic"

$digit = 0-9
$opchar = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$alpha       = [A-Za-z\_]
$upper       = [A-Z]
$lower       = [a-z\_]
$labelchar   = [A-Za-z0-9\_]

tokens :-
    $white+                             ;
    "def"                               { \_   -> Def               }
    "if"                                { \_   -> If                }
    "else"                              { \_   -> Else              }
    "<-"                                { \_   -> LArrow            }
    "->"                                { \_   -> RArrow            }
    "`"                                 { \_   -> Backquort         }
    "\""                                { \_   -> Quort             }
    "\'"                                { \_   -> Apostrophe        }
    "("                                 { \_   -> LParen            }
    ")"                                 { \_   -> RParen            }
    "{"                                 { \_   -> LBrace            }
    "}"                                 { \_   -> RBrace            }
    "["                                 { \_   -> LBracket          }
    "]"                                 { \_   -> RBracket          }
    ":"                                 { \_   -> Colon             }
    ";"                                 { \_   -> Semicolon         }
    $digit+                             { \str -> Num  $ read str   }
    $opchar+                            { \str -> Op   $ T.pack str }
    $lower [$alpha \' \_ \?]* \!?       { \str -> Var  $ T.pack str }
    $upper [$alpha]*                    { \str -> Type $ T.pack str }

{
addToken :: MonadState (Stock Token) m => Token -> m ()
addToken token = modify' (\f -> f . (token:))

lexProg :: MonadState (Stock Token) m => String -> m LexResult
lexProg str = go initialInput
  where
    initialInput = ('\n', [], str)

    inputStr (_, _, s) = s

    go input = case alexScan input 0 of
      AlexEOF                  -> return LexSuccess
      AlexError (_, _, rest)   -> return $ LexError rest
      AlexSkip input' _        -> go input'
      AlexToken input' len act -> do
        let token = act $ take len $ inputStr input
        addToken token
        go input'

data LexResult
  = LexError String
  | LexSuccess
  deriving (Eq, Show)

type Stock a = [a] -> [a]


-- | Token type, used to communicate between the lexer and parser
data Token
    = Def 
    | If 
    | Else 
    | LArrow 
    | RArrow 
    | Equal 
    | Backquort 
    | Quort 
    | Apostrophe
    | Semicolon 
    | Colon 
    | Comma 
    | LParen 
    | RParen 
    | LBrace 
    | RBrace 
    | LBracket 
    | RBracket 
    | Num Int
    | Op  Text
    | Var Text
    | Type Text
    deriving (Eq, Show)

tokenize str = ($ []) <$> runState (lexProg str) id

opToEqual (Op "=") = Equal
opToEqual token    = token

lexer :: String -> Maybe [Token]
lexer str = (\(result, ts) -> if result == LexSuccess 
            then Just (map opToEqual ts) 
            else Nothing) $ tokenize str 
}

