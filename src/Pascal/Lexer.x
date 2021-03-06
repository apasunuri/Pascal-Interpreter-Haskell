{

{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Pascal.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPosN
  )
where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$sq = \'

-- TODO: Map symbols into token types (with or without parameters)
tokens :-
  $white+                               ; -- remove multiple white-spaces
  "//".*                                ; -- skip one line comments
  \(\*.*\*\)                            ;
  $digit+                               { tok_read     TokenInt }
  $digit+\.$digit+                      { tok_read     TokenFloat }
  [\/]|[\-]|[\+]|[\*]|[=]|writeln|
  sin|cos|sqrt|ln|exp|break|continue|
  and|not|or|if|else|case|while|for
  |[\>]|[\<]|[\<][=]|[\>][=]|[\<][\>]   { tok_string     TokenOp }                  
  [\:]|[\;]|[\,]|[\.]|[\(]|[\)]|$sq|         
  begin|end|true|false|program|var|
  real|boolean|then|of|do|to|function
  |procedure|[\"]                       { tok_string     TokenK }
  [:][=]                                { tok_string     TokenOp }
  $alpha [$alpha $digit \_ \']*         { tok_string   TokenID }
  $sq .* $sq                            { tok_string   TokenContent }

{

-- Some action helpers:
tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_read x = tok' (\s -> x (read (B.unpack s)))

-- The token type:
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p


-- TODO: Add your own token types here
data TokenClass
 = TokenOp      String
 | TokenK       String
 | TokenInt     Int
 | TokenFloat   Float
 | TokenID      String
 | TokenContent String
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
