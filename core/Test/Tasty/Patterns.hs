-- This code is largely borrowed from test-framework
{-
Copyright (c) 2008, Maximilian Bolingbroke
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this list of
      conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of
      conditions and the following disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of Maximilian Bolingbroke nor the names of other contributors may be used to
      endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Patterns
  ( TestPattern
  , parseTestPattern
  , noPattern
  , testPatternMatches
  ) where

import Test.Tasty.Options

import Text.Regex.Posix.Wrap
import Text.Regex.Posix.String()

import Data.List
import Data.Typeable


data Token = SlashToken
           | WildcardToken
           | DoubleWildcardToken
           | LiteralToken Char
           deriving (Eq)

tokenize :: String -> [Token]
tokenize ('/':rest)     = SlashToken : tokenize rest
tokenize ('*':'*':rest) = DoubleWildcardToken : tokenize rest
tokenize ('*':rest)     = WildcardToken : tokenize rest
tokenize (c:rest)       = LiteralToken c : tokenize rest
tokenize []             = []


data TestPatternMatchMode = TestMatchMode
                          | PathMatchMode

data TestPattern = TestPattern {
        tp_categories_only :: Bool,
        tp_negated :: Bool,
        tp_match_mode :: TestPatternMatchMode,
        tp_tokens :: [Token]
    } | NoPattern
    deriving Typeable

-- | A pattern that matches anything.
noPattern :: TestPattern
noPattern = NoPattern

instance Read TestPattern where
    readsPrec _ string = [(parseTestPattern string, "")]

instance IsOption TestPattern where
    defaultValue = noPattern
    parseValue = Just . parseTestPattern
    optionName = return "pattern"
    optionHelp = return "Select only tests that match pattern"

parseTestPattern :: String -> TestPattern
parseTestPattern string = TestPattern {
        tp_categories_only = categories_only,
        tp_negated = negated,
        tp_match_mode = match_mode,
        tp_tokens = tokens''
    }
  where
    tokens = tokenize string
    (negated, tokens')
      | (LiteralToken '!'):rest <- tokens = (True, rest)
      | otherwise                         = (False, tokens)
    (categories_only, tokens'')
      | (prefix, [SlashToken]) <- splitAt (length tokens' - 1) tokens' = (True, prefix)
      | otherwise                                                      = (False, tokens')
    match_mode
      | SlashToken `elem` tokens = PathMatchMode
      | otherwise                = TestMatchMode


testPatternMatches :: TestPattern -> [String] -> Bool
testPatternMatches NoPattern _ = True
testPatternMatches test_pattern path = not_maybe $ any (=~ tokens_regex) things_to_match
  where
    not_maybe | tp_negated test_pattern = not
              | otherwise               = id
    path_to_consider | tp_categories_only test_pattern = dropLast 1 path
                     | otherwise                       = path
    tokens_regex = buildTokenRegex (tp_tokens test_pattern)
    
    things_to_match = case tp_match_mode test_pattern of
        -- See if the tokens match any single path component
        TestMatchMode -> path_to_consider
        -- See if the tokens match any prefix of the path
        PathMatchMode -> map pathToString $ inits path_to_consider


buildTokenRegex :: [Token] -> String
buildTokenRegex [] = []
buildTokenRegex (token:tokens) = concat (firstTokenToRegex token : map tokenToRegex tokens)
  where
    firstTokenToRegex SlashToken = "^"
    firstTokenToRegex other = tokenToRegex other
      
    tokenToRegex SlashToken = "/"
    tokenToRegex WildcardToken = "[^/]*"
    tokenToRegex DoubleWildcardToken = "*"
    tokenToRegex (LiteralToken lit) = regexEscapeChar lit

regexEscapeChar :: Char -> String
regexEscapeChar c | c `elem` "\\*+?|{}[]()^$." = '\\' : [c]
                  | otherwise                  = [c]

pathToString :: [String] -> String
pathToString path = concat (intersperse "/" path)

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse
