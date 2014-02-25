-- | Ingredient for registering user-defined options
module Test.Tasty.Ingredients.IncludingOptions where

import Test.Tasty.Ingredients
import Test.Tasty.Options

-- | This ingredient doesn't do anything apart from registering additional
-- options.
--
-- The option values can be accessed using 'askOption'.
includingOptions :: [OptionDescription] -> Ingredient
includingOptions opts = TestManager opts (\_ _ -> Nothing)
