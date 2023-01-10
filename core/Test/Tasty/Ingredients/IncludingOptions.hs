-- | Ingredient for registering user-defined options
module Test.Tasty.Ingredients.IncludingOptions where

import Test.Tasty.Ingredients
import Test.Tasty.Options

-- | This ingredient doesn't do anything apart from registering additional
-- options.
--
-- The option values can be accessed using 'Test.Tasty.askOption'.
--
-- @since 0.6
includingOptions :: [OptionDescription] -> Ingredient
includingOptions opts = TestManager opts (\_ _ -> Nothing)
