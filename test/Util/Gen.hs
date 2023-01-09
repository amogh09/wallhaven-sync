module Util.Gen (alpha) where

import Test.QuickCheck (Gen, elements, listOf1)

alpha :: Gen String
alpha = listOf1 $ elements ['a' .. 'z']
