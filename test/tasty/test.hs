import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QC

import Control.Comonad
import Control.Comonad.Store.MemoTrie
import Data.Functor.Identity

main :: IO ()
main = defaultMain $ testGroup "Tests"
 [ testGroup "Comonad laws"
  [ testProperty "𝑚 ≡ extract (duplicate 𝑚)"
     $ \m -> (m :: Store Bool Char) === extract (duplicate m)
  , testProperty "𝑚 ≡ fmap extract (duplicate 𝑚)"
     $ \m -> (m :: Store Bool Char) === fmap extract (duplicate m)
  , testProperty "duplicate ∘ duplicate = fmap duplicate ∘ duplicate"
     $ \m -> duplicate (duplicate (m :: Store Bool Char)) === fmap duplicate (duplicate m)
  ]
 ]


