module RetrySpec (spec) where

import Control.Monad (void)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT (runStateT), get, modify)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.Either (isLeft)
import Retry
import Test.Hspec
import qualified Util.List as List

newtype TestApp m a = TestApp
  { unTestApp :: StateT Int (WriterT [String] (ReaderT RetryConfig m)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader RetryConfig,
      MonadWriter [String],
      MonadState Int
    )

runTestApp ::
  Int -> RetryConfig -> TestApp m a -> m ((a, Int), [String])
runTestApp s r =
  flip runReaderT r
    . runWriterT
    . flip runStateT s
    . unTestApp

instance Monad m => CapabilityThreadDelay (TestApp m) where
  threadDelay x = void $ tell ["delay " <> show x]

spec :: Spec
spec = do
  describe "retryM" $ do
    it "doesn't retry if the action succeeds" $ do
      let action = tell ["action"] >> modify (+ 1) >> pure 1
      ((res, attempts), logs) <-
        runTestApp 0 (RetryConfig 3 1000) (retryM (const False) action)
      res `shouldBe` (1 :: Int)
      logs `shouldBe` ["action"]
      attempts `shouldBe` 1

    it "exhausts all attempts if action fails each time" $ do
      let action = tell ["action"] >> modify (+ 1) >> pure (Left "error")
      ((res, attempts), logs) <-
        runTestApp 0 (RetryConfig 10 1000) (retryM isLeft action)
      res `shouldBe` (Left "error" :: Either String Int)
      shouldBe
        logs
        (take 19 (repeat "action" `List.interleave` repeat "delay 1000"))
      attempts `shouldBe` 10

    it "stops retrying when the action succeeds" $ do
      let action = do
            tell ["action"]
            modify (+ 1)
            attempts <- get
            pure $ if attempts < 5 then Left "error" else Right 1
      ((res, attempts), logs) <-
        runTestApp 0 (RetryConfig 10 1000) (retryM isLeft action)
      res `shouldBe` (Right 1 :: Either String Int)
      shouldBe
        logs
        (take 9 (repeat "action" `List.interleave` repeat "delay 1000"))
      attempts `shouldBe` 5
