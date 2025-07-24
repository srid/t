{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Main.Utf8 qualified as Utf8
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[Message])
page = do
  pure $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"

data Message = Message1 | Message2
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance HyperView Message es where
  data Action Message = Louder Text
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  update (Louder m) = do
    let new = m <> "!"
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  row ~ gap 10 $ do
    button (Louder m) ~ border 1 . pad 5 $ "Louder"
    el ~ pad 5 $ text m

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    putStrLn "Starting Hyperbole: http://localhost:3000"
    run 3000 $ do
      liveApp (basicDocument "t") (runPage page)
