import Control.Applicative
import Control.Monad
import Data.Monoid

main = do
  putStr "What's your name? "
  name <- getLine
  putStrLn $ "Hello " ++ name
