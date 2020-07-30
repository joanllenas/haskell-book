## Intermission: Check your understanding (pg.498)

Here is the import list from one of the modules in a library of Chris’s, called blacktip:

```hs
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Char8 as B
import qualified Data.Locator as DL
import qualified Data.Time.Clock.POSIX as PSX
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info as NI
import qualified Safe
import Control.Exception (mask, try) 
import Control.Monad (forever, when) 
import Data.Bits
import Data.Bits.Bitwise (fromListBE) 
import Data.List.Split (chunksOf)
import Database.Blacktip.Types
import System.IO.Unsafe (unsafePerformIO)
```

### 1. 
What functions are being imported from `Control.Monad`?

---

- `forever`
- `when`

### 2. 
Which imports are both unqualified and imported in their entirety?

---

- `Data.Bits`
- `Database.Blacktip.Types`

### 3. 
From the name, what do you suppose importing the Types module brings in?

---

- `type`, `newtype`, `data`, `class` and `instance` definitions.

### 4. 
Now, let’s compare a small part of blacktip’s code to the above import list:

```hs
writeTimestamp :: MV.MVar ServerState
               -> FPC.FilePath
               -> IO CC.ThreadId 
writeTimestamp s path = do
  CC.forkIO go
  where go = forever $ do
           ss <- MV.readMVar s 
           mask $ \_ -> do
             FS.writeFile path
             (B.pack (show (ssTime ss)))
           -- sleep for 1 second 
           CC.threadDelay 1000000
```
- a) The type signature refers to three aliased imports. What modules are named in those aliases?
- b) Which import does `FS.writeFile` refer to? 
- c) Which import does `forever` come from?

---

#### a)
- `MV`: `Control.Concurrent.MVar`
- `FPC`: `Filesystem.Path.CurrentOS`
- `CC`: `Control.Concurrent`

#### b)
- `import qualified Filesystem as FS`

#### c)
- `import Control.Monad (forever, when) `



## Modifying code (pg.525)

### 1. 
Ciphers: Open your ciphers module, and modify it so that the Caesar and Vigenère ciphers work with user input.

---

```hs
doEncode :: IO ()
doEncode = do
  putStr "Enter the string you want to encode: "
  s <- getLine
  let s' = caesar s
  putStrLn $ "Your encoded string is: " ++ s'

doDecode :: IO ()
doDecode = do
  putStr "Enter the string you want to decode: "
  s <- getLine
  let s' = uncaesar s
  putStrLn $ "Your decoded string is: " ++ s'

main :: IO ()
main = do
  putStr "Do you want to encode or decode? (e/d)"
  c <- getChar
  putStrLn ""
  case c of
    'e' -> doEncode
    'd' -> doDecode
    _ -> main
```

### 2. 
Here is a very simple, short block of code. Notice it has a `forever` that will make it keep running, over and over again. Load it into your REPL, and test it out. Then, refer back to the chapter, and modify it to exit successfully after a `False` result:

```hs
import Control.Monad

palindrome :: IO () 
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!" 
    False -> putStrLn "Nope!"
```

---

```hs
import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO () 
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!" 
    False -> do
      putStrLn "Nope!"
      exitSuccess
```

### 3. 
If you try using `palindrome` on a sentence such as `“Madam I’m Adam,”` you may notice that it doesn’t work. Modifying the above so that it works on sentences, too, involves several steps.

---

```hs
module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import qualified Data.Char as Char

main :: IO () 
main = forever $ do
  line <- getLine
  let line1 = map Char.toLower line
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!" 
    False -> do
      putStrLn "Nope!"
      exitSuccess
```

### 4.

```hs
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid 
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String 
  deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow 
  | otherwise 
      = Left 
      $ PersonInvalidUnknown 
      $ "Name was: " ++ show name 
      ++ " Age was: " ++ show age
````

Your job is to write the following function without modifying the code above:

```hs
gimmePerson :: IO () 
gimmePerson = undefined
```

---

```hs
getResult :: String -> Integer -> String
getResult name age 
  = case mkPerson name age of
    Right p -> "Yay! Successfully got a person: " ++ (show p)
    Left NameEmpty -> "Error: Name can't be an empty string"
    Left AgeTooLow -> "Error: Age must be higher than 0"
    Left (PersonInvalidUnknown s) -> "Unknown Error: " ++ s -- This seems unreachable, becasue `read` throws an exception before getting here

gimmePerson :: IO () 
gimmePerson = do
  putStr "What's your name?"
  name <- getLine
  putStr "What's your age?"
  age <- getLine
  let age' = read age :: Integer
  putStrLn $ getResult name age'
```