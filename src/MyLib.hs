{-# language OverloadedStrings #-}
module MyLib where


-- Tried to make as simple as possible.
-- Realistically I would:
--     - use lens to make nested updates cleaner
--     - use a relational database instead of Map, to maintain denormalization,
--       interface with external data, etc.

import Data.Map (Map)
import Data.Map qualified as M

import Data.Text (Text)
import Data.Text qualified as T

import Data.ISBN.Types (ISBN(..))

deriving instance Ord ISBN

newtype Author = Author Text
    deriving (Show,Eq)

data Book = Book { title :: Text, author :: Author, isbn :: ISBN }
  deriving (Show, Eq)

newtype LibraryCardNum = LibraryCardNum Text
    deriving (Show,Eq,Ord)

data Patron = Patron { name :: Text, libraryCard :: LibraryCardNum}
  deriving (Show, Eq)

newtype Branch = Branch Text deriving (Show,Eq,Ord)

type PatronBooks = Map LibraryCardNum (Map (ISBN, Branch) Word)

-- | Library is a record of available books and books checked out by patrons
data Library = Library {knownBooks :: Map ISBN Book
                       ,availableBooks :: Map (ISBN, Branch) Word
                       ,patrons :: Map LibraryCardNum Patron
                       ,checkedOutBooks :: PatronBooks}
    deriving (Show, Eq)

-- | Register a book to be known by the library. Overwrites any existing book
registerBook :: Book -> Library -> Library
registerBook book lib = lib { knownBooks = M.insert (isbn book) book (knownBooks lib) }

-- | Register a patron to be known by the library. Overwrites any existing patron
registerPatron :: Patron -> Library -> Library
registerPatron patron lib = lib { patrons = M.insert (libraryCard patron) patron (patrons lib) }

exampleBook :: Book
exampleBook = Book { title = "The Book of Programming", author = Author "The Programmer", isbn = exampleISBN}
exampleISBN = ISBN10 "978-3-16-148410-0"


emptyLib :: Library
emptyLib = Library { knownBooks = M.empty, availableBooks = M.empty, patrons = M.empty, checkedOutBooks = M.empty }

data BookMismatch = BookMismatch { expected :: Book, actual :: Book }
    deriving (Show, Eq)

addBook :: Book -> Branch -> Library -> Maybe Library
addBook book branch lib
  = let registeredBook = M.lookup (isbn book) (knownBooks lib)
    in case registeredBook of
         Nothing -> Just $ stockBook (registerBook book lib)
         Just book' | book == book' -> Just $ stockBook lib -- already registered
                    | otherwise -> Nothing
  where stockBook l = l {availableBooks = M.alter (Just . maybe 1 (+1)) (isbn book, branch) (availableBooks l)}

-- | Decrement the number of available copies of a book or remove it if there would be none left.
-- Fails if the book isn't available.
unstock :: (Ord k, Num a, Eq a) => k -> Map k a -> Maybe (Map k a)
unstock isbn m = case M.lookup isbn m of
                     Nothing -> Nothing
                     Just 1 -> Just $ M.delete isbn m
                     Just n -> Just $ M.insert isbn (n-1) m

unstockBook :: ISBN -> Branch -> Library -> Maybe Library
unstockBook isbn branch lib = do
  availableBooks' <- unstock (isbn,branch) (availableBooks lib)
  pure $ lib { availableBooks = availableBooks' }

-- | Check out a book to a patron.
-- Fails if the book isn't available or the patron doesn't exist.
checkoutBook :: ISBN -> Branch -> LibraryCardNum -> Library -> Maybe Library
checkoutBook isbn branch cardNum lib = do
  _ <- M.lookup isbn (knownBooks lib) -- check that the book is known
  _ <- M.lookup cardNum (patrons lib) -- ceck that the patron is known
  availableBooks' <- unstock (isbn,branch) (availableBooks lib)
  pure $ lib { availableBooks = availableBooks'
             , checkedOutBooks = M.alter (Just . maybe (M.singleton (isbn,branch) 1) (M.adjust (+1) (isbn,branch))) cardNum (checkedOutBooks lib)
             }

returnBook :: ISBN -> Branch -> LibraryCardNum -> Library -> Maybe Library
returnBook isbn branch cardNum lib = do
  _ <- M.lookup isbn (knownBooks lib) -- check that the book is known
  _ <- M.lookup cardNum (patrons lib) -- ceck that the patron is known
  patronBooks <- M.lookup cardNum (checkedOutBooks lib)
  patronBooks' <- unstock (isbn,branch) patronBooks
  pure $ lib { checkedOutBooks = M.insert cardNum patronBooks' (checkedOutBooks lib)
             , availableBooks = M.alter (Just . maybe 1 (+1)) (isbn,branch) (availableBooks lib)
             }


examplePatron :: Patron
examplePatron = Patron { name = "John Doe", libraryCard = exampleCardNum }
exampleCardNum = LibraryCardNum "1234567890"

exampleLib :: Library
exampleLib = case (do
  let l1 = registerPatron examplePatron emptyLib
  l2 <- addBook exampleBook (Branch "1") l1
  l3 <- addBook exampleBook (Branch "1") l2
  l4 <- checkoutBook exampleISBN (Branch "1") exampleCardNum l3
  l5 <- checkoutBook exampleISBN (Branch "1") exampleCardNum l4
  l6 <- returnBook exampleISBN (Branch "1") exampleCardNum l5
  Just l6) of Just l -> l

