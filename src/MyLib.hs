{-# language OverloadedStrings #-}
module MyLib where


-- Tried to make as simple as possible.
-- Realistically I would:
--     - use lens to make nested updates cleaner
--     - use a relational database instead of Map, to maintain denormalization,
--       interface with external data, etc. I'm REALLY a fan of relational DB for tasks like this.
--       For anything except the simplest examples it quickly pays itself back in terms of
--       constraint enforcement, data integrity, and ad-hoc queryability.
--     - use a proper effects system instead of just the maybe monad so that:
--        - meaningful error messages can be returned (interleaving either with maybe for errors is annoying)
--        - code for database access can be included simply without clutter or stray IO
--        - logging for record keeping/audit trail etc. Also useful to reuse for the recommendation engine.
--     - include a proper test suite, with property tests, etc. in eg doctest for the simple data
--             and unit tests for the database accesses

import Data.Map (Map)
import Data.Map qualified as M

import Data.Text (Text)
import Data.Text qualified as T

import Data.ISBN.Types (ISBN(..))

deriving instance Ord ISBN

-- | Metadata for an author (TODO: could be more)
newtype Author = Author Text deriving (Show,Eq)

-- | metadata for a book.
data Book = Book { title :: Text, author :: Author, isbn :: ISBN } deriving (Show, Eq)

-- | A unique identifier for patrons membership.
newtype LibraryCardNum = LibraryCardNum Text deriving (Show,Eq,Ord)

data Patron = Patron { name :: Text, libraryCard :: LibraryCardNum, primaryBranch :: Branch }
  deriving (Show, Eq)

-- | The name of a branch of the library.
newtype Branch = Branch Text deriving (Show,Eq,Ord)

-- | The record of books checked out by patrons.
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

-- | Add a book to a branch's inventory - registering it (for convenience) if it's not already known.
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

checkoutBookPrimaryBranch :: ISBN -> LibraryCardNum -> Library -> Maybe Library
checkoutBookPrimaryBranch isbn cardNum lib = do
  patron <- M.lookup cardNum (patrons lib)
  checkoutBook isbn (primaryBranch patron) cardNum lib
returnBookPrimaryBranch :: ISBN -> LibraryCardNum -> Library -> Maybe Library
returnBookPrimaryBranch isbn cardNum lib = do
  patron <- M.lookup cardNum (patrons lib)
  returnBook isbn (primaryBranch patron) cardNum lib

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
examplePatron = Patron { name = "John Doe", libraryCard = exampleCardNum , primaryBranch = Branch "1"}
exampleCardNum = LibraryCardNum "1234567890"

-- | Transfer from b1 to b2
transferBook :: ISBN -> Branch -> Branch -> Library -> Maybe Library
transferBook isbn b1 b2 lib = do
  _ <- M.lookup isbn (knownBooks lib) -- check that the book is known
  availableBooks' <- unstock (isbn,b1) (availableBooks lib)
  pure $ lib { availableBooks = M.alter (Just . maybe 1 (+1)) (isbn,b2) availableBooks' }

exampleLib :: Library
exampleLib = case (do
  let l1 = registerPatron examplePatron emptyLib
  l2 <- addBook exampleBook (Branch "1") l1
  l3 <- addBook exampleBook (Branch "2") l2
  l4 <- checkoutBookPrimaryBranch exampleISBN exampleCardNum l3
  l5 <- transferBook exampleISBN (Branch "2") (Branch "1") l4
  l6 <- checkoutBookPrimaryBranch exampleISBN exampleCardNum l5
  l7 <- returnBookPrimaryBranch exampleISBN exampleCardNum l6
  Just l7) of Just l -> l


-- reservation system:
-- reservations are indistiguishable from removed books for the purpose of availability.
-- if we also want to track inventory, we can simply add a seperate tracker for physical inventory,
-- and a seperate tracker for "reservations" on patrons.
-- adding a new method "reserve" which adds a reservation to a patron's reservation tracker, and removes availability from the library as normal, but without updating the inventory.


-- book recommendation system:
-- include a history of all book checkouts, not just the current state
-- optionally, include a rating system for returned books.
-- use this data to generate a recommendation (somehow) - exposed via a new function "recommendBook" which takes a patron and a library and returns an IO action for a book list.
-- IO will need to be handled for this which will require a proper effects system.
