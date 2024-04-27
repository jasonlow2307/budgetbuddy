module Main where

import Control.Applicative
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Time
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Random
import Text.Printf
import Text.Read (readMaybe)

-- Data type to represent currencies
data Currency = RM | USD | SGD | HKD | EUR | GBP deriving (Show)

-- Type aliases for better readibility
type Amount = Double
type AccountNumber = Int

-- Data type to represent date
data Date = Date
  { day :: Int,
    month :: String,
    year :: Int
  }
  deriving (Show)

-- Data type to represent time
data Time = Time
  { hour :: Int,
    minute :: Int,
    seconds :: Int
  }
  deriving (Show)

-- Data type to represent one transaction
data Transaction = Transaction
  { amount :: Amount,
    date :: Date,
    time :: Time,
    transactionType :: TransactionType, 
    account :: Account
  }

-- Data type for transaction type with either income or expense
data TransactionType = Income IncomeType | Expense ExpenseType deriving (Show)

-- Data type for expense type
data ExpenseType = Shopping | Entertainment | Dining | Travel | Gift | Bills | Utilities | Miscellaneous deriving (Show, Eq)

data IncomeType = Salary | Bonus | Other deriving (Show, Eq)

-- Datatype to represent a person
data Person = Person {
  firstName :: String, 
  lastName :: String, 
  age :: Int
} deriving Show

-- Global list to store transactions
allTransactions :: TransactionList
allTransactions = []

-- Record for account
data Account = Account
  { accountNumber :: AccountNumber,
    currency :: Currency,
    owner :: Person,
    balance :: Amount
  }
  deriving (Show)

-- Helper function to read in month number and return month string
monthName :: Int -> String
monthName n = case n of
  1 -> "Jan"
  2 -> "Feb"
  3 -> "Mar"
  4 -> "Apr"
  5 -> "May"
  6 -> "Jun"
  7 -> "Jul"
  8 -> "Aug"
  9 -> "Sep"
  10 -> "Oct"
  11 -> "Nov"
  12 -> "Dec"
  _ -> "Invalid month"

-- Function to format a Date
formatDate :: Date -> String
formatDate (Date day month year) = printf "%02d %s %d" day month year

-- Function to format a Time
formatTimeOfDay :: Time -> String
formatTimeOfDay (Time hour minute seconds) = printf "%02d:%02d:%02d" hour minute seconds

-- Function to create transaction
createTransaction :: Amount -> TransactionType -> TransactionList -> Account -> IO (TransactionList, Transaction)
createTransaction amt transType transactions acc = do
  utcCurrentTime <- getCurrentTime
  let timeZone = hoursToTimeZone 8
      localTime = utcToLocalTime timeZone utcCurrentTime
      (year, month, day) = toGregorian (localDay localTime)
      timeOfDay = localTimeOfDay localTime

      -- Use <*> to combine Maybe
      newTransaction =
        Transaction
          { amount = amt,
            date = Date day (monthName month) (fromIntegral year),
            time = Time (todHour timeOfDay) (todMin timeOfDay) (floor $ todSec timeOfDay),
            transactionType = transType,
            account = acc
          }
      -- Append the new transaction to the global list
      updatedTransactions = newTransaction : transactions
  return (updatedTransactions, newTransaction)

-- Function to show a single transaction
showTransaction :: Transaction -> IO ()
showTransaction trans = do
  putStrLn ""
  putStrLn "========================================="
  putStrLn $ "Account Number: " ++ show (accountNumber $ account trans)
  putStrLn $ "Transaction Type: " ++ show (transactionType trans)
  putStrLn $ "Amount: " ++ show (amount trans)
  putStrLn $ "Date: " ++ formatDate (date trans)
  putStrLn $ "Time: " ++ formatTimeOfDay (time trans)
  putStrLn "========================================="

-- Function to show all transactions
showAllTransactions :: TransactionList -> IO ()
showAllTransactions transactions = do
  putStrLn "Showing all transactions"
  mapM_ showTransaction transactions

  let totalExpense = sum [amount trans | trans <- transactions, isExpense trans]
      totalIncome = sum [amount trans | trans <- transactions, isIncome trans]

  putStrLn ""
  putStrLn $ "Total Expense: " ++ show totalExpense
  putStrLn $ "Total Income: " ++ show totalIncome

-- Helper function to check if a transaction is an expense
isExpense :: Transaction -> Bool
isExpense (Transaction _ _ _ (Expense _) _) = True
isExpense _ = False

-- Helper function to check if a transaction is an income
isIncome :: Transaction -> Bool
isIncome (Transaction _ _ _ (Income _) _) = True
isIncome _ = False

-- Function to view expenses based on categories
viewExpensesByCategory :: TransactionList -> IO ()
viewExpensesByCategory transactions = do
  putStrLn "Choose a category to view transactions:"
  putStrLn "1. Shopping"
  putStrLn "2. Entertainment"
  putStrLn "3. Dining"
  putStrLn "4. Travel"
  putStrLn "5. Gift"
  putStrLn "6. Bills"
  putStrLn "7. Utilities"
  putStrLn "8. Miscellaneous"
  putStrLn "9. All Expenses"
  putStr "Enter choice: "
  categoryChoice <- getLine
  let filteredTransactions = case categoryChoice of
        "1" -> filterTransactionsByCategory Shopping
        "2" -> filterTransactionsByCategory Entertainment
        "3" -> filterTransactionsByCategory Dining
        "4" -> filterTransactionsByCategory Travel
        "5" -> filterTransactionsByCategory Gift
        "6" -> filterTransactionsByCategory Bills
        "7" -> filterTransactionsByCategory Utilities
        "8" -> filterTransactionsByCategory Miscellaneous
        "9" -> transactions -- View all categories
        _ -> []
  showAllTransactions filteredTransactions
  where
    -- Helper function to filter transaction based on category
    filterTransactionsByCategory :: ExpenseType -> TransactionList
    filterTransactionsByCategory category =
      filter
        ( \trans -> case transactionType trans of
            Expense expType -> expType == category
            _ -> False
        )
        transactions

-- Function to view incomes based on categories
viewIncomesByCategory :: TransactionList -> IO ()
viewIncomesByCategory transactions = do
  putStrLn "Choose a category to view transactions: "
  putStrLn "1. Salary"
  putStrLn "2. Bonus"
  putStrLn "3. Others"
  putStrLn "4. All Income"
  putStr "Enter choice: "
  categoryChoice <- getLine
  let filteredTransactions = case categoryChoice of
        "1" -> filterTransactionsByCategory Salary
        "2" -> filterTransactionsByCategory Bonus
        "3" -> filterTransactionsByCategory Other
        "4" -> transactions -- View all categories
        _ -> []
  showAllTransactions filteredTransactions
  where
    -- Helper function to filter transactions based on category
    filterTransactionsByCategory :: IncomeType -> TransactionList
    filterTransactionsByCategory category =
      filter
        ( \trans -> case transactionType trans of
            Income incType -> incType == category
            _ -> False
        )
        transactions

-- Function to analyze transactions
analyzeTransactions :: TransactionList -> IO ()
analyzeTransactions transactions = do
  let totalIncome = sum [amount trans | trans <- transactions, isIncome trans]
      totalExpenses = sum [amount trans | trans <- transactions, isExpense trans]
      -- Rounding to 2 decimal places
      incomeExpenseRatio = fromIntegral (round (totalIncome / totalExpenses * 100)) / fromIntegral 100
      incomeRatio = totalIncome / (totalExpenses + totalIncome)
      expenseRatio = totalExpenses / (totalExpenses + totalIncome)

  putStrLn "=============================================================="
  putStrLn "This is a bar chart comparing your expenses and incomes"

  -- Round the values and represent them using Xs
  let roundAndRepresent value = replicate (round (value * 10)) 'X'

  putStrLn $ "\n Income: " <> roundAndRepresent incomeRatio
  putStrLn $ "Expense: " <> roundAndRepresent expenseRatio <> "\n"
  putStrLn $ "Ratio: " <> show incomeExpenseRatio
  putStrLn $ "============================================================"
  if incomeExpenseRatio < 1 then putStrLn "You are spending more than you are earning, please spend cautiously" else if incomeExpenseRatio > 1 then putStrLn "You are earning more than spending, keep up the great work!" else putStrLn "You are spending and earning equally, keep it up!"
  putStrLn "=============================================================="

-- Function to deduct an amount from the account balance
debit :: Transaction -> Account -> Account
debit transaction acc =
  acc {balance = balance acc + amount transaction}

-- Function to add an amoun to the account balance
credit :: Transaction -> Account -> Account
credit transaction acc =
  acc {balance = balance acc - amount transaction}

-- Define a type synonym for the list of accounts
type AccountList = [Account]
-- Define a type synonym for list of transactions
type TransactionList = [Transaction]

-- Function to generate a random 4-digit account number
generateAccountNumber :: IO Int
generateAccountNumber = getStdRandom (randomR (1000, 9999))

-- Function to create account
createAccount :: AccountList -> IO (AccountList, TransactionList)
createAccount accounts = do
  putStr "Enter owner's first name: "
  firstName <- getLine
  putStr "Enter owner's last name: "
  lastName <- getLine
  putStr "Enter owner's age: "
  age <- getLine
  -- Use liftA3 to create Person using Person data constructor
  let owner = liftA3 Person (Just firstName) (Just lastName) (Just $ readInt age) 
  
  putStrLn "Enter currency (RM, USD, SGD, HKD, EUR, GBP):"
  currencyInput <- getLine
  let maybeCurrency = readCurrency currencyInput

  case maybeCurrency of
    Just currency -> do
      accountNumber <- generateAccountNumber
      putStrLn "Enter opening balance:"
      openingBalanceInput <- getLine
      let openingBalance = readDouble openingBalanceInput

      putStrLn $
        "\n Account created succcessfully for "
          ++ firstName
          ++ " "
          ++ lastName
          ++ " with an initial balance of "
          ++ show openingBalance
          ++ " "
          ++ show currency
          ++ " and account number "
          ++ show accountNumber
          ++ ".\n"
      let newAccount = case owner of 
            Just person -> Account {
              accountNumber = accountNumber,
              owner = person, 
              currency = currency, 
              balance = openingBalance
            }
            Nothing ->
              error "Error: No valid owner"
              
      return (newAccount : accounts, []) -- Return an empty transaction list initially
    Nothing -> do
      putStrLn "Error: Invalid currency. Account not created"
      return (accounts, [])

-- Function to show one account and account details
showAccount :: Account -> IO()
showAccount acc = do
  putStrLn ""
  putStrLn "========================================="
  putStrLn $ "You are viewing account #" <> show (accountNumber acc)
  putStrLn ""
  putStrLn $ "Owner Name: " <> firstName (owner acc)
  putStrLn $ "Balance: " ++ show (currency acc) ++ " " ++ show (balance acc)
  putStrLn "========================================="

-- Function to show all accounts
showAllAccounts :: AccountList -> IO ()
showAllAccounts accounts =
  if null accounts
    then putStrLn "\nNo accounts to display.\n"
    else do
      putStrLn "Showing all accounts"
      mapM_ showAccount accounts

-- Helper function to read currency (case-insensitive)
readCurrency :: String -> Maybe Currency
readCurrency input = case map toLower input of
  "rm" -> Just RM
  "usd" -> Just USD
  "sgd" -> Just SGD
  "hkd" -> Just HKD
  "eur" -> Just EUR
  "gbp" -> Just GBP
  _ -> Nothing

-- Function to add an income transaction
addIncome :: Int -> Double -> AccountList -> TransactionList -> IO (TransactionList, AccountList)
addIncome accNumber incomeAmount accounts transactions = do
  let maybeAccount = findAccountByNumber accNumber accounts
  case maybeAccount of
    Just acc -> do
      putStrLn "Select income type: "
      putStrLn "1. Salary"
      putStrLn "2. Bonus"
      putStrLn "3. Others"
      putStr "Enter Choice: "
      incomeTypeChoice <- getLine
      let incomeType = case readInt incomeTypeChoice of
            1 -> Salary
            2 -> Bonus
            3 -> Other
            _ -> error "Invalid Choice"
      (updatedTransactions, incomeTransaction) <- createTransaction incomeAmount (Income incomeType) transactions acc
      let updatedAcc = debit incomeTransaction acc
      putStrLn $ "\n Successfully added " <> show incomeAmount <> " to " <> firstName (owner updatedAcc)<> "'s account."
      putStrLn $ "Balance: " ++ show (balance updatedAcc) ++ "\n"
      let updatedAccounts = updateAccount updatedAcc accounts
      return (updatedTransactions, updatedAccounts)
    Nothing -> do
      putStrLn "\nAccount not found. \n"
      return (transactions, accounts)

-- Function to add an expense transaction
addExpense :: Int -> Double -> AccountList -> TransactionList -> IO (TransactionList, AccountList)
addExpense accNumber expenseAmount accounts transactions = do
  let maybeAccount = findAccountByNumber accNumber accounts
  case maybeAccount of
    Just acc -> do
      putStrLn "Select expense type:"
      putStrLn "1. Shopping"
      putStrLn "2. Entertainment"
      putStrLn "3. Dining"
      putStrLn "4. Travel"
      putStrLn "5. Gift"
      putStrLn "6. Bills"
      putStrLn "7. Utilities"
      putStrLn "8. Miscellaneous"
      putStrLn "Enter choice: "
      expenseTypeChoice <- getLine
      let expenseType = case readInt expenseTypeChoice of
            1 -> Shopping
            2 -> Entertainment
            3 -> Dining
            4 -> Travel
            5 -> Gift
            6 -> Bills
            7 -> Utilities
            8 -> Miscellaneous
            _ -> error "Invalid Choice"
      (updatedTransactions, expenseTransaction) <- createTransaction expenseAmount (Expense expenseType) transactions acc
      let updatedAcc = credit expenseTransaction acc
      if balance updatedAcc < 0
        then do
          putStrLn "Error: Insufficient funds. Expense cannot be added."
          putStrLn $ "Balance: " ++ show (balance acc) ++ "\n"
          return (transactions, accounts)
        else do
          putStrLn $ "\nSuccessfully added " <> show expenseAmount <> " as " <> show expenseType <> " expenses to " <> firstName (owner updatedAcc) <> "'s account."
          putStrLn $ "Balance: " <> show (balance updatedAcc) <> "\n"
          let updatedAccounts = updateAccount updatedAcc accounts
          return (updatedTransactions, updatedAccounts)
    Nothing -> do
      putStrLn "\nAccount not found.\n"
      return (transactions, accounts)

-- Function to handle Add Transaction submenu
addTransactionMenu :: AccountList -> TransactionList -> IO ()
addTransactionMenu accounts transactions = do
  putStrLn "Choose transaction type:"
  putStrLn "1. Add Income"
  putStrLn "2. Add Expense"
  putStr "Enter choice: "
  transactionChoice <- getLine
  case transactionChoice of
    "1" -> do
      putStrLn "Select an account to add income:"
      showAllAccounts accounts
      putStr "Enter account number: "
      accNumberInput <- getLine
      let accNumber = readInt accNumberInput
      putStr "Enter Income Amount: "
      incomeAmountInput <- getLine
      let incomeAmount = readDouble incomeAmountInput
      (updatedTransactions, updatedAccounts) <- addIncome accNumber incomeAmount accounts transactions
      mainMenu updatedAccounts updatedTransactions
    "2" -> do
      putStrLn "Select an account to add expenses:"
      showAllAccounts accounts
      putStr "Enter account number: "
      accNumberInput <- getLine
      let accNumber = readInt accNumberInput
      putStr "Enter expense amount: "
      expenseAmountInput <- getLine
      let expenseAmount = readDouble expenseAmountInput
      (updatedTransactions, updatedAccounts) <- addExpense accNumber expenseAmount accounts transactions
      mainMenu updatedAccounts updatedTransactions
    _ -> putStrLn "\nInvalid choice\n" >> mainMenu accounts transactions

-- Helper function to read a String and convert to Double
readDouble :: String -> Double
readDouble str =
  case readMaybe str of
    Just value -> value
    Nothing -> error "Invalid input. Please enter a valid number."

-- Helper function to read a String and convert to Int
readInt :: String -> Int
readInt str =
  case readMaybe str of
    Just value -> value
    Nothing -> error "invalid input. Please enter a valid number."

-- Main Menu Function
mainMenu :: AccountList -> TransactionList -> IO ()
mainMenu accounts transactions = do
  putStrLn "\nThis is the main menu for Budget Buddy\n"
  putStrLn "1. Create Account"
  putStrLn "2. Show All Accounts"
  putStrLn "3. Add Transaction"
  putStrLn "4. Show Transactions"
  putStrLn "5. Show Analysis"
  putStrLn "0. Exit Program"
  putStrLn ""
  putStr "Choice: "
  choice <- getLine
  case choice of
    "1" -> do
      (updatedAccounts, _) <- createAccount accounts
      mainMenu updatedAccounts transactions
    "2" -> do
      showAllAccounts accounts
      mainMenu accounts transactions
    "3" -> addTransactionMenu accounts transactions
    "4" -> do
      putStrLn "Choose an option to view records:"
      putStrLn "1. Show All Transactions"
      putStrLn "2. View Transactions based on categories"
      putStr "Enter choice: "
      recordsChoice <- getLine
      case recordsChoice of
        "1" -> do
          showAllTransactions transactions
          mainMenu accounts transactions
        "2" -> do
          putStrLn "Choose the crecord type:"
          putStrLn "1. Expenses"
          putStrLn "2. Incomes"
          putStr "Enter choice: "
          recordTypeChoice <- getLine
          case recordTypeChoice of
            "1" -> do
              let expenseTransactions =
                    filter
                      ( \trans -> case transactionType trans of
                          Expense _ -> True
                          _ -> False
                      )
                      transactions
              viewExpensesByCategory expenseTransactions
              mainMenu accounts transactions
            "2" -> do
              let incomeTransactions =
                    filter
                      ( \trans -> case transactionType trans of
                          Income _ -> True
                          _ -> False
                      )
                      transactions
              viewIncomesByCategory incomeTransactions
              mainMenu accounts transactions
            _ -> putStrLn "\nInvalid choice\n" >> mainMenu accounts transactions
        _ -> putStrLn "\nInvalid choice\n" >> mainMenu accounts transactions
    "5" -> do
      putStrLn $ "\nBased on the " ++ show (length transactions) ++ " transactions, the breakdown is shown below"
      analyzeTransactions transactions >> mainMenu accounts transactions
    "0" -> putStrLn "\nProgram End\n"
    _ -> putStrLn "\nInput Error\n" >> mainMenu accounts transactions

-- Function to find an account by account number
findAccountByNumber :: Int -> AccountList -> Maybe Account
findAccountByNumber targetAccountNumber accounts =
  -- Use function composition (.) to combine the functions
  listToMaybe . filter (\acc -> targetAccountNumber == accountNumber acc) $ accounts

-- Helpter function to update an account in the list
updateAccount :: Account -> AccountList -> AccountList
updateAccount updatedAcc accounts = map (\acc -> if firstName (owner acc) == firstName (owner updatedAcc) then updatedAcc else acc) accounts

-- Main method
main :: IO ()
main = do
  putStrLn "========================================="
  putStrLn "Welcome to Budget Buddy"
  putStrLn "Your command-line companion for tracking exenses, managaing budget and gaining financial insights"
  putStrLn "========================================="
  putStrLn ""
  mainMenu [] [] -- Pass empty lists for accounts and transactions initially
