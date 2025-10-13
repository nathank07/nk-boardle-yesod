module LoadDb (main) where

import Database.LoadCsv

main :: IO ()
main = Database.LoadCsv.loadCsvFileToPG "src/Database/lichess_db_puzzle.csv" 3 7