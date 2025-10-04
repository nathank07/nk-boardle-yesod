CREATE TABLE IF NOT EXISTS Puzzles (
    id TEXT PRIMARY KEY,
    fen TEXT NOT NULL,
    uci_solution TEXT[] NOT NULL,
    rating INTEGER NOT NULL,
    rating_deviation INTEGER NOT NULL,
    popularity INTEGER NOT NULL,
    themes TEXT[]
);