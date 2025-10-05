CREATE TABLE IF NOT EXISTS Puzzles (
    id TEXT PRIMARY KEY,
    fen TEXT NOT NULL,
    uci_solution TEXT NOT NULL,
    rating INTEGER NOT NULL,
    rating_deviation INTEGER NOT NULL,
    popularity INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS Themes
(
    id SERIAL PRIMARY KEY,
    name TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS Puzzle_Themes
(
    puzzle_id TEXT REFERENCES Puzzles(id),
    theme_id INTEGER REFERENCES Themes(id),
    PRIMARY KEY (puzzle_id, theme_id)
);