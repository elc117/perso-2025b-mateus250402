CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    password TEXT NOT NULL
);

CREATE TABLE games (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    title TEXT NOT NULL,
    score REAL,
    platform TEXT NOT NULL,
    cover_url TEXT,
    FOREIGN KEY(user_id) REFERENCES users(id)
);
