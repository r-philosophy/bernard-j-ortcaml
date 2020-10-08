CREATE TYPE thing_id AS (
    kind smallint,
    id bigint
);

CREATE TABLE users (
    id serial PRIMARY KEY,
    username text UNIQUE
);

CREATE TABLE subreddits (
    id serial PRIMARY KEY,
    display_name text UNIQUE,
    subscribers int
);

CREATE TABLE modmails (
    id serial PRIMARY KEY,
    author integer,
    time timestamp,
    body text,
    subreddit integer,
    FOREIGN KEY (author) REFERENCES users (id),
    FOREIGN KEY (subreddit) REFERENCES subreddits (id)
);

CREATE TABLE actions (
    id serial PRIMARY KEY,
    target thing_id NOT NULL,
    action_summary text,
    author integer,
    moderator integer NOT NULL,
    time timestamp NOT NULL,
    subreddit integer NOT NULL,
    FOREIGN KEY (author) REFERENCES users (id),
    FOREIGN KEY (moderator) REFERENCES users (id),
    FOREIGN KEY (subreddit) REFERENCES subreddits (id)
);

CREATE TABLE subreddit_moderator (
    subreddit_id integer NOT NULL,
    moderator_id integer NOT NULL,
    PRIMARY KEY (subreddit_id, moderator_id),
    FOREIGN KEY (subreddit_id) REFERENCES subreddits (id),
    FOREIGN KEY (moderator_id) REFERENCES users (id)
);

CREATE TABLE contents (
    id thing_id PRIMARY KEY,
    author integer,
    subreddit integer NOT NULL,
    time timestamp NOT NULL,
    json text NOT NULL,
    FOREIGN KEY (author) REFERENCES users (id),
    FOREIGN KEY (subreddit) REFERENCES subreddits (id)
);

