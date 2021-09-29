CREATE TYPE thing_id AS (
    kind smallint,
    id bigint
);

CREATE COLLATION case_insensitive (
    provider = icu,
    locale = 'und-u-ks-level2',
    deterministic = FALSE
);

CREATE TABLE users (
    id serial PRIMARY KEY,
    username text UNIQUE COLLATE case_insensitive
);

CREATE TABLE subreddits (
    id integer PRIMARY KEY,
    display_name text UNIQUE COLLATE case_insensitive,
    subscribers integer
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

CREATE OR REPLACE VIEW vw_actions AS
SELECT
    action.id,
    action.target,
    action.action_summary,
    author.username AS author,
    moderator.username AS moderator,
    action. "time",
    subreddits.display_name AS subreddit
FROM
    actions action
    LEFT JOIN users author ON author.id = action.author
    LEFT JOIN users moderator ON moderator.id = action.moderator
    LEFT JOIN subreddits ON subreddits.id = action.subreddit;

CREATE OR REPLACE VIEW public.vw_contents AS
SELECT
    contents.id,
    users.username AS author,
    subreddits.display_name AS subreddit,
    contents. "time",
    contents.json
FROM
    contents
    LEFT JOIN users ON users.id = contents.author
    LEFT JOIN subreddits ON subreddits.id = contents.subreddit;

CREATE OR REPLACE VIEW public.vw_subreddit_moderator AS
SELECT
    subreddits.display_name AS subreddit,
    users.username AS moderator
FROM
    subreddit_moderator
    LEFT JOIN subreddits ON subreddit_moderator.subreddit_id = subreddits.id
    LEFT JOIN users ON subreddit_moderator.moderator_id = users.id;

