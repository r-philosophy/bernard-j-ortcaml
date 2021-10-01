dropdb testing
createdb testing
psql -d testing -f create_tables.sql
./test_database/main.exe -database testing -link test_database/link.json  -subreddit test_database/subreddit.json
