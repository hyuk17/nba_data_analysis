DROP TABLE IF EXISTS games_details;
CREATE TABLE games_details(
	GAME_ID					bigint,
	TEAM_ID					bigint,
	TEAM_ABBREVIATION		varchar(255),
	TEAM_CITY				varchar(255),
	PLAYER_ID				bigint,
	PLAYER_NAME				varchar(255),
	START_POSITION			varchar(255),
	COMMENT					varchar(255),
	MIN						varchar(255),
	FGM						numeric,
	FGA						numeric,
	FG_PCT					numeric,
	FG3M					numeric,
	FG3A					numeric,
	FG3_PCT					numeric,
	FTM						numeric,
	FTA						numeric,
	FT_PCT					numeric,
	OREB					numeric,
	DREB					numeric,
	REB						numeric,
	AST						numeric,
	STL						numeric,
	BLK						numeric,
	T						numeric,
	PF						numeric,
	PTS						numeric,
	plus_minus				numeric
);
DROP TABLE IF EXISTS games;
CREATE TABLE games(
	GAME_DATE_EST			date,
	GAME_ID					bigint,
	GAME_STATUS_TEXT		varchar(255),
	HOME_TEAM_ID			bigint,
	VISITOR_TEAM_ID			bigint,
	SEASON					int,
	TEAM_ID_home			bigint,
	PTS_home				numeric,
	FG_PCT_home				numeric,
	FT_PCT_home				numeric,
	FG3_PCT_home			numeric,
	AST_home				numeric,
	REB_home				numeric,
	TEAM_ID_away			numeric,
	PTS_away				numeric,
	FG_PCT_away				numeric,
	FT_PCT_away				numeric,
	FG3_PCT_away			numeric,
	AST_away				numeric,
	REB_away				numeric,
	HOME_TEAM_WINS			numeric
);
DROP TABLE IF EXISTS teams;
CREATE TABLE teams(
	LEAGUE_ID				varchar(255),
	TEAM_ID					bigint PRIMARY KEY,
	MIN_YEAR				int,
	MAX_YEAR				int,
	ABBREVIATION			varchar(255),
	NICKNAME				varchar(255),
	YEARFOUNDED				int,
	CITY					varchar(255),
	ARENA					varchar(255),
	ARENACAPACITY			int,
	OWNER					varchar(255),
	GENERALMANAGER			varchar(255),
	HEADCOACH				varchar(255),
	DLEAGUEAFFILIATION		varchar(255)
);

--Data cleaning for teams table

--remove unnecessary columns
Alter Table teams
--league_id serves no purpose. 
DROP COLUMN league_id,
--Min_year column is identical to yearfounded.
DROP COLUMN min_year;

--renaming column
Alter Table teams
--dleagueaffiliation refers to minor league for NBA.
--The current name is G League.
RENAME COLUMN dleagueaffiliation TO g_league;

--editing missing or incorrect values
--city column
UPDATE teams
--The Warriors play in San Francisco
SET city = 'San Francisco'
WHERE  team_id = 1610612744;

--arenacapacity column. Used google search.
UPDATE teams SET arenacapacity = 16867
WHERE arena = 'Smoothie King Center';
UPDATE teams SET arenacapacity = 17732
WHERE arena = 'Barclays Center';
UPDATE teams SET arenacapacity = 18846
WHERE arena = 'Amway Center';
UPDATE teams SET arenacapacity = 20478
WHERE arena = 'Wells Fargo Center';
UPDATE teams SET arena = 'Footprint Center'
WHERE city = 'Phoenix';
UPDATE teams SET arenacapacity = 18055
WHERE arena = 'Footprint Center';

--data cleaning for games table

--drop unnecessary columns
ALTER TABLE games
--game_status is always final
DROP COLUMN GAME_STATUS_TEXT,
--identical to team_id_home and away
DROP COLUMN HOME_TEAM_ID,
DROP COLUMN VISITOR_TEAM_ID;


--check for duplicates
WITH dup as (
SELECT *,
	--for each game date and id, give a row number
	row_number() OVER(PARTITION BY game_date_est, game_id)
FROM games
)
SELECT count(*)
FROM dup
--if the row number is greater than 1, it means duplicate
WHERE row_number > 1;
--the count function returns 55
--the duplicates are caused by numbers rounded differently
--first, give unique id numbers to each row using serial
ALTER TABLE games 
ADD COLUMN id SERIAL;
--then delete duplicates
DELETE FROM games a
USING games b 
--the serial number must be different
WHERE a.id < b.id
--while game_id and game_date are the same
AND a.game_id = b.game_id
AND a.game_date_est = b.game_date_est;
--lastly, drop the id column. 
ALTER TABLE games
DROP COLUMN id;

--polish it by setting decimal places to 3.
UPDATE games
SET FG_PCT_home = ROUND(FG_PCT_home,3);
UPDATE games
SET FT_PCT_home = ROUND(FT_PCT_home,3);
UPDATE games
SET FG3_PCT_home = ROUND(FG3_PCT_home,3);
UPDATE games
SET FG_PCT_away = ROUND(FG_PCT_away,3);
UPDATE games
SET FT_PCT_away = ROUND(FT_PCT_away,3);
UPDATE games
SET FG3_PCT_away = ROUND(FG3_PCT_away,3);

--data cleaning for games_details table

--start with renaming columns 
ALTER TABLE games_details
--the way shown in nba traditional stats
RENAME COLUMN FG3M to "3pm";
ALTER TABLE games_details
RENAME COLUMN FG3A to "3pa";
ALTER TABLE games_details
RENAME COLUMN FG3_PCT to "3p_pct";
ALTER TABLE games_details
RENAME COLUMN PF to fouls;
ALTER TABLE games_details
RENAME COLUMN T to tov;

--team_city needs correction
--for example, both los angeles and la being present
UPDATE games_details
--will join from teams table.
SET team_city = city
FROM teams
WHERE games_details.team_id = teams.team_id;

--check for duplicates
WITH duplicate as (
SELECT *,
	row_number() OVER(PARTITION BY game_id, player_id)
	FROM games_details
)
SELECT count(*)
FROM duplicate
WHERE row_number > 1;
--we get 324 duplicates. let's remove dupliates.
--repeat the earlier process
ALTER TABLE games_details 
ADD COLUMN id SERIAL;
--here, game_id and player_id should equal
DELETE FROM games_details a
USING games_details b 
WHERE a.id < b.id
AND a.game_id = b.game_id
AND a.player_id = b.player_id;
ALTER TABLE games_details
DROP COLUMN id;
