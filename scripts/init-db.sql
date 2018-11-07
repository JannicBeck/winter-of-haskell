-- SET SUPERUSER PASSWORD

ALTER USER postgres WITH PASSWORD 'postgres';

-- Role: winter

-- DROP ROLE winter;

CREATE ROLE winter LOGIN
  PASSWORD 'winter'
  NOSUPERUSER;

-- Database: "winter-db"

-- DROP DATABASE "winter-db";

CREATE DATABASE "winter-db"
  WITH OWNER = winter
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       LC_COLLATE = 'en_US.utf8'
       LC_CTYPE = 'en_US.utf8'
       CONNECTION LIMIT = -1;

