\connect winter-db

DROP SCHEMA IF EXISTS winter;
CREATE SCHEMA IF NOT EXISTS winter AUTHORIZATION winter;

-- -----------------------------------------------------
-- Table winter.users
-- -----------------------------------------------------
DROP TABLE IF EXISTS winter.users;

CREATE TABLE IF NOT EXISTS winter.users (
  id UUID NOT NULL,
  name TEXT NOT NULL,
  email TEXT NOT NULL,
  PRIMARY KEY (id)
);

CREATE UNIQUE INDEX users_UNIQUE ON winter.users (id ASC);
CREATE UNIQUE INDEX email_UNIQUE ON winter.users (email ASC);

-- -----------------------------------------------------
-- Table winter.groups
-- -----------------------------------------------------
DROP TABLE IF EXISTS winter.groups;

CREATE TABLE IF NOT EXISTS winter.groups (
  id UUID NOT NULL,
  name TEXT NOT NULL,
  description TEXT,
  cost_limit NUMERIC,
  creator_id UUID,
  PRIMARY KEY (id),
  CONSTRAINT fk_groups_users_creator
    FOREIGN KEY (creator_id)
    REFERENCES winter.users (id)
);

CREATE UNIQUE INDEX groups_id_UNIQUE ON winter.groups (id ASC);
CREATE INDEX fk_groups_users_creator_idx ON winter.groups (creator_id ASC);


-- -----------------------------------------------------
-- Table winter.group_members
-- -----------------------------------------------------
DROP TABLE IF EXISTS winter.group_members;

CREATE TABLE IF NOT EXISTS winter.group_members (
  id UUID NOT NULL,
  group_id UUID NOT NULL,
  user_id UUID NOT NULL,
  PRIMARY KEY (id),
  CONSTRAINT fk_group_members_users
    FOREIGN KEY (user_id)
    REFERENCES winter.users (id),
  CONSTRAINT fk_group_members_groups
    FOREIGN KEY (group_id)
    REFERENCES winter.groups (id)
);

CREATE UNIQUE INDEX group_members_id_UNIQUE ON winter.group_members (id ASC);
CREATE INDEX fk_group_members_groups_idx ON winter.group_members (group_id ASC);
CREATE INDEX fk_group_members_users_idx ON winter.group_members (user_id ASC);
