CREATE TABLE sexes (
  id     serial PRIMARY KEY,
  string text   NOT NULL
);

CREATE TABLE humans (
  id        serial  PRIMARY KEY,
  forename  text    NOT NULL,
  surname   text    NOT NULL,
  birthdate date    NOT NULL,
  sex_id    integer REFERENCES sexes(id)
);

CREATE TABLE marriages (
  id            serial    PRIMARY KEY,
  partner_1_id  integer   NOT NULL REFERENCES humans(id),
  partner_2_id  integer   NOT NULL REFERENCES humans(id),
  marriage_date date      NOT NULL,
  divorce_date  date
  CONSTRAINT unique_order CHECK (partner_1_id < partner_2_id)
  CONSTRAINT dates        CHECK (marriage_date <= divorce_date)
);


INSERT INTO sexes
 (string)
VALUES
 ('female'),
 ('male'),
 ('intersexed'),
 ('not stated');

INSERT INTO humans
 (forename, surname, birthdate, sex_id)
VALUES
 ('Victoria','Cohen', '1958/01/26', 1),
 ('Maxine', 'Smith',' 1973/01/31', 1),
 ('Conrad', 'Miner', '1973/06/15', 2),
 ('David', 'Eco', '1975/05/29', 2),
 ('Ellen', 'Floyd', '1974/04/17', 1),
 ('Jonas', 'Floyd', '1975/05/02', 2),
 ('Amelia', 'Muller', '1975/06/04', 1),
 ('David', 'Bergstrom', '1963/12/18', 2);

INSERT INTO marriages
 (partner_1_id, partner_2_id, marriage_date, divorce_date)
VALUES
 (1, 2, '2008/08/16', NULL),
 (5, 6, '1999/07/04', NULL),
 (7, 8, '2014/08/23', '2017/09/19');

CREATE VIEW married_people AS (
  SELECT
    h1.forename  AS partner_1_forename,
    h1.surname   AS partner_1_surname,
    h2.forename  AS partner_2_forename,
    h2.surname   AS partner_2_surname,
    h1.birthdate AS partner_1_birthdate,
    h2.birthdate AS partner_2_birthdate,
    hs1.string   AS partner_1_sex,
    hs2.string   AS partner_2_sex,
    marriages.marriage_date
  FROM marriages
  INNER JOIN humans AS h1 ON marriages.partner_1_id = h1.id
  INNER JOIN humans AS h2 ON marriages.partner_2_id = h2.id
  INNER JOIN sexes AS hs1 ON h1.sex_id = hs1.id
  INNER JOIN sexes AS hs2 ON h2.sex_id = sexjoin2.id
  WHERE marriages.divorce_date IS NULL
);
