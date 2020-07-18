CREATE TABLE sexes (
  id     INTEGER PRIMARY KEY,
  string text UNIQUE NOT NULL,
  CONSTRAINT gender_limit CHECK (id <= 4),
  CONSTRAINT gender_description CHECK (string = 'male' or string = 'female' or string = 'intersexed' or string = 'not stated')
);

INSERT INTO sexes
  (id, string)
VALUES
  (1, 'male'),
  (2, 'female'),
  (3, 'intersexed'),
  (4, 'not stated');

CREATE TABLE humans (
  id         INTEGER PRIMARY KEY,
  forename   text NOT NULL,
  surname    text NOT NULL,
  birthdate  date NOT NULL,
  sex_id     integer NOT NULL REFERENCES sexes(id),
  CONSTRAINT sex_id_quad CHECK (sex_id = 1 OR sex_id = 2 OR sex_id = 3 OR sex_id = 4)
);

INSERT INTO humans
  (id, forename, surname, birthdate, sex_id)
VALUES
  (1001, 'Ann', 'Kaspersky', '1985-02-08' , 2),
  (1002, 'Mark', 'Harvey', '1986-02-08' , 1),
  (1003, 'Diane', 'Schwartz', '1985-12-08' , 1),
  (1004, 'Julio', 'Rodriguez', '1982-02-18' , 2),
  (1005, 'Zena', 'Shim', '1985-12-28' , 3),
  (1006, 'SAM', 'Otry', '1987-02-08', 4);

CREATE TABLE marriages (
  id             SERIAL PRIMARY KEY,
  partner_1_id   INTEGER NOT NULL REFERENCES humans(id),
  partner_2_id   INTEGER NOT NULL REFERENCES humans(id),
  marriage_date  date NOT NULL,
  divorce_date   date DEFAULT NULL,
  CONSTRAINT date_mar_div CHECK (marriage_date < divorce_date),
  CONSTRAINT no_selfmarriage CHECK (partner_1_id != partner_2_id),
  CONSTRAINT marriage_duplication UNIQUE (partner_1_id, partner_2_id, marriage_date)
);

INSERT INTO marriages
  (partner_1_id, partner_2_id, marriage_date, divorce_date)
VALUES
  (1001, 1002, '2010-02-10', NULL),
  (1003, 1005, '2011-04-15', NULL),
  (1004, 1006, '2013-09-19', '2015-03-19');


CREATE VIEW married_people AS (
SELECT
marriage_date,
humans.forename AS Partner1_forename,
humans.surname AS Partner1_surname,
sexes.string AS Partner1_sex,
humans.birthdate AS Partner1_birth,
humans1.forename AS partner2_forename,
humans1.surname AS Partner2_surname,
sexes1.string AS Partner2_sex,
humans1.birthdate AS Partner2_birth
FROM marriages
INNER JOIN humans ON marriages.partner_1_id = humans.id
INNER JOIN humans AS humans1 ON marriages.partner_2_id = humans1.id
INNER JOIN sexes ON humans.sex_id = sexes.id
INNER JOIN sexes AS sexes1 ON humans.sex_id = sexes1.id
WHERE marriages.divorce_date IS NULL
);
