DROP TABLE IF EXISTS Summary;
CREATE TABLE Summary(
   CustID INTEGER PRIMARY KEY,
   SFCCode DECIMAL(3, 0),
   RetF07Dollars INTEGER NOT NULL,
   RetF07Trips INTEGER NOT NULL,
   RetF07Lines INTEGER NOT NULL,
   RetS07Dollars INTEGER NOT NULL,
   RetS07Trips INTEGER NOT NULL,
   RetS07Lines INTEGER NOT NULL,
   RetF06Dollars INTEGER NOT NULL,
   RetF06Trips INTEGER NOT NULL,
   RetF06Lines INTEGER NOT NULL,
   RetS06Dollars INTEGER NOT NULL,
   RetS06Trips INTEGER NOT NULL,
   RetS06Lines INTEGER NOT NULL,
   RetF05Dollars INTEGER NOT NULL,
   RetF05Trips INTEGER NOT NULL,
   RetF05Lines INTEGER NOT NULL,
   RetS05Dollars INTEGER NOT NULL,
   RetS05Trips INTEGER NOT NULL,
   RetS05Lines INTEGER NOT NULL,
   RetF04Dollars INTEGER NOT NULL,
   RetF04Trips INTEGER NOT NULL,
   RetF04Lines INTEGER NOT NULL,
   RetS04Dollars INTEGER NOT NULL,
   RetS04Trips INTEGER NOT NULL,
   RetS04Lines INTEGER NOT NULL,
   RetPre04Dollars INTEGER NOT NULL,
   RetPre04Trips INTEGER NOT NULL,
   RetPre04Lines INTEGER NOT NULL,
   RetPre04Recency SMALLINT NOT NULL,
   IntF07GDollars INTEGER NOT NULL,
   IntF07NGDollars INTEGER NOT NULL,
   IntF07Orders INTEGER NOT NULL,
   IntF07Lines INTEGER NOT NULL,
   IntS07GDollars INTEGER NOT NULL,
   IntS07NGDollars INTEGER NOT NULL,
   IntS07Orders INTEGER NOT NULL,
   IntS07Lines INTEGER NOT NULL,
   IntF06GDollars INTEGER NOT NULL,
   IntF06NGDollars INTEGER NOT NULL,
   IntF06Orders INTEGER NOT NULL,
   IntF06Lines INTEGER NOT NULL,
   IntS06GDollars INTEGER NOT NULL,
   IntS06NGDollars INTEGER NOT NULL,
   IntS06Orders INTEGER NOT NULL,
   IntS06Lines INTEGER NOT NULL,
   IntF05GDollars INTEGER NOT NULL,
   IntF05NGDollars INTEGER NOT NULL,
   IntF05Orders INTEGER NOT NULL,
   IntF05Lines INTEGER NOT NULL,
   IntS05GDollars INTEGER NOT NULL,
   IntS05NGDollars INTEGER NOT NULL,
   IntS05Orders INTEGER NOT NULL,
   IntS05Lines INTEGER NOT NULL,
   IntF04GDollars INTEGER NOT NULL,
   IntF04NGDollars INTEGER NOT NULL,
   IntF04Orders INTEGER NOT NULL,
   IntF04Lines INTEGER NOT NULL,
   IntS04GDollars INTEGER NOT NULL,
   IntS04NGDollars INTEGER NOT NULL,
   IntS04Orders INTEGER NOT NULL,
   IntS04Lines INTEGER NOT NULL,
   IntPre04GDollars INTEGER NOT NULL,
   IntPre04NGDollars INTEGER NOT NULL,
   IntPre04Orders INTEGER NOT NULL,
   IntPre04Lines INTEGER NOT NULL,
   IntPre04Recency SMALLINT NOT NULL,
   CatF07GDollars INTEGER NOT NULL,
   CatF07NGDollars INTEGER NOT NULL,
   CatF07Orders INTEGER NOT NULL,
   CatF07Lines INTEGER NOT NULL,
   CatS07GDollars INTEGER NOT NULL,
   CatS07NGDollars INTEGER NOT NULL,
   CatS07Orders INTEGER NOT NULL,
   CatS07Lines INTEGER NOT NULL,
   CatF06GDollars INTEGER NOT NULL,
   CatF06NGDollars INTEGER NOT NULL,
   CatF06Orders INTEGER NOT NULL,
   CatF06Lines INTEGER NOT NULL,
   CatS06GDollars INTEGER NOT NULL,
   CatS06NGDollars INTEGER NOT NULL,
   CatS06Orders INTEGER NOT NULL,
   CatS06Lines INTEGER NOT NULL,
   CatF05GDollars INTEGER NOT NULL,
   CatF05NGDollars INTEGER NOT NULL,
   CatF05Orders INTEGER NOT NULL,
   CatF05Lines INTEGER NOT NULL,
   CatS05GDollars INTEGER NOT NULL,
   CatS05NGDollars INTEGER NOT NULL,
   CatS05Orders INTEGER NOT NULL,
   CatS05Lines INTEGER NOT NULL,
   CatF04GDollars INTEGER NOT NULL,
   CatF04NGDollars INTEGER NOT NULL,
   CatF04Orders INTEGER NOT NULL,
   CatF04Lines INTEGER NOT NULL,
   CatS04GDollars INTEGER NOT NULL,
   CatS04NGDollars INTEGER NOT NULL,
   CatS04Orders INTEGER NOT NULL,
   CatS04Lines INTEGER NOT NULL,
   CatPre04GDollars INTEGER NOT NULL,
   CatPre04NGDollars INTEGER NOT NULL,
   CatPre04Orders INTEGER NOT NULL,
   CatPre04Lines INTEGER NOT NULL,
   CatPre04Recency SMALLINT NOT NULL,
   EmailsF07 INTEGER NOT NULL,
   EmailsS07 INTEGER NOT NULL,
   EmailsF06 INTEGER NOT NULL,
   EmailsS06 INTEGER NOT NULL,
   EmailsF05 INTEGER NOT NULL,
   EmailsS05 INTEGER NOT NULL,
   CatCircF07 INTEGER NOT NULL,
   CatCircS07 INTEGER NOT NULL,
   CatCircF06 INTEGER NOT NULL,
   CatCircS06 INTEGER NOT NULL,
   CatCircF05 INTEGER NOT NULL,
   CatCircS05 INTEGER NOT NULL,
   GiftRecF07 INTEGER NOT NULL,
   GiftRecS07 INTEGER NOT NULL,
   GiftRecF06 INTEGER NOT NULL,
   GiftRecS06 INTEGER NOT NULL,
   GiftRecF05 INTEGER NOT NULL,
   GiftRecS05 INTEGER NOT NULL,
   GiftRecF04 INTEGER NOT NULL,
   GiftRecS04 INTEGER NOT NULL,
   GiftRecPre04 INTEGER NOT NULL,
   NewGRF07 INTEGER NOT NULL,
   NewGRS07 INTEGER NOT NULL,
   NewGRF06 INTEGER NOT NULL,
   NewGRS06 INTEGER NOT NULL,
   NewGRF05 INTEGER NOT NULL,
   NewGRS05 INTEGER NOT NULL,
   NewGRF04 INTEGER NOT NULL,
   NewGRS04 INTEGER NOT NULL,
   NewGRPre04 INTEGER NOT NULL,
   FirstYYMM INTEGER NOT NULL,
   FirstChannel CHAR(3),
   FirstDollar INTEGER NOT NULL,
   StoreDist DECIMAL(7, 2) ,
   AcqDate INTEGER NOT NULL,
   Email CHAR(1),
   OccupCd INTEGER,
   Travel CHAR(1),
   CurrAff CHAR(1),
   CurrEv CHAR(1),
   Wines CHAR(1),
   FineArts CHAR(1),
   Exercise CHAR(1),
   SelfHelp CHAR(1),
   Collect CHAR(1),
   Needle CHAR(1),
   Sewing CHAR(1),
   DogOwner CHAR(1),
   CarOwner CHAR(1),
   Cooking CHAR(1),
   Pets CHAR(1),
   Fashion CHAR(1),
   Camping CHAR(1),
   Hunting CHAR(1),
   Boating CHAR(1),
   AgeCode INTEGER,
   IncCode INTEGER,
   HomeCode INTEGER,
   Child0_2 CHAR(1),
   Child3_5 CHAR(1),
   Child6_11 CHAR(1),
   Child12_16 CHAR(1),
   Child17_18 CHAR(1),
   Dwelling INTEGER,
   LengthRes INTEGER,
   HomeValue BIGINT
);

COPY
   Summary
FROM
   'C:\\Users\\seong\\Documents\\00. Summer\\0. Digital Market\\HW1\\Digital Marketing HW1 data set\\DMEFExtractSummaryV01.csv'
WITH
   NULL AS ' '
   DELIMITER ','
   CSV HEADER;



DROP TABLE IF EXISTS contacts;
CREATE TABLE contacts
(
  CustID INTEGER NOT NULL REFERENCES Summary(CustID),
  ContactDate DATE NOT NULL,
  ContactType CHAR(1) NOT NULL
);

COPY
  contacts
FROM
  'C:\\Users\\seong\\Documents\\00. Summer\\0. Digital Market\\HW1\\Digital Marketing HW1 data set\\DMEFExtractContactsV01.csv'
WITH
  NULL AS ' '
  DELIMITER ','
  CSV HEADER;


DROP TABLE IF EXISTS orders;
CREATE TABLE orders(
  CustID INTEGER NOT NULL REFERENCES Summary(CustID),
  OrderNo BIGINT NOT NULL,
  OrderDate DATE("%Y-%m-%d") NOT NULL,
  OrderMethod VARCHAR(2) NOT NULL,
  PaymentType CHAR(2) NOT NULL,
  PRIMARY KEY (CustID, OrderNo, OrderDate)
);

CREATE INDEX ON orders(CustID);

COPY
  orders
FROM
  'C:\\Users\\seong\\Documents\\00. Summer\\0. Digital Market\\HW1\\Digital Marketing HW1 data set\\DMEFExtractOrdersV01.csv'
WITH
  NULL AS ' '
  DELIMITER ','
  CSV HEADER;


DROP TABLE IF EXISTS lines;
CREATE TABLE lines(
  CustID INTEGER NOT NULL,
  OrderNo BIGINT NOT NULL,
  OrderDate DATE NOT NULL,
  LineDollar DECIMAL(9, 2),
  Gift CHAR(1),
  RecipNo INTEGER,
  FOREIGN KEY (CustID, OrderNo, OrderDate) REFERENCES Orders(CustID, OrderNo, OrderDate)
);

CREATE INDEX ON lines(CustID);

COPY
  lines
FROM
  'C:\\Users\\seong\\Documents\\00. Summer\\0. Digital Market\\HW1\\Digital Marketing HW1 data set\\DMEFExtractLinesV01.csv'
WITH
  NULL AS ' '
  DELIMITER ','
  CSV HEADER;