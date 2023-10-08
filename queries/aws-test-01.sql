use test_schema;

CREATE TABLE test (
	id VARCHAR(10) PRIMARY KEY,
    fname VARCHAR(20),
    lname VARCHAR(20),
    rating INTEGER
);

INSERT INTO test VALUES(1,'gary','mulligan', 4);

SELECT * FROM test;