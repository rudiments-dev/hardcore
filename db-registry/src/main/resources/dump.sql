CREATE TABLE sample (
    id IDENTITY PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    comment CLOB
);

CREATE TABLE example (
    id                IDENTITY PRIMARY KEY,
    int_column        INT,
    bool_column       BOOLEAN,
    tinyint_column    TINYINT,
    smallint_column   SMALLINT,
    bigint_column     BIGINT,
    decimal_column    DECIMAL(12, 3),
    double_column     DOUBLE(12),
    real_column       REAL(12),
    time_column       TIME,
    date_column       DATE,
    timestamp_column  TIMESTAMP,
    binary_column     BINARY(5),
    varchar_column    VARCHAR(67),
    char_column       CHAR(89),
    blob_column       BLOB(10),
    clob_column       CLOB(10),
    uuid_column       UUID,
    array_column      ARRAY,
    enum_column       ENUM('RED', 'GREEN', 'BLUE')
);

ALTER TABLE example ADD CONSTRAINT ref_1 FOREIGN KEY (bigint_column) REFERENCES sample (id);

SELECT DISTINCT -- table_name (table_columns) REFERENCES ref_name (ref_columns)
       fk.constraint_name AS name,
       fk.table_name      AS table_name,
       fk.column_list     AS table_columns,
       pk.table_name      AS ref_name,
       pk.column_list     AS ref_columns
FROM   information_schema.constraints fk
           JOIN information_schema.constraints pk
                ON fk.unique_index_name =  pk.unique_index_name
                    AND pk.constraint_type =    'PRIMARY KEY'
WHERE fk.table_schema = 'HELLO'
  AND fk.constraint_type = 'REFERENTIAL';