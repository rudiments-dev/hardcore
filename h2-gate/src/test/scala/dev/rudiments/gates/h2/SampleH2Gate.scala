package dev.rudiments.gates.h2


trait SampleH2Gate {
  val config: H2Config = H2Config(
    driver =    "org.h2.Driver",
    url =       "jdbc:h2:mem:hello2",
    user =      "user",
    password =  "pass",
    schema =    "hello2"
  ).initConnectionPool()

  val gate: H2Gate = new H2Gate(config)

  {
    import scalikejdbc._
    implicit val session: DBSession = AutoSession
    sql"CREATE SCHEMA hello2".execute().apply()
    sql"SET SCHEMA hello2".execute().apply()

    sql"""CREATE TABLE sample (
         |      id IDENTITY PRIMARY KEY,
         |      name VARCHAR(255) NOT NULL,
         |      comment CLOB
         |)""".stripMargin.execute().apply()

    sql"""CREATE TABLE example (
         |    id                IDENTITY PRIMARY KEY,
         |    int_column        INT,
         |    bool_column       BOOLEAN,
         |    tinyint_column    TINYINT,
         |    smallint_column   SMALLINT,
         |    bigint_column     BIGINT,
         |    decimal_column    DECIMAL(12, 3),
         |    double_column     DOUBLE(12),
         |    real_column       REAL(12),
         |    time_column       TIME,
         |    date_column       DATE,
         |    timestamp_column  TIMESTAMP,
         |    binary_column     BINARY(5),
         |    varchar_column    VARCHAR(67),
         |    char_column       CHAR(89),
         |    blob_column       BLOB(10),
         |    clob_column       CLOB(10),
         |    uuid_column       UUID,
         |    array_column      ARRAY,
         |    enum_column       ENUM('RED', 'GREEN', 'BLUE')
         |)""".stripMargin.execute().apply()

    sql"ALTER TABLE example ADD CONSTRAINT ref_1 FOREIGN KEY (bigint_column) REFERENCES sample (id)".execute().apply()
    session.close()
  }

}
