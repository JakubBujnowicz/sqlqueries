library(DBI)
library(RSQLite)
library(data.table)


# Tests ------------------------------------------------------------------------
test_that("sql_insert() works with values", {
    con <- dbConnect(SQLite(), ":memory:")

    # Create table
    dbExecute(con, "CREATE TABLE test_table (a integer, b text)")

    # Insert data
    dt <- data.table(a = 1:2,
                     b = c("x", "y"))
    sql_stmt <- sql_insert(into = "test_table",
                           values = dt)

    # Execute insert
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    # Verify result
    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_identical(as.data.table(res), dt)

    # Clean up
    dbDisconnect(con)
})

test_that("sql_insert() works with columns", {
    con <- dbConnect(SQLite(), ":memory:")

    # Create table
    dbExecute(con, "CREATE TABLE test_table (a integer, b text, c real)")

    # Insert data into subset of columns
    dt <- data.table(a = 1L, b = "x")
    sql_stmt <- sql_insert(into = "test_table",
                           values = dt,
                           columns = c("a", "b"))

    # Execute insert
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    # Verify result
    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_equal(nrow(res), 1)
    expect_equal(res$a, 1L)
    expect_equal(res$b, "x")
    expect_true(is.na(res$c))

    # Clean up
    dbDisconnect(con)
})

test_that("sql_insert() handles numeric NAs as NULL", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE test_table (a integer)")

    dt <- data.table(a = c(1L, NA, 3L))
    sql_stmt <- sql_insert(into = "test_table", values = dt)

    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_equal(res$a, c(1L, NA, 3L))
    expect_true(is.na(res$a[2]))

    dbDisconnect(con)
})

test_that("sql_insert() handles character NAs as NULL", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE test_table (b text)")

    dt <- data.table(b = c("x", NA, "z"))
    sql_stmt <- sql_insert(into = "test_table", values = dt)

    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_equal(res$b, c("x", NA, "z"))
    expect_true(is.na(res$b[2]))

    dbDisconnect(con)
})

test_that("sql_insert() handles mixed type NAs", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE test_table (a integer, b text)")

    dt <- data.table(a = c(1L, NA, 3L),
                     b = c(NA, "y", "z"))
    sql_stmt <- sql_insert(into = "test_table", values = dt)

    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_equal(res$a, c(1L, NA, 3L))
    expect_equal(res$b, c(NA, "y", "z"))
    expect_true(is.na(res$a[2]))
    expect_true(is.na(res$b[1]))

    dbDisconnect(con)
})

test_that("sql_insert() works with query", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE source (a integer, b text)")
    dbExecute(con, "CREATE TABLE target (a integer, b text)")
    dbExecute(con, "INSERT INTO source VALUES (1, 'x'), (2, 'y'), (3, 'z')")

    query <- sql(select("*"),
                 from("source"),
                 where("a > 1"))
    sql_stmt <- sql_insert(into = "target", query = query)

    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM target ORDER BY a")
    expect_equal(as.data.table(res), data.table(a = 2:3, b = c("y", "z")))
    dbDisconnect(con)
})

test_that("sql_insert() works with query and columns", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE source (a integer, b text, c real)")
    dbExecute(con, "CREATE TABLE target (a integer, b text)")
    dbExecute(con, "INSERT INTO source VALUES (1, 'x', 1.5), (2, 'y', 2.5)")

    query <- sql(select("a", "b"),
                 from("source"))
    sql_stmt <- sql_insert(into = "target",
                           query = query,
                           columns = c("a", "b"))

    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM target")
    expect_equal(as.data.table(res),
                 data.table(a = 1:2,
                            b = c("x", "y")))
    dbDisconnect(con)
})

test_that("sql_insert() validates inputs", {
    dt <- data.table(a = 1:2,
                     b = c("x", "y"))
    expect_error(sql_insert(into = "test", values = dt,
                            query = sql(select("*"),
                                        from("table"))))
    expect_error(sql_insert(into = "test", values = NULL, query = NULL))
    expect_error(sql_insert(into = 123, values = dt))
})

