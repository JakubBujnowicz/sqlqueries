library(DBI)
library(RSQLite)
library(data.table)


# Tests ------------------------------------------------------------------------
test_that("sql_update() works", {
    con <- dbConnect(SQLite(), ":memory:")

    # Create table
    dbExecute(con, "CREATE TABLE test_table (a integer, b text)")
    dbExecute(con, "INSERT INTO test_table VALUES (1, 'x'), (2, 'y')")

    # Update data
    sql_stmt <- sql_update(table = "test_table",
                           set = list(b = "z"))

    # Execute update
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    # Verify result
    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_true(all(res$b == "z"))

    # Clean up
    dbDisconnect(con)
})

test_that("sql_update() handles character NA as NULL", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE test_table (a integer, b text)")
    dbExecute(con, "INSERT INTO test_table VALUES (1, 'x'), (2, 'y')")

    sql_stmt <- sql_update(table = "test_table",
                           set = list(b = NA_character_))
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM test_table ORDER BY a")
    expect_true(all(is.na(res$b)))
    dbDisconnect(con)
})

test_that("sql_update() handles numeric NA as NULL", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE test_table (a integer, b text)")
    dbExecute(con, "INSERT INTO test_table VALUES (1, 'x'), (2, 'y')")

    sql_stmt <- sql_update(table = "test_table",
                           set = list(a = NA_integer_))
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM test_table ORDER BY a")
    expect_true(all(is.na(res$a)))
    dbDisconnect(con)
})

test_that("sql_update() handles mixed NAs as NULL", {
    con <- dbConnect(SQLite(), ":memory:")
    dbExecute(con, "CREATE TABLE test_table (a integer, b text)")
    dbExecute(con, "INSERT INTO test_table VALUES (1, 'x')")

    sql_stmt <- sql_update(table = "test_table",
                           set = list(a = NA_integer_,
                                      b = NA_character_))
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_true(is.na(res$a))
    expect_true(is.na(res$b))
    dbDisconnect(con)
})

test_that("sql_update() validates inputs", {
    expect_error(sql_update(table = 123,
                            set = list(a = 1)))
    expect_error(sql_update(table = "test",
                            set = list(1)))
})

