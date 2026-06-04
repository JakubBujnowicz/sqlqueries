library(DBI)
library(RSQLite)
library(data.table)


# Tests ------------------------------------------------------------------------
test_that("sql_delete() works with condition as dots", {
    con <- dbConnect(SQLite(), ":memory:")

    # Prepare data
    dt <- data.table(a = 1:5, b = letters[1:5])
    dbWriteTable(con, "test_table", dt)

    # Delete row where a = 1
    sql_stmt <- sql_delete(from = "test_table", "a = 1")

    # Execute delete
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    # Verify result
    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_equal(nrow(res), 4)
    expect_false(1 %in% res$a)

    # Clean up
    dbDisconnect(con)
})

test_that("sql_delete() works with where argument", {
    con <- dbConnect(SQLite(), ":memory:")

    # Prepare data
    dt <- data.table(a = 1:5, b = letters[1:5])
    dbWriteTable(con, "test_table", dt)

    # Delete row where a > 3 using sql_where
    wh <- sql_where("a > 3")
    sql_stmt <- sql_delete(from = "test_table", where = wh)

    # Execute delete
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    # Verify result
    res <- dbGetQuery(con, "SELECT * FROM test_table")
    expect_equal(nrow(res), 3)
    expect_true(all(res$a <= 3))

    # Clean up
    dbDisconnect(con)
})

test_that("sql_delete() validates inputs", {
    expect_error(sql_delete(from = 123))
})

