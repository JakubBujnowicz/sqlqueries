library(DBI)
library(RSQLite)
library(data.table)

# Tests ------------------------------------------------------------------------
test_that("sql_create_table() works with variables", {
    con <- dbConnect(SQLite(), ":memory:")

    name <- "test_table"
    variables <- c(col1 = "integer", col2 = "text")

    sql_stmt <- sql_create_table(name = name, variables = variables)

    # Create table
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    # Check if table exists
    expect_true(name %in% dbListTables(con))

    # Clean up
    dbDisconnect(con)
})

test_that("sql_create_table() works with query", {
    con <- dbConnect(SQLite(), ":memory:")

    # Prepare data
    dt <- data.table(a = 1:5, b = letters[1:5])
    dbWriteTable(con, "original", dt)

    name <- "new_table"
    query <- sql(select("*"), from("original"))

    sql_stmt <- sql_create_table(name = name, query = query)

    # Create new table
    rs <- dbSendStatement(con, as.character(sql_stmt))
    dbClearResult(rs)

    # Verify content
    res <- dbGetQuery(con, paste("SELECT * FROM", name))
    expect_identical(as.data.table(res), dt)

    # Clean up
    dbDisconnect(con)
})

test_that("sql_create_table() validates inputs", {
    expect_error(sql_create_table(name = "test", variables = c(a = "int"),
                                  query = sql(select("*"),
                                              from("table"))))
    expect_error(sql_create_table(name = "test", variables = NULL,
                                  query = NULL))
    expect_error(sql_create_table(name = 123, variables = c(a = "int")))
})


