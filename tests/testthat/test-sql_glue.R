test_that("sql_glue() works", {
    # Basic sql string with placeholder
    sql_str <- "SELECT * FROM {table}"
    data <- list(table = "my_table")

    result <- sql_glue(sql_str, .x = data)

    expect_equal(result, "SELECT * FROM my_table")
})
