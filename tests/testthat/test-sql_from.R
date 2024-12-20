test_that("selecting from a table works", {
    query <- sql(select("*"), from("iris"))
    testthat::expect_equal(iris, sqldf::sqldf(query))
})

test_that("nested query", {
    query_inner <- sql(select("*"), from("iris"))
    query <- sql(select("*"), from(query_inner, alias = "new_tab"))
    testthat::expect_equal(iris, sqldf::sqldf(query))
})

