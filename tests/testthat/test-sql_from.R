test_that("selecting from a table works", {
    query <- sql(select("*"), from("iris"))
    testthat::expect_equal(iris, sqldf::sqldf(query))
})
