test_that("sql_parenth() works", {
    query <- sql_parenth("a = 1")
    expect_equal(as.character(query), "(a = 1)")
})
