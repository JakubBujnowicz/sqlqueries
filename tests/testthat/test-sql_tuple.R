test_that("sql_tuple() handles single numeric vector", {
    query <- sql_tuple(1:3)
    expect_match(query, "(1, 2, 3)")
})

test_that("sql_tuple() handles single character vector", {
    query <- sql_tuple(c("a", "b", "c"))
    expect_match(query, "('a', 'b', 'c')")
})

test_that("sql_tuple() handles numeric vector with NAs", {
    query <- sql_tuple(c(1, NA, 3))
    expect_match(query, "(1, NULL, 3)")
})

test_that("sql_tuple() handles character vector with NAs", {
    query <- sql_tuple(c("a", NA, "c"))
    expect_match(query, "('a', NULL, 'c')")
})

test_that("sql_tuple() handles multiple vectors with mixed NAs", {
    query <- sql_tuple(c(1, NA, 3), c("a", "b", NA))
    expect_match(query, "((1, 'a'), (NULL, 'b'), (3, NULL))", fixed = TRUE)
})

test_that("sql_tuple() handles all NA vectors", {
    query <- sql_tuple(c(NA, NA), c(NA, NA))
    expect_match(query, "((NULL, NULL), (NULL, NULL))", fixed = TRUE)
})

test_that("sql_tuple() validates inputs", {
    expect_error(sql_tuple())
    expect_error(sql_tuple(c(1, 2), c("a", "b", "c")))
})

