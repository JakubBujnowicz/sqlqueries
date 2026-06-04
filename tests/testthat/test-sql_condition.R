library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x), keep.rownames = FALSE)

n <- 1e4
k <- 4
dt <- data.table(w = sample(k, n, TRUE),
                 x = sample(k, n, TRUE),
                 y = sample(k, n, TRUE),
                 z = sample(k, n, TRUE))


# Tests ------------------------------------------------------------------------
test_that("sql_condition() works", {
    target <- dt[(x == 1 & y == 2 & w == z) &
                 (z == 2 | w == 3 | x == 2) &
                 x < 3]

    cond <- sql_condition(
        ("x = 1" %AND% "y = 2" %AND% "w = z") %AND%
        ("z = 2" %OR% "w = 3" %OR% "x = 2"),
        "x < 3")
    query <- sql(select("*"),
                 from("dt"),
                 where(cond))

    expect_identical(.sqldf(query), target)
})

test_that("sql_condition() works with simple strings", {
    target <- dt[x == 1 & y == 2]
    cond <- sql_condition("x = 1", "y = 2")
    query <- sql(select("*"),
                 from("dt"),
                 where(cond))

    expect_identical(.sqldf(query), target)
})

test_that("sql_condition() works without defusing", {
    target <- dt[(x == 1 & y == 2 & w == z) &
                 (z == 2 | w == 3 | x == 2) &
                 x < 3]

    cond <- sql_condition(
        sql_parenth("x = 1" %AND% "y = 2" %AND% "w = z") %AND%
            sql_parenth("z = 2" %OR% "w = 3" %OR% "x = 2"),
        "x < 3",
        .defuse = FALSE)
    query <- sql(select("*"),
                 from("dt"),
                 where(cond))

    expect_identical(.sqldf(query), target)

    # Omits parentheses without defusing
    cond <- sql_condition(
        ("x = 1" %OR% "y = 2") %AND% "w = z",
        .defuse = FALSE)
    target <- dt[x == 1 | y == 2 & w == z]
    query <- sql(select("*"),
                 from("dt"),
                 where(cond))

    expect_identical(.sqldf(query), target)
})

test_that("sql_condition() validates inputs", {
    expect_error(sql_condition())
    expect_warning(sql_condition("a", sum),
                   regexp = "following expressions were omitted")
    expect_error(sql_condition("some string", .defuse = c(FALSE, FALSE)))
})

