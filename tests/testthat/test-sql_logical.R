library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x), keep.rownames = FALSE)

n <- 100
k <- 4
dt <- data.table(w = sample(k, n, TRUE),
                 x = sample(k, n, TRUE),
                 y = sample(k, n, TRUE),
                 z = sample(k, n, TRUE))


# Tests ------------------------------------------------------------------------
test_that(".new_logical() runs", {
    cond <- .new_logical("x = 1", "y = 1", "or")

    target <- dt[x == 1 | y == 1]
    query <- sql(select("*"),
                 from("dt"),
                 where(cond))

    expect_identical(.sqldf(query), target)

    # Also with some other custom operators
    cond <- .new_logical("x = 1", "y = 1", "xor")
    checkmate::expect_string(
        cond, pattern = "x = 1.*XOR.*y = 1")
})

test_that(".new_logical() works with sql_conditions", {
    target <- dt[x == 1 & y == 1 | z > 2]

    cond <- .new_logical("x = 1" %AND% "y = 1",
                         "z > 2",
                         operator = "or")
    query <- sql(select("*"),
                 from("dt"),
                 where(cond))

    expect_identical(.sqldf(query), target)
})

test_that(".new_logical() validates inputs", {
    expect_error(.new_logical())
    expect_error(.new_logical("a", sum))
})


