library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x))

# Test data
dt <- data.table(a = c(3, 1, 2),
                 b = c(1, 2, 3))

# Tests
test_that("sql_order_by() works", {
    query <- sql(select("*"),
                 from("dt"),
                 order_by("a"))
    target <- dt[order(a)]

    expect_equal(.sqldf(query), target)
})

test_that("sql_order_by() with vars and minus signs", {
    query <- sql(select("*"),
                 from("dt"),
                 order_by(vars(-a, b)))
    target <- dt[order(-a, b)]

    expect_equal(.sqldf(query), target)
})

