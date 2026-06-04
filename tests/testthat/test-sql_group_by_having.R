library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x))

# Test data
dt <- data.table(a = 1:5,
                 b = c(1, 1, 2, 2, 3),
                 c = 1:5)


# Tests ------------------------------------------------------------------------
test_that("sql_group_by() works", {
    query <- sql(select(c("b", "sum(a) as s")),
                 from("dt"),
                 sql_group_by("b"))
    target <- dt[, .(s = sum(a)), by = .(b)]

    expect_equal(.sqldf(query), target)
})

test_that("sql_having() works", {
    query <- sql(select(c("b", "sum(a) as s")),
                 from("dt"),
                 sql_group_by("b"),
                 sql_having("s > 3"))
    target <- dt[, .(s = sum(a)), by = .(b)][s > 3]

    expect_equal(.sqldf(query), target)
})
