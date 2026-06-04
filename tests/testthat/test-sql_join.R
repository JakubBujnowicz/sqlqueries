library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x))

# Test data
dt1 <- data.table(a = 1:3,
                  b = c("x", "y", "z"))
dt2 <- data.table(a = 2:4,
                  c = c("u", "v", "w"))

# Tests
test_that("sql_join() works", {
    query <- sql(select("*"),
                 from("dt1"),
                 sql_join(table = c(alias = "dt2"),
                          on = "dt1.a = alias.a",
                          type = "inner"))
    res <- .sqldf(query)

    expect_equal(nrow(res), 2)
    expect_equal(res$b, c("y", "z"))
    expect_equal(res$c, c("u", "v"))
})

test_that("sql_join() joins a named table with an alias", {
    query <- sql(select(vars(dt1.a,
                             dt1.b,
                             d.c)),
                 from("dt1"),
                 sql_join(table = "dt2",
                          alias = "d",
                          on = "dt1.a = d.a"))
    target <- data.table(a = 2:3,
                         b = c("y", "z"),
                         c = c("u", "v"))

    expect_equal(.sqldf(query), target)
})

test_that("sql_join() joins a subquery with an alias", {
    inner <- sql(select("a", "c"),
                 from("dt2"),
                 where("a > 1"))
    query <- sql(select(vars(dt1.a,
                             dt1.b,
                             d.c)),
                 from("dt1"),
                 sql_join(table = inner,
                          alias = "d",
                          on = "dt1.a = d.a"))
    target <- data.table(a = 2:3,
                         b = c("y", "z"),
                         c = c("u", "v"))

    expect_equal(.sqldf(query), target)
})

