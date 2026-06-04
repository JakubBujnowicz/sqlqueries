library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x))

# Test data
dt <- data.table(a = 1:3)

# Tests
test_that("sql_in() works", {
    query <- sql(select("*"),
                 from("dt"),
                 where(sql_in("a", c(1, 2))))
    target <- dt[a %in% c(1, 2)]

    expect_equal(.sqldf(query), target)
})
