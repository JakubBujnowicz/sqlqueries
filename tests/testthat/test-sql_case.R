library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x))

# Test data
dt <- data.table(a = 1:3)

# Tests
test_that("sql_case() works", {
    query <- sql(
        select("a",
               cs = case("a = 1", 10,
                         "a = 2", 20,
                         .else = 30)),
        from("dt"))
    target <- copy(dt)
    target[, cs := fcase(a == 1, 10,
                         a == 2, 20,
                         default = 30)]

    expect_equal(.sqldf(query), target)
})

