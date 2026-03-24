library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x))

dt <- as.data.table(mtcars)


# Tests ------------------------------------------------------------------------
test_that("sql_from() with a simple name", {
    query <- sql(select("*"), from("dt"))
    expect_equal(.sqldf(query), dt)
})

test_that("sql_from() with a simple alias", {
    target <- dt[, list(cyl, mpg)]
    sel <- sql_select(vars(mt.cyl, mt.mpg))
    query <- sql(sel, from("dt", alias = "mt"))
    expect_equal(.sqldf(query), target)

    query <- sql(sel, from(c(mt = "dt")))
    expect_equal(.sqldf(query), target)

    # Check that alias overwrites the stupid_name
    query <- sql(sel, from(c(stupid_name = "dt"), alias = "mt"))
    expect_equal(.sqldf(query), target)
})

test_that("sql_from() with table as query", {
    target <- dt[hp > 180, list(cyl2 = cyl, mpg2 = mpg)]

    inner_query <- sql(select(vars(cyl2 = cyl, mpg2 = mpg)),
                       from("dt"),
                       where("hp > 180"))
    query <- sql(select(vars(mt.cyl2, mt.mpg2)),
                 from(inner_query, alias = "mt"))
    expect_equal(.sqldf(query), target)
})

test_that("sql_from() validates inputs", {
    expect_error(sql_from())
    expect_error(sql_from(table = 1))
    expect_error(sql_from(table = "dt", alias = character(0)))
})


