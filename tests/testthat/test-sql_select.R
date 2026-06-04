library(data.table)

# Functions
.sqldf <- function(x) as.data.table(sqldf::sqldf(x))

# Test data
dt <- data.table(col1 = 1:3,
                 col2 = c(4, 4, 5),
                 col3 = c(5, 5, 5))


# Tests ------------------------------------------------------------------------
test_that("sql_select() handles basic column selection", {
    query <- sql(select("col1", "col3"),
                 from("dt"))
    target <- dt[, list(col1, col3)]

    expect_identical(.sqldf(query), target)
})

test_that("sql_select() handles distinct", {
    query <- sql(select("col3", .distinct = TRUE),
                 from("dt"))
    target <- unique(dt[, list(col3)])

    expect_identical(.sqldf(query), target)

    query <- sql(select(vars(col1, col3), .distinct = TRUE),
                 from("dt"))
    target <- unique(dt[, list(col1, col3)])

    expect_identical(.sqldf(query), target)
})

# sqldf does not handle SQLServer "TOP" syntax, can't test against data as
# easily
## TODO: Figure out a way to test it
test_that("sql_select() handles TOP: text based", {
    query <- sql_select("*", .top = 2)
    expect_match(query, "SELECT TOP \\(2\\)")

    query <- sql_select("*", .top = 2, .top_percent = TRUE)
    expect_match(query, "SELECT TOP \\(2\\) PERCENT")

    query <- sql_select("*", .top = 2, .top_with_ties = TRUE)
    expect_match(query, "SELECT TOP \\(2\\) WITH TIES")
})

test_that("sql_select() handles aliased columns", {
    query <- sql(select(NewCol = "col1"),
                 from("dt"))
    target <- dt[, list(NewCol = col1)]

    expect_identical(.sqldf(query), target)
})

dt2 <- data.table(a = 1,
                  able = c(0, 0:2),
                  about = 3,
                  asdasd = 4,
                  glued_var = 5,
                  Variable = 7,
                  SecondVar = 8)
test_that("sql_select() handles multiple selection methods", {
    target <- dt2[, .(a, able, about,
                      A = asdasd,
                      glued = glued_var,
                      CaseVar = data.table::fcase(able == 1, 1,
                                                  able == 0, 2,
                                                  default = 100),
                      Variable,
                      SV = SecondVar)]
    target <- unique(target)

    cs <- sql_case("able IS 1", 1,
                   "able IS 0", 2,
                   .else = 100)
    query <- sql(select(stringr::words[1:3],
                        A = "asdasd",
                        glued = "{variable}",
                        CaseVar = cs,
                        vars(Variable, SV = SecondVar),
                        .distinct = TRUE),
                 .glue = list(variable = "glued_var")) +
        sql_from("dt2")

    expect_equal(.sqldf(query), target)
})

test_that("sql_select() errors when no columns are selectable", {
    expect_error(sql_select(.defuse = FALSE),
                 "no SQL functions nor objects passed")
})

test_that("sql_select() validates inputs", {
    expect_error(sql_select())
    expect_error(sql_select("col1", .distinct = "TRUE"))
    expect_error(sql_select("col1", .top = -1))
    expect_error(sql_select("col1", .top = "5"))
    expect_error(sql_select("col1", .top_percent = c(FALSE, FALSE)))
    expect_error(sql_select("col1", .top_with_ties = c(FALSE, FALSE)))
    expect_error(sql_select("col1", .defuse = c(FALSE, FALSE)))
})

