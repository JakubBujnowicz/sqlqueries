test_that("case with numerics", {
    cs <- sql_case("species = 'setosa'", 1,
                   "species = 'versicolor'", 2,
                   .else = 3)
    query <- sql(select("Species", CaseCol = cs),
                 from("iris"))

    target <- data.frame(
        iris["Species"],
        CaseCol = data.table::fcase(iris$Species == "setosa", 1,
                                    iris$Species == "versicolor", 2,
                                    default = 3))
    expect_equal(sqldf::sqldf(query), target)
})
