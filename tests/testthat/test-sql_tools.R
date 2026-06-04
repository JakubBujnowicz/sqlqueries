test_that("sql_tree() returns expected fields for a simple sql_select", {
    tree <- sql_tree(sql_select("*"))
    expect_type(tree, "list")
    expect_named(tree, c("columns", "distinct", "top",
                         "top_percent", "top_with_ties"))
    expect_equal(tree$columns, c(`*` = "*"))
    expect_false(tree$distinct)
    expect_null(tree$top)
    expect_false(tree$top_percent)
    expect_false(tree$top_with_ties)
})

test_that("sql_tree() returns expected fields for a simple sql_from", {
    tree <- sql_tree(sql_from("dt"))
    expect_type(tree, "list")
    expect_named(tree, c("table", "alias"))
    expect_equal(tree$table, "dt")
    expect_null(tree$alias)
})

test_that("sql_tree() handles composed queries as a nested tree", {
    query <- sql_select("*") + sql_from("dt")
    tree <- sql_tree(query)
    expect_type(tree, "list")
    expect_named(tree, c("SELECT", "FROM"))
    expect_named(tree$SELECT, c("columns", "distinct", "top",
                                "top_percent", "top_with_ties"))
    expect_named(tree$FROM, c("table", "alias"))
    expect_equal(tree$FROM$table, "dt")
})

test_that("sql_tree() handles deep nesting with subqueries", {
    inner <- sql(select("*"), from("dt"), where("a = 1"))
    outer <- sql(select("*"), from(inner, alias = "t"))

    tree <- sql_tree(outer)
    expect_type(tree, "list")
    expect_named(tree, c("SELECT", "FROM"))

    # The FROM contains a nested subquery tree
    expect_type(tree$FROM$table, "list")
    expect_named(tree$FROM$table, c("SELECT", "FROM", "WHERE"))
    expect_equal(tree$FROM$alias, "t")
    expect_equal(tree$FROM$table$WHERE$condition, "a = 1")
})

test_that("sql_tree() validates inputs", {
    expect_error(sql_tree("not_a_sql_object"))
    expect_error(sql_tree(1))
    expect_error(sql_tree(NULL))
    expect_error(sql_tree(letters))
})

