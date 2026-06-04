test_that("all exported sql_* functions return a 'sql' object", {
    # Define a list of functions and their minimal required arguments
    # Note: Using NULL or dummy values that pass assertions
    sql_funcs <- list(
        sql_case = list(args = list("a = 1", 1),
                        kwargs = list(.else = 2)),
        sql_create_table = list(args = list("test_table"),
                                kwargs = list(variables = c(a = "integer"))),
        sql_delete = list(args = list("test_table")),
        sql_from = list(args = list("test_table")),
        sql_group_by = list(args = list("col1")),
        sql_having = list(args = list("col1 = 1")),
        sql_in = list(args = list("col1", 1)),
        sql_insert = list(args = list("test_table"),
                          kwargs = list(values = data.frame(a = 1))),
        sql_join = list(args = list("table2", "col1 = col2")),
        sql_logical = list(args = list("a = 1", "b = 2", "and")),
        sql_order_by = list(args = list("col1")),
        sql_parenth = list(args = list("a = 1")),
        sql_query = list(args = list("SELECT * FROM table")),
        sql_select = list(args = list("*")),
        sql_tuple = list(args = list(1, 2)),
        sql_update = list(args = list("table", list(a = 1))),
        sql_where = list(args = list("a = 1"))
    )

    for (func_name in names(sql_funcs)) {
        func <- get(func_name)
        spec <- sql_funcs[[func_name]]

        # Execute function
        result <- do.call(func, c(spec$args, spec$kwargs))

        # Check if it inherits from "sql"
        msg <- paste("Function", func_name, "does not return a 'sql' object")
        expect_true(is_sql(result), info = msg)
    }
})

