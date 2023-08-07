sql_create_table <- function(name, variables = NULL, query = NULL)
{
    if (!xor(is.null(variables), is.null(query))) {
        stop("only one of 'variables' and 'query' must be filled in a single ",
             "CREATE TABLE statement")
    }
    assert_string(name, min.chars = 1L)
    assert_class(query, classes = "sql_query", null.ok = TRUE)
    assert_character(variables, min.chars = 1L, min.len = 1L,
                     any.missing = FALSE, null.ok = TRUE)
    if (!is.null(variables)) {
        assert_names(names(variables), type = "unique")
    }

    rslt <- .new_sql(class = "sql_create_table",
                     fields = list(name = name,
                                   variables = variables,
                                   query = query))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
