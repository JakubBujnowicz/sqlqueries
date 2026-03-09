sql_insert <- function(into, values = NULL, query = NULL, columns = NULL)
{
    assert_string(into, min.chars = 1L)
    assert_character(columns, min.chars = 1, min.len = 1,
                     any.missing = FALSE, null.ok = TRUE)
    assert_data_frame(values, min.rows = 1, null.ok = TRUE)
    if (!is.null(values)) {
        assert_names(names(values), type = "unique", must.include = columns)
    }
    assert_class(query, classes = "sql_query", null.ok = TRUE)

    if (!xor(is.null(values), is.null(query))) {
        stop("'values' and 'query' cannot be filled in a single INSERT statement")
    }

    rslt <- .new_sql(class = "sql_insert",
                     fields = list(into = into,
                                   values = values,
                                   query = query,
                                   columns = columns))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



.parse.sql_insert <- function(x, fields, ...)
{
    vals <- as.list(fields$values)
    cols <- fields$columns

    if (!is.null(cols)) {
        vals <- vals[cols]
        cols <- paste0("(", toString(cols), ")")
    } else {
        cols <- ""
    }

    target <- paste0("INSERT INTO\n", fields$into, " ", cols)

    if (!is.null(fields$query)) {
        what <- fields$query
    } else {
        vals <- do.call(sql_tuple, args = vals)
        vals <- .sql_parse(vals, as_values = TRUE)
        what <- paste0("VALUES\n", .indent(vals, by = 4, indent_first = TRUE))
    }

    rslt <- paste0(.indent(target, by = 4), "\n", what)
    return(rslt)
}