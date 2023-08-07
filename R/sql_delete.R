sql_delete <- function(from, ..., where = NULL, .defuse = TRUE)
{
    assert_string(from, min.chars = 1L)
    assert_class(where, classes = "sql_where", null.ok = TRUE)
    assert_flag(.defuse)

    if (...length() > 0) {
        condition <- sql_condition(..., .defuse = .defuse)
    } else {
        condition <- NULL
    }

    rslt <- .new_sql(class = "sql_delete",
                     fields = list(from = from,
                                   where = where,
                                   condition = condition))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
