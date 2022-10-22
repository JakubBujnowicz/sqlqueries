sql_update <- function(table, set)
{
    # Assertions
    assert_string(table)
    assert_list(set, min.len = 1)
    assert_names(names(set), type = "unique")
    for (i in seq_along(set)) {
        assert_scalar(set[[i]], na.ok = TRUE, null.ok = TRUE)
    }

    # Create the object
    rslt <- .new_sql(class = "sql_update",
                     fields = list(table = table,
                                   set = set))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
