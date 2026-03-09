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



.parse.sql_update <- function(x, fields, ...)
{
    update <- paste0("UPDATE\n", fields$table)
    update <- .indent(update, by = 4)

    .prepare <- function(x)
    {
        if (is.na(x) || is.null(x)) {
            x <- "NULL"
        } else if (!is.numeric(x)) {
            x <- paste0("'", x, "'")
        }

        return(x)
    }

    set <- lapply(fields$set, .prepare)

    set <- paste(format(names(set)), "=", set,
                  collapse = ",\n")
    set <- .indent(set, by = 4, indent_first = TRUE)
    rslt <- paste0(update, "\nSET\n", set)
    return(rslt)
}