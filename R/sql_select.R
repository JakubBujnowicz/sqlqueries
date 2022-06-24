sql_select <- function(..., .distinct = FALSE, .defuse = TRUE)
{
    assert_flag(.distinct)
    assert_flag(.defuse)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_select",
                     fields = list(columns = cols,
                                 distinct = .distinct))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
