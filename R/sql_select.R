sql_select <- function(..., .distinct = FALSE, .top_n = NULL, .defuse = TRUE)
{
    assert_flag(.distinct)
    assert_flag(.defuse)
    assert_count(.top_n, positive = TRUE, null.ok = TRUE)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_select",
                     fields = list(columns = cols,
                                   distinct = .distinct,
                                   top_n = .top_n))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
