sql_order_by <- function(..., .defuse = TRUE)
{
    assert_flag(.defuse)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_order_by",
                     fields = list(columns = cols))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
