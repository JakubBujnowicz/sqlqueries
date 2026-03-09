sql_group_by <- function(..., .defuse = TRUE)
{
    assert_flag(.defuse)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_group_by",
                     fields = list(columns = cols))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



.parse.sql_group_by <- function(x, fields, ...)
{
    header <- "GROUP BY"
    rslt <- paste(header, .columns_parser(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}