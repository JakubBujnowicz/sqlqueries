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



.parse.sql_select <- function(x, fields, ...)
{
    header <- "SELECT"
    if (fields$distinct) {
        header <- paste(header, "DISTINCT")
    }
    if (!is.null(fields$top_n)) {
        header <- paste0(header, " TOP ", fields$top_n)
    }

    rslt <- paste(header, .columns_parser(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}