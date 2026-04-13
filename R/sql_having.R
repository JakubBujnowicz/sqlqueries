#' Construct a SQL HAVING statement
#'
#' DESCRIPTION TO BE WRITTEN
#'
#' @inheritParams sql_where
#'
#' @return A character string representing the SQL HAVING statement, with S3
#'   class 'sql_having'.
#' @export
#'
sql_having <- function(..., .defuse = TRUE)
{
    checkmate::assert_flag(.defuse)

    condition <- sql_condition(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_having",
                     fields = list(condition = condition))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Internal parser for `sql_having` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_having <- function(x, fields, ...)
{
    rslt <- paste("HAVING", fields$condition, sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


