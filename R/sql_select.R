#' Construct an SQL SELECT statement
#'
#' @param ... Columns to select. Can be unquoted column names, character strings,
#'   or results of other sql queries (e.g. [sql_case()]).
#' @param .distinct a logical value. If `TRUE`, adds "DISTINCT" to the query.
#' @param .top a single number. If provided, adds a "TOP" clause.
#' @param .top_percent a logical value. If `TRUE`, adds "PERCENT" to the "TOP"
#'   clause.
#' @param .top_with_ties a logical value. If `TRUE`, adds "WITH TIES" to the
#'   "TOP" clause.
#' @param .defuse Logical. Whether to defuse the input arguments.
#'
#' @return A character string representing the SQL SELECT statement, with S3
#'   class 'sql_select'.
#' @export
#'
sql_select <- function(..., .distinct = FALSE,
                       .top = NULL, .top_percent = FALSE, .top_with_ties = FALSE,
                       .defuse = TRUE)
{
    checkmate::assert_flag(.distinct)
    checkmate::assert_count(.top, null.ok = TRUE)
    checkmate::assert_flag(.top_percent)
    checkmate::assert_flag(.top_with_ties)
    checkmate::assert_flag(.defuse)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_select",
                     fields = list(columns = cols,
                                   distinct = .distinct,
                                   top = .top,
                                   top_percent = .top_percent,
                                   top_with_ties = .top_with_ties))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Internal parser for `sql_select` objects
#'
#' @param x A sql_select object.
#' @param fields Fields of the sql_select object.
#' @param ... additional arguments.
#'
#' @return A character string with parsed string.
#' @keywords internal
#'
.parse.sql_select <- function(x, fields, ...)
{
    header <- "SELECT"
    if (fields$distinct) {
        header <- paste(header, "DISTINCT")
    }

    if (!is.null(fields$top)) {
        header <- sprintf("%s TOP (%i)", header, fields$top)

        if (fields$top_percent) {
            header <- paste0(header, " PERCENT")
        }
        if (fields$top_with_ties) {
            header <- paste0(header, " WITH TIES")
        }
    }

    rslt <- paste(header, .columns_parser(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


