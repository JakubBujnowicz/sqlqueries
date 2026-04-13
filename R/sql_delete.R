#' Construct a SQL DELETE statement
#'
#' DESCRIPTION TO BE WRITTEN
#'
#' @param from a single, non-empty string with a table name to delete from.
#' @param ... a vector of conditions, passed to [sqlqueries::sql_condition()].
#' @param where a single [sqlqueries::sql_where()] object. Joined to conditions
#'   from `...` with an AND statement. Can also be used on its own.
#' @template param_dot-defuse
#'
#' @return A character string representing the SQL DELETE statement, with S3
#'   class 'sql_delete'.
#' @export
#'
sql_delete <- function(from, ..., where = NULL, .defuse = TRUE)
{
    checkmate::assert_string(from, min.chars = 1L)
    checkmate::assert_class(where, classes = "sql_where", null.ok = TRUE)
    checkmate::assert_flag(.defuse)

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



#' Internal parser for `sql_delete` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_delete <- function(x, fields, ...)
{
    wh_cond <- attr(fields$where, "fields")$condition
    dots_cond <- fields$condition
    cond <- list(wh_cond, dots_cond)

    # Remove empty
    cond <- cond[!sapply(cond, is.null)]
    if (length(cond) > 0) {
        cond <- do.call(sql_condition, args = cond)
    }

    rslt <- paste("DELETE FROM", fields$from)
    if (length(cond) > 0) {
        cond <- paste("WHERE", cond, sep = "\n")
        cond <- .indent(cond, by = 4)

        rslt <- paste0(rslt, "\n", cond)
    }
    return(rslt)
}
