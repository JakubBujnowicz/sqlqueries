#' Create a SQL tuple
#'
#' Creates a SQL tuple, representing a list of values. A single vector
#' becomes a value list like `(1, 2, 3)`. Multiple vectors of the same
#' length produce row-wise tuples like `((1, 'a'), (2, 'b'))`. Used
#' internally by [sql_in()] and [sql_insert()].
#'
#' @param ... atomic vectors to create a tuple from. They all must be of
#'   the same length.
#'
#' @return A character string representing a SQL tuple, with S3 class
#'   'sql_tuple'.
#' @export
#'
#' @family sql_utilities
#'
#' @examples
#' sql_tuple(1:3)
#' sql_tuple(c("a", "b", "c"))
#' sql_tuple(c(1, NA, 3), c("a", "b", NA))
#'
sql_tuple <- function(...)
{
    rslt <- list(...)
    n <- length(rslt[[1]])
    for (i in seq_along(rslt)) {
        checkmate::assert_atomic_vector(
            rslt[[i]], len = n,
            .var.name = paste0("list(...)[[", i, "]]"))
    }

    rslt <- .new_sql(class = "sql_tuple",
                     fields = list(vectors = rslt))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Internal parser for `sql_tuple` objects
#'
#' @inheritParams sql_parse
#' @param as_values a logical value. If `TRUE`, then every element of the tuple
#'   is considered as inner list of values, i.e. the entire vector is not wrapped
#'   in outside parentheses and every item is on a new line. Used in [sql_insert()].
#' @keywords internal
#'
.parse.sql_tuple <- function(x, fields, as_values = FALSE, ...)
{
    rslt <- fields$vectors
    rslt <- lapply(rslt,
                   function(e)
                   {
                       if (!is.numeric(e)) {
                           nas <- is.na(e)
                           e <- paste0("'", e, "'")
                           e[nas] <- NA_character_
                       }

                       return(e)
                   })

    if (length(rslt) > 1 || as_values) {
        rslt <- lapply(rslt, .prepare_vector, short = !as_values)
        rslt <- do.call(paste, args = c(rslt, list(sep = ", ")))
        rslt <- paste0("(", rslt, ")")
    } else {
        rslt <- unlist(rslt)
        rslt <- .prepare_vector(rslt, short = TRUE)
    }

    if (as_values) {
        rslt <- paste0(rslt, collapse = ",\n")
    } else {
        rslt <- paste0("(", toString(rslt), ")")
    }

    return(rslt)
}


