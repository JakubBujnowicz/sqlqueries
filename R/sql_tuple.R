#' Create a SQL tuple
#'
#' DESCRIPTION TO BE WRITTEN
#'
#' @param ... atomic vectors to create a tuple from. They all must be of
#'   the same length.
#'
#' @return A single string with a SQL tuple.
#' @export
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


