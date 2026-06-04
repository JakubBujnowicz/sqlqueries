#' Internal column picker helper
#'
#' Processes `...` arguments into a character vector of column names using
#' non-standard evaluation. Supports column aliases via named arguments and
#' short-form function names (e.g. `vars()`, `case()`) through defusing.
#'
#' @eval .docs_dots_columns(purpose = "names of columns", aliases = TRUE)
#' @template param_dot-defuse
#'
#' @return A character vector of column names.
#' @keywords internal
#'
.columns_picker <- function(..., .defuse = TRUE)
{
    x <- unlist(.sql_prepare(..., defuse = .defuse, sql_like = FALSE))
    checkmate::assert_character(x, any.missing = FALSE,
                                min.chars = 1,
                                min.len = 1,
                                unique = TRUE,
                                .var.name = "...")

    nms <- names(x)
    if (!is.null(nms)) {
        ind <- is.na(nms) | nms == ""
        nms[ind] <- x[ind]
        names(x) <- nms

        checkmate::assert_character(nms,
                                    any.missing = FALSE,
                                    unique = TRUE,
                                    min.chars = 1,
                                    .var.name = "names(...)")
    }

    return(x)
}


#' Internal columns formatter
#'
#' Formats a character vector of provided columns (e.g. in SELECT, ORDER BY,
#' GROUP BY...). This aligns "AS" statements in one line for pretty visual
#' output.
#'
#' @param cols a character vector of columns. Vector names may be used to provide
#'   aliases to columns.
#'
#' @return A formatted string of columns.
#' @keywords internal
#'
.columns_formatter <- function(cols)
{
    rslt <- .align_lines(cols)
    add_as <- names(cols) != cols & names(cols) != ""
    rslt[add_as] <- paste0(rslt[add_as], " AS ", names(cols[add_as]))
    rslt <- paste0(trimws(rslt, which = "right"), collapse = ",\n")
    return(rslt)
}


