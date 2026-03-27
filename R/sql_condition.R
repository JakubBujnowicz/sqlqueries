#' Create an SQL condition
#'
#' TO BE WRITTEN
#'
#' @param ... Arguments to be processed, representing conditions.
#' @template param_dot-defuse
#'
#' @return A character string representing the logical conditions, with S3
#'   class 'sql_logical'.
#' @export
#'
sql_condition <- function(..., .defuse = TRUE)
{
    checkmate::assert_flag(.defuse)

    condition <- .sql_prepare(..., defuse = .defuse)
    condition <- Reduce(sql_and, condition)

    return(condition)
}


