#' @rdname sql_logical
#' @export
#'
sql_in <- function(x, vector)
{
    assert_string(x, min.chars = 1L)
    assert(check_string(vector, min.chars = 1L),
           check_atomic(vector, any.missing = FALSE,
                        min.len = 1))

    rslt <- .new_sql(class = "sql_in",
                     fields = list(x = x,
                                   vector = vector))
    rslt <- .sql_parse(rslt)
}


#' @rdname sql_logical
#' @export
#'
`%IN%` <- sql_in
