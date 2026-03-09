sql_case <- function(..., .else = NULL)
{
    x <- c(...)
    assert_atomic_vector(x, min.len = 2,
                         .var.name = "...")
    assert_true(length(x) %% 2 == 0,
                .var.name = "length of '...' is even")

    rslt <- .new_sql(class = "sql_case",
                     fields = list(cases = x,
                                   .else = .else))
    rslt <- .sql_parse(rslt)
    return(rslt)
}

#' @keywords internal
.parse.sql_case <- function(x, fields, ...)
{
    x <- fields$cases
    n <- length(x)
    thens <- seq_len(n / 2L) * 2L
    whens <- thens - 1L

    rslt <- paste0("\nWHEN ", format(x[whens]), " THEN ", x[thens])
    rslt <- paste0(rslt, collapse = "")
    if (!is.null(fields$.else)) {
        rslt <- paste0(rslt, "\nELSE ", fields$.else)
    }

    rslt <- paste0("CASE", .indent(rslt, by = 4L), "\nEND")
    return(rslt)
}


