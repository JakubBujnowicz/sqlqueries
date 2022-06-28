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
