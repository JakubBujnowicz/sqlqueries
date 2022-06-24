sql_tuple <- function(...)
{
    rslt <- list(...)
    n <- length(rslt[[1]])
    for (i in seq_along(rslt)) {
        assert_atomic_vector(rslt[[i]], len = n,
                             .var.name = paste0("list(...)[[", i, "]]"))
    }

    rslt <- .new_sql(class = "sql_tuple",
                     fields = list(vectors = rslt))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
