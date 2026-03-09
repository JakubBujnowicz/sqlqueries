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



.parse.sql_tuple <- function(x, fields, as_values = FALSE, ...)
{
    rslt <- fields$vectors
    rslt <- lapply(rslt,
                   function(e)
                   {
                       if (!is.numeric(e)) {
                           e <- paste0("'", e, "'")
                       }

                       return(e)
                   })

    if (length(rslt) > 1 || as_values) {
        rslt <- lapply(rslt, .align, short = !as_values)
        rslt <- do.call(paste, args = c(rslt, list(sep = ", ")))
        rslt <- paste0("(", rslt, ")")
    } else {
        rslt <- unlist(rslt)
        rslt <- .align(rslt, short = TRUE)
    }

    if (as_values) {
        rslt <- paste0(rslt, collapse = ",\n")
    } else {
        rslt <- paste0("(", toString(rslt), ")")
    }

    return(rslt)
}