sql_tuple <- function(...)
{
    rslt <- list(...)
    n <- length(rslt[[1]])
    for (i in seq_along(rslt)) {
        assert_atomic_vector(rslt[[i]], len = n,
                             .var.name = paste0("list(...)[[", i, "]]"))

        if (!is.numeric(rslt[[i]])) {
            rslt[[i]] <- paste0("'", rslt[[i]], "'")
        }
    }

    if (length(rslt) > 1) {
        rslt <- do.call(paste, args = c(rslt, list(sep = ", ")))
        rslt <- paste0("(", rslt, ")")
    } else {
        rslt <- unlist(rslt)
    }

    rslt <- paste0("(", toString(rslt), ")")
    return(rslt)
}
