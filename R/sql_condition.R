sql_condition <- function(..., .defuse = TRUE)
{
    condition <- .sql_prepare(..., defuse = .defuse)
    condition <- Reduce(sql_and, condition)
    return(condition)
}
