.docs_dots_columns <- function(purpose, aliases = FALSE)
{
    result <- paste0(
        "@param ... character vectors, ", purpose, ". ",
        "May also include other `sql` objects ",
        "(e.g.[sqlqueries::sql_vars()], [sqlqueries::sql_case()]). ",
        "Non-character elements are dropped with a warning, after dropping the ",
        "vector cannot be empty. ")

    if (aliases) {
        result <- paste0(
            result,
            "Vectors may be named to provide column aliases. ")
    }

    return(result)
}


