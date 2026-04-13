#' Construct a SQL UPDATE statement
#'
#' DESCRIPTION TO BE WRITTEN
#'
#' @param table a single string with the name of the table to update.
#' @param set a list, every element must be named (with unique names) and
#'   must contain only a scalar value.
#'
#' @return A character string representing the SQL UPDATE statement, with S3
#'   class 'sql_update'.
#' @export
#'
sql_update <- function(table, set)
{
    # Assertions
    checkmate::assert_string(table)
    checkmate::assert_list(set, min.len = 1)
    checkmate::assert_names(names(set), type = "unique")
    for (i in seq_along(set)) {
        checkmate::assert_scalar(set[[i]], na.ok = TRUE, null.ok = TRUE)
    }

    # Create the object
    rslt <- .new_sql(class = "sql_update",
                     fields = list(table = table,
                                   set = set))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



#' Internal parser for `sql_update` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_update <- function(x, fields, ...)
{
    update <- paste0("UPDATE\n", fields$table)
    update <- .indent(update, by = 4)

    .prepare_scalar <- function(x)
    {
        if (is.na(x) || is.null(x)) {
            x <- "NULL"
        } else if (!is.numeric(x)) {
            x <- paste0("'", x, "'")
        }

        return(x)
    }

        set <- lapply(fields$set, .prepare_scalar)

    set <- paste(format(names(set)), "=", set,
                  collapse = ",\n")
    set <- .indent(set, by = 4, indent_first = TRUE)
    rslt <- paste0(update, "\nSET\n", set)
    return(rslt)
}


