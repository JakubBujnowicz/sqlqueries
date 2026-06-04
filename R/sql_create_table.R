#' Construct a SQL CREATE TABLE statement
#'
#' Builds a CREATE TABLE statement. New tables can be defined by providing
#' column names and their types, or created from the results of a SELECT
#' query using `CREATE TABLE ... AS`.
#'
#' @param name a single string with the name of the table to be created.
#' @param variables a named character vector. Vector names represent names of
#'   the columns, whereas vector values represent column types (e.g. integer).
#'   Names must be unique.
#' @param query an [sqlqueries::sql_query()] object, query used as a basis
#'   for the new table.
#'
#' @return A character string representing the SQL CREATE TABLE statement, with S3
#'   class 'sql_create_table'.
#' @export
#'
#' @family dml
#'
#' @examples
#' sql_create_table("new_table",
#'                  variables = c(col1 = "integer", col2 = "text"))
#' q <- sql(select("*"),
#'          from("original"))
#' sql_create_table("new_table", query = q)
#'
sql_create_table <- function(name, variables = NULL, query = NULL)
{
    if (!xor(is.null(variables), is.null(query))) {
        stop("'variables' and 'query' cannot both be filled in a single ",
             "CREATE TABLE statement")
    }
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_character(variables, min.chars = 1L, min.len = 1L,
                                any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_class(query, classes = "sql_query", null.ok = TRUE)
    if (!is.null(variables)) {
        checkmate::assert_names(names(variables), type = "unique")
    }

    rslt <- .new_sql(class = "sql_create_table",
                     fields = list(name = name,
                                   variables = variables,
                                   query = query))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



#' Internal parser for `sql_create_table` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_create_table <- function(x, fields, ...)
{
    query <- fields$query
    vars <- fields$variables

    rslt <- paste("CREATE TABLE", fields$name)
    if (!is.null(query)) {
        rslt <- paste0(rslt, " AS \n",
                       .indent(query, by = 4L, indent_first = TRUE))
    } else {
        vars <- paste(format(names(vars)),
                      toupper(vars),
                      collapse = ",\n")
        vars <- .indent(vars, by = 4L, indent_first = TRUE)
        rslt <- paste0(rslt, " (\n", vars, "\n)")
    }
    return(rslt)
}


