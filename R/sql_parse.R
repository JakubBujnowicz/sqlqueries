# Outer function for parsing ---------------------------------------------------
.sql_parse <- function(x, ...)
{
    assert_string(x)

    if (!is_sql(x)) {
        return(x)
    }

    message("Parsing: ", .mclass(x), "...")

    attrs <- attributes(x)
    rslt <- .parse(x = x, fields = attrs$fields, ...)
    attributes(rslt) <- attrs
    return(rslt)
}


# Inner parser with methods for each class -------------------------------------
#' Title
#'
#' @param x
#' @param fields
#' @param ...
#'
#' @return
#' @export
#'
#' @name sql_parse
#'
#' @keywords internal
#'
.parse <- function(x, fields, ...)
{
    UseMethod(".parse")
}

# CASE -------------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
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


# FROM -------------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_from <- function(x, fields, ...)
{
    rslt <- fields$table
    if (inherits(rslt, "sql_query")) {
        rslt <- .add_parenth(rslt)
    }

    tab_name <- names(fields$table)
    if (!is.null(fields$alias)) {
        rslt <- paste0(rslt, " AS ", fields$alias)
    } else if (test_string(tab_name, min.chars = 1)) {
        rslt <- paste0(rslt, " AS ", tab_name)
    }

    rslt <- paste("FROM", rslt, sep = "\n")
    rslt <- .indent(rslt, by = 4)

    return(rslt)
}


# GROUP BY ---------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_group_by <- function(x, fields, ...)
{
    header <- "GROUP BY"
    rslt <- paste(header, .columns_parser(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


# HAVING -----------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_having <- function(x, fields, ...)
{
    rslt <- paste("HAVING", fields$condition, sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


# IN ---------------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_in <- function(x, fields, ...)
{
    vec <- unique(fields$vector)
    if (length(vec) > 1) {
        vec <- sql_tuple(vec)
    }

    rslt <- paste(fields$x, "IN", vec)
    return(rslt)
}


# INSERT -----------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_insert <- function(x, fields, ...)
{
    vals <- as.list(fields$values)
    cols <- fields$columns

    if (!is.null(cols)) {
        vals <- vals[cols]
        cols <- paste0("(", toString(cols), ")")
    } else {
        cols <- ""
    }

    target <- paste0("INSERT INTO\n", fields$into, " ", cols)

    if (!is.null(fields$query)) {
        what <- fields$query
    } else {
        vals <- do.call(sql_tuple, args = vals)
        vals <- .sql_parse(vals, as_values = TRUE)
        what <- paste0("VALUES\n", .indent(vals, by = 4, indent_first = TRUE))
    }

    rslt <- paste0(.indent(target, by = 4), "\n", what)
    return(rslt)
}




# JOIN -------------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_join <- function(x, fields, ...)
{
    tab <- fields$table
    if (inherits(tab, "sql_query")) {
        tab <- .add_parenth(tab)
    }

    tab_name <- names(fields$table)
    if (!is.null(fields$alias)) {
        tab <- paste0(tab, " AS ", fields$alias)
    } else if (test_string(tab_name, min.chars = 1L)) {
        tab <- paste0(tab, " AS ", tab_name)
    }

    joinstr <- paste0(toupper(fields$type), " JOIN")
    # Indent by ON + space
    rslt <- list(sep = "\n", joinstr, tab)
    if (fields$type != "key") {
        condition <- paste0("ON ", .indent(fields$condition, by = 3L))
        rslt <- c(rslt, condition)
    }
    rslt <- do.call(paste, args = rslt)
    rslt <- .indent(rslt, by = 4L)
    return(rslt)
}


# Logical ----------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_logical <- function(x, fields, break_lines = TRUE, ...)
{
    n <- length(fields$operators)

    sep <- ifelse(break_lines, "\n", " ")
    sep <- paste0(sep, toupper(fields$operators), " ")

    rslt <- fields$elements[[1]]
    for (i in seq_len(n)) {
        curr <- fields$elements[[i + 1]]
        if (inherits(curr, "sql_parenth")) {

            # Indent by the width of the operator
            curr <- .indent(curr, by = nchar(sep[i]) - 1)
        }

        rslt <- paste0(rslt, sep[i], curr)
    }

    return(rslt)
}


# ORDER BY ---------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_order_by <- function(x, fields, ...)
{
    header <- "ORDER BY"
    rslt <- paste(header, .columns_parser(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


# Parenth(esis) ----------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_parenth <- function(x, fields, ...)
{
    contains <- fields$contains

    if (inherits(contains, "sql_logical")) {
        operators <- attr(contains, "fields", exact = TRUE)$operators
        n <- length(operators)
        contains <- .sql_parse(contains, break_lines = n >= 2)
    }

    rslt <- paste0("(", contains, ")", collapse = "")
    rslt <- .indent(rslt, by = 1)
    return(rslt)
}


# Query ------------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_query <- function(x, fields, ...)
{
    rslt <- do.call(paste0, args = list(fields, collapse = "\n"))
    return(rslt)
}


# SELECT -----------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_select <- function(x, fields, ...)
{
    header <- "SELECT"
    if (fields$distinct) {
        header <- paste(header, "DISTINCT")
    }
    if (!is.null(fields$top_n)) {
        header <- paste0(header, " TOP ", fields$top_n)
    }

    rslt <- paste(header, .columns_parser(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


# Tuple ------------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
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


# UPDATE -----------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_update <- function(x, fields, ...)
{
    update <- paste0("UPDATE\n", fields$table)
    update <- .indent(update, by = 4)

    .prepare <- function(x)
    {
        if (is.na(x) || is.null(x)) {
            x <- "NULL"
        } else if (!is.numeric(x)) {
            x <- paste0("'", x, "'")
        }

        return(x)
    }

    set <- lapply(fields$set, .prepare)

    set <- paste(format(names(set)), "=", set,
                  collapse = ",\n")
    set <- .indent(set, by = 4, indent_first = TRUE)
    rslt <- paste0(update, "\nSET\n", set)
    return(rslt)
}


# WHERE ------------------------------------------------------------------------
#' @rdname sql_parse
#' @keywords internal
#'
.parse.sql_where <- function(x, fields, ...)
{
    rslt <- paste("WHERE", fields$condition, sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}
