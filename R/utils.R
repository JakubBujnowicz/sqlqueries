#' Add parentheses around a string
#'
#' Meant for modifying `sql` objects safely (preserving attributes) and wrapping
#' them in parentheses additionally indenting where necessary.
#'
#' @param str a single string, often `sql` objects.
#' @param indent a logical value, decides whether indentation should match the
#'   newly created parentheses.
#'
#' @return The `str` string wrapped with parentheses.
#' @keywords internal
#'
.add_parenth <- function(str, indent = TRUE)
{
    attrs <- attributes(str)

    if (indent) {
        str <- .indent(str, by = 1)
    }

    str <- paste0("(", str, ")")
    attributes(str) <- attrs
    return(str)
}


#' Align lengths of the last lines to the longest string
#'
#' This function adds spaces at the end of the last line of every string
#' in `strings`, so that the last line has the same amount of characters
#' as the longest line in ALL strings in `strings`.
#' This is useful for aligning "AS" statements, e.g. in SELECT.
#'
#' Significance of aligning the last stems from possible multiline expressions,
#' possibly not strictly necessary in most cases.
#'
#' @param strings a character vector of strings to align.
#'
#' @return The character vectors with spaces added at the end (if necessary).
#' @keywords internal
#'
.align_lines <- function(strings)
{
    splt <- strsplit(strings, split = "\n")
    chars <- lapply(splt, nchar)

    max_chars <- max(unlist(chars))
    last_line_chars <- sapply(chars, tail, 1L)
    fill <- max_chars - last_line_chars

    spaces <- strrep(" ", fill)
    rslt <- paste0(strings, spaces)
    return(rslt)
}


#' Prepare a vector
#'
#' This prepares atomic vectors to be used e.g. in tuples or INSERT statements.
#' Numerics are always converted to non-scientific version, with precision as long
#' as possible. `NA` values are replaced with "NULL" strings.
#' If necessary, alignment is applied, i.e. numerics to the right, characters to
#' the left, with constant length, so that the output is human readable.
#'
#' @param x an atomic vector to align.
#' @param short a logical value, whether this is meant to be as short as possible.
#'   That means trimming whitespaces and dropping trailing zeros. Short version
#'   is used in tuples, whereas longer in INSERTs.
#'
#' @return A character vector based on `x`.
#' @keywords internal
#'
.prepare_vector <- function(x, short = FALSE)
{
    nas <- is.na(x)
    rslt <- format(x, digits = abs(floor(log10(.Machine$double.eps)) + 1),
                   scientific = FALSE,
                   trim = short,
                   drop0trailing = short)

    if (any(nas)) {
        rslt[nas] <- "NULL"

        if (!short) {
            just <- ifelse(is.numeric(x), "right", "left")
            rslt <- format(rslt, justify = just)
        }
    }

    return(rslt)
}


#' Indent a string
#'
#' This takes a string `str` and indents every line (separating by `\n` signs")
#' by a certain amount of spaces (`by`).
#'
#' @param str a string to indent.
#' @param by a positive integer, amount of spaces to indent by.
#' @param indent_first a logical value, whether the first line should be indented
#'   as well.
#'
#' @return The string with indented lines.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'     string <- "The first line\nAnd one more\nAnd the last"
#'     cat(string, "\n\n")
#'
#'     cat(.indent(string, by = 4), "\n\n")
#'     cat(.indent(string, by = 2, indent_first = TRUE))
#' }
#'
.indent <- function(str, by, indent_first = FALSE)
{
    checkmate::assert_string(str)
    checkmate::assert_count(by)
    checkmate::assert_flag(indent_first)

    ind <- strrep(" ", by)
    str <- str_replace_all(str, "\n", paste0("\n", ind))

    if (indent_first) {
        str <- paste0(ind, str)
    }

    return(str)
}


#' Replace elements of a list with another list
#'
#' This takes a list `x` and replaces its elements in indices `where` with
#' all elements of `what` list.
#'
#' @param x a list.
#' @param what a list with replacement values. All values will be put into `x`.
#'   Atomic vectors are coerced to lists and appended, hence it is possible
#'   for the output vector to be longer than `x`.
#' @param where an integer vector of indices of `x` list to be replaced.
#'
#' @return The list `x` with replaced elements.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'     x <- as.list(letters[1:6])
#'     what <- list(1:3, sum)
#'     where <- c(2, 5)
#'     .replace_list(x, what = what, where = where)
#' }
#'
.replace_list <- function(x, what, where)
{
    # Assertions
    checkmate::assert_list(x)
    n <- length(x)
    checkmate::assert_list(what)
    k <- length(what)
    checkmate::assert_integerish(where, lower = 1L, upper = n,
                                 len = k)

    lens <- lengths(what)
    lens <- c(0, lens[-k])
    where_app <- where + cumsum(lens)

    for (i in where_app) {
        j <- match(i, where_app)
        x <- append(x, what[[j]], after = i)
    }

    x <- x[-where_app]
    return(x)
}


#' Main class of an object
#'
#' @param x an object to extract the class from.
#'
#' @return The first class of the object, as given by `class()`.
#' @keywords internal
#'
.main_class <- function(x)
{
    class(x)[1]
}


#' Create a new `sql` object
#'
#' Creates an empty string with a proper structure:
#' * Must have at least three classes --- main `sql_` class, general `sql` class
#'   and `character` class.
#' * Must have a `field` attribute with all relevant data for object parsing.
#'
#' The object is modified into a proper string (non-empty) only during parsing.
#'
#' @param class a single string, the main class of a `sql` object, e.g. `"sql_select"`.
#' @param fields
#'
#' @return An empty string with a proper `sql` object structure, ready for parsing.
#' @keywords internal
#'
.new_sql <- function(class, fields)
{
    rslt <- structure("",
                      class = c(class, "sql", "character"),
                      fields = fields)
    return(rslt)
}


#' Change the name of a call without modifying arguments
#'
#' @param call a call to be modified.
#' @param name a single string, new name for the call.
#'
#' @return The call with a modified name.
#' @keywords internal
#'
.rename_call <- function(call, name)
{
    checkmate::assert_multi_class(call, classes = c("call", "("))
    checkmate::assert_string(name, min.chars = 1)

    call[[1]] <- sym(name)
    return(call)
}


