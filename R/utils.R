#' Title
#'
#' @param str
#' @param indent
#'
#' @return
#' @export
#'
#' @examples
.add_parenth <- function(str, indent = TRUE)
{
    attrs <- attributes(str)

    if (indent) {
        str <- .indent(str, by = 1)
    }

    str <- paste0("(", str, ")")
    attributes(str) <- attrs
    # attr(str, "add_parenth") <- TRUE
    return(str)
}



#' Title
#'
#' @param obj
#' @param class
#' @param fields
#'
#' @return
#' @export
#'
#' @keywords internal
#'
.cast2sql <- function(obj, class, fields)
{
    obj_name <- deparse(substitute(obj))
    class(obj) <- c(class, "sql", "character")
    attr(obj, "fields") <- fields
    assign(x = obj_name,
           value = obj,
           envir = parent.frame())
}


#' Title
#'
#' @param quo
#'
#' @return
#'
#' @keywords internal
#'
.defuse_calls <- function(quo)
{
    assert_class(quo, classes = "quosure")

    .cancel_defusing <- function(call)
    {
        nm <- rlang::call_name(call)
        call <- rlang::call_match(call, fn = get(nm), defaults = TRUE)
        call$`.defuse` <- FALSE
        return(call)
    }

    .defuse <- function(call)
    {
        nm <- rlang::call_name(call)
        if (!is.null(nm) && nm %in% .sql$keywords) {
            call <- .rename_call(call, name = paste0("sql_", nm))
        } else if (!is.null(nm) && nm == "(") {
            call <- .rename_call(call, ".sql_parenth")
        }

        # First defusing takes care of every call in the call fields,
        # so no need to do it again
        defusable <- rlang::is_call(call, name = .sql$defusables)
        if (defusable) {
            call <- .cancel_defusing(call)
        }

        # Apply to further calls within 'call'
        calls <- sapply(call, rlang::is_call,
                        name = c(.sql$allfuns, .sql$keywords, "("))
        if (sum(calls) > 0) {
            call[calls] <- lapply(call[calls], .defuse)
        }

        return(call)
    }

    call <- rlang::quo_get_expr(quo)
    quo <- rlang::quo_set_expr(quo, .defuse(call))

    return(quo)
}


#' Title
#'
#' @param str
#' @param by
#' @param indent_first
#'
#' @return
#'
#' @keywords internal
#'
.indent <- function(str, by, indent_first = FALSE)
{
    assert_string(str)
    assert_count(by)
    assert_flag(indent_first)

    ind <- strrep(" ", by)
    str <- str_replace_all(str, "\n", paste0("\n", ind))

    if (indent_first) {
        str <- paste0(ind, str)
    }

    return(str)
}


#' Title
#'
#' @param x
#' @param what
#' @param where
#'
#' @return
#' @export
#'
#' @keywords internal
#'
.replace_list <- function(x, what, where)
{
    # Assertions
    assert_list(x)
    n <- length(x)
    assert_list(what)
    k <- length(what)
    assert_integerish(where, lower = 1L, upper = n,
                      len = k)

    lens <- lengths(what)
    lens <- c(0, lens[-k])
    where_app <- where + cumsum(lens)

    for (i in where_app)
    {
        j <- match(i, where_app)
        x <- append(x, what[[j]], after = i)
    }

    x <- x[-where_app]
    return(x)
}


#' Main class of an object
#'
#' @param x
#'
#' @return
#'
#' @keywords internal
#'
.mclass <- function(x)
{
    class(x)[1]
}


#' Title
#'
#' @param class
#' @param fields
#'
#' @return
#'
#' @keywords internal
#'
.new_sql <- function(class, fields)
{
    rslt <- structure("",
                      class = c(class, "sql", "character"),
                      fields = fields)
    return(rslt)
}


#' Title
#'
#' @param call
#' @param name
#'
#' @return
#'
#' @keywords internal
#'
.rename_call <- function(call, name)
{
    assert_multi_class(call, classes = c("call", "("))
    assert_string(name, min.chars = 1)

    call[[1]] <- sym(name)
    return(call)
}
