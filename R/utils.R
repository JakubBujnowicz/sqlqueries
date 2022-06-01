#' Title
#'
#' @param obj
#' @param class
#' @param tree
#'
#' @return
#' @export
#'
#' @keywords internal
#'
.cast2sql <- function(obj, class, tree)
{
    obj_name <- deparse(substitute(obj))
    class(obj) <- c(class, "sql", "character")
    attr(obj, "tree") <- tree
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
.prefix_sql_callnames <- function(quo)
{
    assert_class(quo, classes = "quosure")

    .prefix_calls <- function(call)
    {
        nm <- rlang::call_name(call)
        if (nm %in% .sql$keywords) {
            call <- .rename_call(call, name = paste0("sql_", nm))
        }

        # Apply to further calls within 'call'
        calls <- sapply(call, rlang::is_call)
        if (sum(calls) > 0) {
            call[calls] <- lapply(call[calls], .prefix_calls)
        }

        return(call)
    }

    call <- rlang::quo_get_expr(quo)
    quo <- rlang::quo_set_expr(quo, .prefix_calls(call))

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
        str <- paste0(str, ind)
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
#' @param tree
#'
#' @return
#'
#' @keywords internal
#'
.new_sql <- function(class, tree)
{
    rslt <- structure("",
                      class = c(class, "sql", "character"),
                      tree = tree)
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
    assert_class(call, classes = "call")
    assert_string(name, min.chars = 1)

    call[[1]] <- sym(name)
    return(call)
}
