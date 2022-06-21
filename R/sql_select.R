sql_select <- function(..., .distinct = FALSE, .defuse = TRUE)
{
    assert_flag(.defuse)
    assert_flag(.distinct)

    x <- unlist(.sql_prepare(..., defuse = .defuse, sql_like = FALSE))
    assert_character(x, any.missing = FALSE,
                     min.chars = 1,
                     min.len = 1,
                     unique = TRUE,
                     .var.name = "...")

    nms <- names(x)
    if (!is.null(nms)) {
        ind <- is.na(nms) | nms == ""
        nms[ind] <- x[ind]
        names(x) <- nms

        assert_character(nms,
                         any.missing = FALSE,
                         unique = TRUE,
                         min.chars = 1,
                         .var.name = "names(...)")
    }

    rslt <- .new_sql(class = "sql_select",
                     tree = list(columns = x,
                                 distinct = .distinct))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @keywords internal
#'
.sql_parse.sql_select <- function(x, ...)
{
    attrs <- attributes(x)
    tree <- attrs$tree

    rslt <- tree$columns
    add_as <- names(rslt) != rslt & names(rslt) != ""
    rslt[add_as] <- paste0(rslt[add_as], " AS ", names(rslt[add_as]))
    rslt <- paste0(rslt, collapse = ",\n")

    header <- ifelse(tree$distinct, "SELECT DISTINCT", "SELECT")
    rslt <- paste(header, rslt, sep = "\n")
    rslt <- .indent(rslt, by = 4)

    attributes(rslt) <- attrs
    return(rslt)
}
