sql_select <- function(..., distinct = FALSE)
{
    x <- unlist(.sql_defuse(..., sql_like = FALSE))
    assert_character(x, any.missing = FALSE,
                     min.chars = 1,
                     min.len = 1,
                     unique = TRUE,
                     .var.name = "...")
    assert_flag(distinct)

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
                                 distinct = distinct)) |>
        .sql_parse()
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
.sql_parse.sql_select <- function(x, level = 0,
                                  ...)
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
