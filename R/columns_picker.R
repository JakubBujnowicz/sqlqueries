.columns_picker <- function(..., .defuse = TRUE)
{
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

    return(x)
}

.columns_parser <- function(cols)
{
    rslt <- format(cols)
    add_as <- names(cols) != cols & names(cols) != ""
    rslt[add_as] <- paste0(rslt[add_as], " AS ", names(rslt[add_as]))
    rslt <- paste0(trimws(rslt, which = "right"), collapse = ",\n")
    return(rslt)
}
