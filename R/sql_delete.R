sql_delete <- function(from, ..., where = NULL, .defuse = TRUE)
{
    assert_string(from, min.chars = 1L)
    assert_class(where, classes = "sql_where", null.ok = TRUE)
    assert_flag(.defuse)

    if (...length() > 0) {
        condition <- sql_condition(..., .defuse = .defuse)
    } else {
        condition <- NULL
    }

    rslt <- .new_sql(class = "sql_delete",
                     fields = list(from = from,
                                   where = where,
                                   condition = condition))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



.parse.sql_delete <- function(x, fields, ...)
{
    wh_cond <- attr(fields$where, "fields")$condition
    dots_cond <- fields$condition
    cond <- list(wh_cond, dots_cond)

    # Remove empty
    cond <- cond[!sapply(cond, is.null)]
    if (length(cond) > 0) {
        cond <- do.call(sql_condition, args = cond)
    }

    rslt <- paste("DELETE FROM", fields$from)
    if (length(cond) > 0) {
        cond <- paste("WHERE", cond, sep = "\n")
        cond <- .indent(cond, by = 4)

        rslt <- paste0(rslt, "\n", cond)
    }
    return(rslt)
}