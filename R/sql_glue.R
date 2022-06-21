sql_glue <- function(sql, .x, ...)
{
    assert(check_class(sql, classes = "sql"),
           check_string(sql, min.chars = 1),
           combine = "or")

    attrs <- attributes(sql)

    rslt <- glue::glue_data(.x = .x,
                            .trim = FALSE,
                            sql,
                            ...)

    attributes(rslt) <- attrs
    return(rslt)
}
