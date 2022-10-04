sql_join <- function(table, on, type = "inner", alias = NULL)
{
    assert_string(table, min.chars = 1L)
    assert_string(on, min.chars = 1L)
    assert_string(type, min.chars = 1L)
    type <- tolower(type)
    assert_choice(type, choices = c("inner", "left", "right", "full", "key"))
    assert_string(alias, null.ok = TRUE,
                  min.chars = 1L)

    rslt <- .new_sql(class = "sql_join",
                     fields = list(table = table,
                                   type = type,
                                   alias = alias,
                                   condition = on))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
