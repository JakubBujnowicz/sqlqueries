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