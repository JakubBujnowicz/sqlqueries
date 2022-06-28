load_all()


# SELECT -----------------------------------------------------------------------
(sel <- sql_select(stringr::words[1:3],
                   A = "asdasd",
                   glued = "{variable}",
                   ("parenth"),
                   vars(Variable, SV = SecondVar)))


# FROM -------------------------------------------------------------------------
(fr <- sql_from("table"))
sql_from("table", alias = "d")
sql_from(sql(select("*"),
             from("d")),
         alias = "a")


# WHERE ------------------------------------------------------------------------
(wh <- sql_where(("x = 10" %AND% "y = 11" %AND% "a = b") %OR%
                 ("z = 12" %OR%
                      "w = 13" %AND%
                      "zw = 1213"),
                 "a = 100"))


# HAVING ------------------------------------------------------------------------
(hv <- sql_having(("x = 10" %AND% "y = 11" %AND% "a = b") %OR%
                     ("z = 12" %OR%
                          "w = 13" %AND%
                          "zw = 1213"),
                 "a = 100"))


# JOIN -------------------------------------------------------------------------
(lj <- sql_join(table = "table2", type = "left",
                alias = "b",
                on = "a.KEY = b.KEY" %AND%
                     "a.KEY2 = b.KEY2"))


# CASE -------------------------------------------------------------------------
(cs <- sql_case("SPACE IS 10", 1,
                "c", 2,
                .else = 100))

# QUERY ------------------------------------------------------------------------
(qr <- sql_query(select(stringr::words[1:3]),
                 from("table", alias = "a"),
                 lj,
                 wh,
                 order_by(letters),
                 "TEST"))
sql(sel, fr, .glue = list(variable = "glued_var"))
sel + fr + wh

sql_from(qr)
sql_query(qr)
sql(from(qr))
