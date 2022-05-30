load_all()


# SELECT -----------------------------------------------------------------------
(sel <- sql_select(stringr::words[1:3],
                   A = "asdasd",
                   vars(Variable, SV = SecondVar)))

# FROM -------------------------------------------------------------------------
(fr <- sql_from("table"))
sql_from("table", alias = "dupa")
sql_from(sql(select("*"),
             from("dupa")),
         alias = "a")

# WHERE ------------------------------------------------------------------------
(wh <- sql_where("x = 10" %AND% "y = 11" %OR% "z = 12" %AND% "w = 13"))

# QUERY ------------------------------------------------------------------------
(qr <- sql_query(select(stringr::words[1:3]),
                 from("table", alias = "a"),
                 wh,
                 "TEST"))
sql(sel, fr)
sel + fr

sql_from(qr)
sql_query(qr)
sql(from(qr))
