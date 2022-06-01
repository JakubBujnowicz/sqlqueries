load_all()


# SELECT -----------------------------------------------------------------------
(sel <- sql_select(stringr::words[1:3],
                   A = "asdasd",
                   vars(Variable, SV = SecondVar)))

# FROM -------------------------------------------------------------------------
(fr <- sql_from("table"))
sql_from("table", alias = "d")
sql_from(sql(select("*"),
             from("d")),
         alias = "a")

# WHERE ------------------------------------------------------------------------
(wh <- sql_where(and("x = 10",
                     "y = 11") %OR%
                     and("z = 12",
                         "w = 13"),
                 "a = 100")
)

# QUERY ------------------------------------------------------------------------
(qr <- sql_query(select(stringr::words[1:3]),
                 from("table", alias = "a"),
                 wh,
                 "TEST"))
sql(sel, fr)
sel + fr + wh

sql_from(qr)
sql_query(qr)
sql(from(qr))

