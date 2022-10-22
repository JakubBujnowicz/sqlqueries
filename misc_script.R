load_all()


# CASE -------------------------------------------------------------------------
(cs <- sql_case("SPACE IS 10", 1,
                "c", 2,
                .else = 100))


# SELECT -----------------------------------------------------------------------
(sel <- sql_select(stringr::words[1:3],
                   A = "asdasd",
                   glued = "{variable}",
                   ("parenth"),
                   Case = cs,
                   vars(Variable, SV = SecondVar),
                   .distinct = TRUE,
                   .top_n = 10))


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
                 "a" %IN% 1:5))


# JOIN -------------------------------------------------------------------------
(lj <- sql_join(table = "table2", type = "left",
                alias = "b",
                on = "a.KEY = b.KEY" %AND%
                     "a.KEY2 = b.KEY2"))

# ORDER BY ---------------------------------------------------------------------
(ob <- sql_order_by(stringr::words[1:3]))


# GROUP BY ---------------------------------------------------------------------
(gb <- sql_group_by(stringr::words[4:6]))


# INSERT -----------------------------------------------------------------------
dt <- iris[sample(seq_len(nrow(iris)), 10), ]
dt[1:2, 1] <- c(pi + 10, NA)
dt$Sepal.Width <- rbinom(10, 10, runif(10))
(ins <- sql_insert(into = "tab",
                   values = dt,
                   columns = sample(names(dt), 3)))


(ins2 <- sql_insert(into = "tab",
                    query = sel + fr + wh))


# UPDATE -----------------------------------------------------------------------
(upd <- sql_update(table = "tab",
                   set = list(Date = Sys.Date(),
                              TimeDate = Sys.time(),
                              Number = 1,
                              Pi = pi,
                              Name = "Qwerty")))
sql(upd, wh)

# QUERY ------------------------------------------------------------------------
(qr <- sql_query(select(stringr::words[1:3]),
                 from("table", alias = "a"),
                 lj,
                 wh,
                 ob,
                 gb,
                 "TEST"))
sql(sel, fr, .glue = list(variable = "glued_var"))
sel + fr + wh

sql_from(qr)
sql_query(qr)
sql(from(qr, alias = "A"))
