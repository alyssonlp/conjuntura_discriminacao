dt <- fread(file.path(csv_output, "premio_brancos.csv"))

dt1 <- dt[Ano_trimestre %in% c("2023T1"),]
dt1 <- setnames(dt1, c("Decomposição", "Perda"), c("Decomposição2023", "Perda2023"))
dt1[, Ano_trimestre := NULL]
dt2 <- dt[Ano_trimestre %in% c("2024T1"),]
dt2 <- setnames(dt2, c("Decomposição", "Perda"), c("Decomposição2024", "Perda2024"))
dt2[, Ano_trimestre := NULL]

join_dt <- cbind(dt1, dt2)
join_dt[,Decomposição2024 := NULL]
join_dt <- setnames(join_dt, c("Decomposição2023", "Perda2023", "Perda2024"),
                    c("", "2023", "2024"))

premio_brancos <-
  kable(head(join_dt, 3), align = 'l', booktabs = TRUE, 
        col.names = rep("", ncol(join_dt)), format = 'latex') %>% 
  column_spec(1, width = "250px") %>%  
  column_spec(c(2:7), width = "60px") %>% 
  row_spec(c(1:3), color = "black") %>% 
  add_indent(c(2,3), level_of_indent = 2) 

writeLines(premio_brancos, file.path(outputs, "premio_brancos.tex"))
