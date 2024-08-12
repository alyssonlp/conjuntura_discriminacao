dt <- fread(file.path(csv_output, "penal_negros.csv"))
dt <- dt[, Perda := Perda*(-1)]


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
#join_dt[, Penalidade Salarial := ]


