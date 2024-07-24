table_massa <-
  kable(head(massa_recente, 7), align = 'l', booktabs = TRUE, col.names = rep("", ncol(massa_recente))) %>% 
  column_spec(1, width = "300px") %>%  
  column_spec(c(2:7), width = "100px") %>% 
  row_spec(1, bold = TRUE, color = "white",background = "#d7261e") %>% 
  add_header_above(c("", "Homem" = 1, "Mulher" = 1, "Total" = 1, 
                     "Homem" = 1, "Mulher" = 1, "Total" = 1),
                   bold = TRUE, color = "white", background =    "#e96262") %>% 
  add_header_above(c("", "2023" = 3, "2024" = 3), bold = TRUE, color = "white", background = "#e96262") %>% 
  pack_rows("Devido às diferenças salariais", 2, 4) %>% 
  pack_rows("Devido às diferenças na empregabilidade", 5, 7) %>% 
  row_spec(c(2, 5), bold = TRUE, background = "#f4ccc1") %>% 
  row_spec(c(3,4, 6, 7), italic = TRUE) %>% 
  add_indent(c(3, 4, 6, 7), level_of_indent = 3)