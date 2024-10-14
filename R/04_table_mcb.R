library(tidyverse)
library(gtable)
library(grid)
library(kableExtra)
options(knitr.kable.NA = '')
load("./results/nRMSEdata.RData")

comb_vec <- c("pers" = "PERS$_{bu}$",
              "3TIER" = "3TIER$_{bu}$",
              "base" = "$base$",
              "ols" = "$ols$",
              "str" = "$str$",
              "testr" = "$str_{te}$",
              "csstr" = "$str_{cs}$",
              "wlsv" = "$wlsv$",
              "ka[tcs]" = "ka$_{tcs}$",
              "ka[cst]" = "ka$_{cst}$")

tab <- nRMSEdata |>
  filter(type %in% c("none", "tilde0")) |>
  select(k, id, comb, value, h, group) |>
  group_by(k, comb, h, group) |>
  summarise(value = mean(value)) |>
  filter(h == 2) |>
  group_by(h, k, group) |>
  mutate(ming = sort(value)[1],
         ming2 = sort(value)[2],
         pers = value[comb == "pers"]) |>
  ungroup() |>
  mutate(value = cell_spec(sprintf("%.2f", value), format = "latex", bold = value == ming, 
                           italic = value == ming2,
                           color = ifelse(value>pers | is.na(value), "red", "black"))) |>
  select(-ming, -ming2, -pers, -h) |>
  mutate(comb =recode(comb, !!!comb_vec),
         comb = factor(comb, comb_vec, ordered = TRUE)) |>
  arrange(group, comb) |>
  pivot_wider(names_from = k, values_from = value, names_glue = "$k={k}$") 



ind <- setNames(as.vector(table(tab$group)), paste0("} & \\\\multicolumn{8}{c}{$\\\\mathcal{L}_", 
                                                    readr::parse_number(names(table(tab$group)))-1, "$"))


tab[,-2] |>
  kbl(format = "latex", digits = 3, booktabs = TRUE, 
      linesep = "",
      align = c("r|", "c", "c", "c", "c", "c", "c", "c", "c"),
      col.names = c("\\textbf{App.}", colnames(tab)[-c(1:2)]),
      escape = FALSE) |>
  add_header_above(c("", "Temporal aggregation orders" = 8),
                   line = FALSE,
                   escape = TRUE, bold = TRUE, line_sep = 0) |>
  pack_rows(index = ind, colnum = 1,
            indent = FALSE, escape = FALSE, latex_align = "c", bold = FALSE) |>
  save_kable(file = paste0("./table/score_nRMSE.tex"))

nemenyi_fun <- function(data){
  nemenyi <- tsutils::nemenyi(data, plottype = "none")
  df_plot <- full_join(as_tibble(nemenyi$means, rownames = "name"), 
                       full_join(rename(as_tibble(nemenyi$means-nemenyi$cd/2, rownames = "name"), "l" = "value"),
                                 rename(as_tibble(nemenyi$means+nemenyi$cd/2, rownames = "name"), "u" = "value"), 
                                 by = "name"), by = "name") |>
    arrange(value) |>
    mutate(#name = gsub(" ", "", name),
      name = paste0(name, " - ", format(round(value, 2), width = 5, nsmall = 2))) |>
    add_column(fpval = nemenyi$fpval,
               fH = nemenyi$fH)
  df_plot$col <- df_plot$l <= df_plot$u[1]
  
  as_tibble(df_plot)
}

nemg <- nRMSEdata |>
  filter(type %in% c("none", "tilde0")) |>
  select(k, id, comb, value, h) |>
  mutate(comb =recode(comb, !!!c("pers" = "PERS[bu]",
                                 "3TIER" = "3*TIER[bu]",
                                 "testr" = "str[te]",
                                 "csstr" = "str[cs]"))) |>
  group_by(k, h) |>
  nest() |>
  mutate(data = map(data, pivot_wider, names_from = comb),
         data = map(data, select, -any_of(c("id"))),
         data = map(data, nemenyi_fun)) |>
  unnest(cols = c(data)) |>
  ungroup() |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE),
         pch_name = str_detect(name, "base") | str_detect(name, "PERS") | str_detect(name, "TIER"))

nem_plot <- nemg  |> 
  filter(h == 2, k %in% c(1, 24)) |>
  mutate(facet = ifelse(k==1, "Hourly - operating day - Friedman: 0 (Ha: Different)",
                        "Daily - operating day - Friedman: 0 (Ha: Different)")) |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE)) |>
  #arrange(k) |>
  #mutate(facet = factor(facet, unique(facet), ordered = TRUE)) |>
  ggplot() + 
  geom_rect(aes(xmin=l, xmax=u, fill = col, col = col), ymin=-Inf, ymax=Inf, alpha = 0.2, 
            data = function(x) summarise(group_by(x, facet), l = min(l), col = TRUE,
                                         u = min(u), .groups = "drop"))+
  geom_segment(aes(x = l, xend = u, yend = name, y = name)) + 
  geom_point(aes(x = l, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = u, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = value, fill = col, y = name, pch = pch_name), size = 3) +
  scale_fill_manual(values = c("#F67E78", "#96D196")) + 
  scale_color_manual(values = c("#96D196")) + 
  scale_shape_manual(values=c(21, 24))+
  facet_wrap(.~facet, scales = "free", ncol = 2) + #, labeller = label_parsed)+
  labs(y = NULL, x = NULL, subtitle = NULL) + 
  theme_bw()+
  scale_y_discrete(labels = scales::label_parse())+
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(),
        legend.position = "none",
        text = element_text(size = 10),
        strip.text = element_text(face = "bold"),
        legend.margin = margin())
ggsave("./img/nem_plot.pdf", plot = nem_plot, height = 3,
       width= 8)


