# clear ws
rm(list = ls(all = TRUE))

# Load data
load("./raw/info_reco.RData")

# source file
#source("./R/varname.R")
source("./R/fun.R")

# ite ----
fls <- list.files('./results/results_ite', full.names = TRUE)
mem_ite <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  id <- as.numeric(strsplit(basename(j), "--")[[1]][2])
  
  cbind(reshape::melt(lapply(all$mem, function(x) lapply(x, as.numeric))), id = id)
})

# oct ---
fls <- list.files('./results/results_oct', full.names = TRUE)
mem_oct <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  id <- as.numeric(strsplit(basename(j), "--")[[1]][2])
  cbind(reshape::melt(lapply(all$mem, function(x) lapply(x, as.numeric))), id = id)
})


# ka ---
fls <- list.files('./results/results_ka', full.names = TRUE)
mem_ka <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  id <- as.numeric(strsplit(basename(j), "--")[[1]][2])
  cbind(reshape::melt(lapply(all$mem, function(x) lapply(x, as.numeric))), id = id)
})



mem_ite <- tibble(do.call(rbind, mem_ite)) |>
  add_column(type = "ite")

mem_oct <- tibble(do.call(rbind, mem_oct)) |>
  add_column(type = "oct")

mem_ka <- tibble(do.call(rbind, mem_ka)) |>
  add_column(type = "ka")

mem <- rbind(mem_oct, mem_ite, mem_ka) %>%
  mutate(name = paste0(type, ifelse(L1 == "free", "", paste0("[", L1, "]")))) |>
  filter(#L1 %in% c("tcs", "free"), 
         L2 %in% c("str","ols","wlsv", "ols-ols", "str-str", "str-ols", 
                   "ols-str", "csstr", "testr", "wlsv-wls")) %>%
  mutate(L2 = recode(L2, "ols-ols"="ols", "str-str"="str", "ols-str"="csstr", 
                     "str-ols"="testr", "wlsv-wls"="wlsv")) |>
  arrange(id) |>
  mutate(name = factor(name, c("oct", "ite[tcs]", "ite[cst]", "ka[tcs]", "ka[cst]"), ordered = TRUE),
         L2 = factor(L2, c("ols", "str", "csstr", "testr", "wlsv"), ordered = TRUE)) |>
  mutate(L2 = recode(L2, !!!c("testr" = "str[te]",
                              "csstr" = "str[cs]")))


mem_plot <- mem %>%
  mutate(value = value/as.numeric(bench::bench_bytes("1MB")),
         Approach = paste0("bold(Approach):~", L2)) |>
  #mutate(L2 = "Boxplot") %>%
  ggplot(aes(y = name, x = as.numeric(value), col = name, fill = name, group = interaction(L2, name)))+ 
  facet_wrap(.~Approach, scales = "free_x", nrow = 2, labeller = label_parsed)+
  #facet_grid(.~L2, scales = "free")+
  geom_boxplot(alpha = 0.2, outlier.alpha = 1)+
  labs(y = NULL, x = "Measure memory in MB") + 
  scale_color_manual(values = c(scales::hue_pal()(5)))+
  scale_fill_manual(values = c(scales::hue_pal()(5)))+ 
  scale_y_discrete(label = function(x) parse(text = x))+
  #scale_x_discrete(expand = c(0.3,0.3), labels = sapply(L1_expression, function(i) parse(text=i)))+
  #scale_y_continuous(position = "right", breaks = seq(10, 25, 5))+
  #scale_y_log10(breaks = c(0.06, 0.2, 0.4, 1, 2, 5, 10, 17), position = "right")+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        #axis.text.x = element_text(color = "white"),
        #axis.text.x = element_blank(),
        #axis.ticks.y = element_line(),
        #axis.ticks.x = element_blank(),
        #plot.margin = margin(l = 0, r = 5.5, b = 5.5, t = 2.5),
        #panel.grid.major.x = element_blank(),
        #panel.border = element_rect(fill = NA, color = "black"),
        #panel.border = element_blank(),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_text(color = "black", face = "bold", size = 10),
        text = element_text(size = 10))

dir.create("./img", recursive = TRUE, showWarnings = FALSE)
ggsave("./img/mem_plot.pdf", plot = mem_plot, height = 4,
       width= 8)