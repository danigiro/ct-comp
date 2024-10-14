# clear ws
rm(list = ls(all = TRUE))

# Load data
library(tidyverse)
library(ggformula)
library(ggpubr)
load("./raw/info_reco.RData")

# source file
#source("./R/varname.R")
source("./R/fun.R")

# ite ----
fls <- list.files('./results/results_ite', full.names = TRUE)
time_ite <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  id <- as.numeric(strsplit(basename(j), "--")[[1]][2])
  cbind(reshape::melt(all$time), id = id)
})

# oct ---
fls <- list.files('./results/results_oct', full.names = TRUE)
time_oct <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  id <- as.numeric(strsplit(basename(j), "--")[[1]][2])
  cbind(reshape::melt(all$time), id = id)
})


# ka ---
fls <- list.files('./results/results_ka', full.names = TRUE)
time_ka <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  id <- as.numeric(strsplit(basename(j), "--")[[1]][2])
  cbind(reshape::melt(all$time), id = id)
})


time_ite <- tibble(do.call(rbind, time_ite)) |>
  add_column(type = "ite")

time_oct <- tibble(do.call(rbind, time_oct)) |>
  add_column(type = "oct")

time_ka <- tibble(do.call(rbind, time_ka)) |>
  add_column(type = "ka")

time <- rbind(time_oct, time_ite, time_ka) %>%
  mutate(name = paste0(type, ifelse(L1 == "free", "", paste0("[", L1, "]")))) |>
  filter(#L1 %in% c("tcs", "free"), 
    L2 %in% c("str","ols","wlsv", "ols-ols", "str-str", "str-ols", 
              "ols-str", "csstr", "testr", "wlsv-wls")) %>%
  mutate(L2 = recode(L2, "ols-ols"="ols", "str-str"="str", "ols-str"="csstr", 
                     "str-ols"="testr", "wlsv-wls"="wlsv")) |>
  arrange(id) |>
  mutate(name = factor(name, c("oct", "ite[tcs]", "ite[cst]", "ka[tcs]", "ka[cst]"), 
                       ordered = TRUE)) |>
  mutate(L2 = recode(L2, !!!c("testr" = "str[te]",
                                                    "csstr" = "str[cs]")))

pp <- NULL
for(i in unique(time$L2)){
  p1 <- time |>
    filter(L2 == i) |>
    add_column(facet = "Time (s) for each replication with smoothed line") %>%
    ggplot(aes(x = id, y = as.numeric(value), col = name)) +
    #geom_line(alpha = 0.75, size = 0.25)+
    geom_point(alpha = 0.75, size = 0.1)+
    #geom_smooth(method = 'loess', formula = 'y ~ x', se = FALSE)+
    geom_spline(spar = 0.5)+
    #facet_wrap(.~L2, scales = "free")+
    scale_color_manual(values = c(scales::hue_pal()(5)))+
    scale_fill_manual(values = c(scales::hue_pal()(5)))+
    #scale_linetype_manual(values = 1:2, labels = L1_expression)+
    #scale_y_continuous(breaks = seq(0, 400, 100))+
    scale_x_continuous(breaks = seq(0, 350, 70))+
    scale_y_log10(breaks = c(0.06, 0.2, 0.4, 1, 2, 5, 10, 17))+
    #scale_y_log10(breaks = 10^seq(1, 2.60206, by = 0.01))+
    ylab("Seconds") + 
    xlab("Replication")+
    theme_minimal()+
    theme(plot.margin = margin(r = 0, l = 5.5, b = 5.5, t = 2.5),
          legend.title = element_blank(),
          legend.position = "none",
          panel.border = element_rect(fill = NA, color = "black"),
          axis.ticks = element_line(),
          strip.background = element_rect(fill = NA, color = NA),
          strip.text = element_text(color = "black", face = "bold", size = 10),
          text = element_text(size = 10))
  
  p2 <- time %>%
    filter(L2 == i) |>
    #mutate(L2 = "Boxplot") %>%
    ggplot(aes(x = name, y = as.numeric(value), col = name, fill = name, group = interaction(L2, name)))+ 
    #facet_wrap(.~L2, scales = "free")+
    geom_boxplot(alpha = 0.2, outlier.alpha = 1)+
    xlab("") +
    labs(y = NULL) + 
    scale_color_manual(values = c(scales::hue_pal()(5)))+
    scale_fill_manual(values = c(scales::hue_pal()(5)))+ 
    #scale_x_discrete(expand = c(0.3,0.3), labels = sapply(L1_expression, function(i) parse(text=i)))+
    #scale_y_continuous(position = "right", breaks = seq(10, 25, 5))+
    scale_y_log10(breaks = c(0.06, 0.2, 0.4, 1, 2, 5, 10, 17), position = "right")+
    scale_x_discrete(label = function(x) parse(text = x))+
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          #axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
          #axis.text.x = element_blank(),
          axis.ticks.y = element_line(),
          axis.ticks.x = element_line(),
          plot.margin = margin(l = 0, r = 5.5, b = 4.5, t = 2.5),
          panel.grid.major.x = element_blank(),
          #panel.border = element_rect(fill = NA, color = "black"),
          panel.border = element_blank(),
          strip.background = element_rect(fill = NA, color = NA),
          strip.text = element_text(color = "black", face = "bold", size = 10),
          text = element_text(size = 10))
  
  
  pp[[i]] <- gridExtra::arrangeGrob(grobs = list(p1, p2), widths = c(2.75,1.25), 
                                    #top = expression(italic(Tamarix)~"Green Foliage %"),
                                    #top = bquote("Approach:~"*.(i)),
                                    top = grid::textGrob(parse( text = paste0("bold(Approach):~", i)), 
                                                         gp=grid::gpar(fontsize=10, bold=TRUE)), 
                                    ncol=2)
  
}

ppcom <- gridExtra::grid.arrange(grobs = pp[c("ols", "str", "str[cs]", "str[te]", "wlsv")], ncol = 1)
ggsave("./img/time_complete.pdf", plot = ppcom, height = 8,
       width= 8)

time_plot <- time %>%
  mutate(#value = value/as.numeric(bench::bench_bytes("1MB")),
         Approach = paste0("bold(Approach):~", L2)) |>
  #mutate(L2 = "Boxplot") %>%
  ggplot(aes(y = name, x = as.numeric(value), col = name, fill = name, group = interaction(L2, name)))+ 
  facet_wrap(.~Approach, scales = "free_x", nrow = 2, labeller = label_parsed)+
  #facet_grid(.~L2, scales = "free")+
  geom_boxplot(alpha = 0.2, outlier.alpha = 1)+
  labs(y = NULL, x = "Time in Seconds") + 
  scale_color_manual(values = c(scales::hue_pal()(5)))+
  scale_fill_manual(values = c(scales::hue_pal()(5)))+ 
  scale_y_discrete(label = function(x) parse(text = x))+
  #scale_x_discrete(expand = c(0.3,0.3), labels = sapply(L1_expression, function(i) parse(text=i)))+
  #scale_y_continuous(position = "right", breaks = seq(10, 25, 5))+
  #scale_y_log10(breaks = c(0.06, 0.2, 0.4, 1, 2, 5, 10, 17), position = "right")+
  theme_bw() +
  scale_x_log10(breaks = c(0.06, 0.2, 0.4, 1, 2, 5, 10, 17))+
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

ggsave("./img/time_summary.pdf", plot = time_plot, height = 4,
       width= 8)

