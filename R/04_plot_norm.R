# clear ws
rm(list = ls(all = TRUE))

# Load data
load("./raw/info_reco.RData")

# source file
#source("./R/varname.R")
source("./R/fun.R")

# ite ----
fls <- list.files('./results/results_ite_tol', full.names = TRUE)
data <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  out <- tibble(tcs = all$tcs, cst = all$tcs, tol = names(all$tcs))

  all <- mget(load(paste0("./results/results_oct/",
                          str_replace(basename(j), "ite", "oct")), envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  out$oct <- list(all$free$wlsv)
  out$id <- as.numeric(strsplit(basename(j), "--")[[1]][2])
  out
})

data_norm <- bind_rows(data)|>
  group_by(id, tol) |>
  summarise(norm_tcs = unlist(Map(function(x, y) norm(x-y, "F"), x = tcs, y = oct)),
            norm_cst = unlist(Map(function(x, y) norm(x-y, "F"), x = cst, y = oct)))|>
  mutate(tol = factor(tol, ordered = TRUE),
         tol = recode(tol, "1e-05" = "10^{-5}", "1e-06" = "10^{-6}", "1e-08" = "10^{-8}", "1e-10" = "10^{-10}"))

p1 <- data_norm |>
  filter(tol != "10^{-10}") |>
  ggplot(aes(y = norm_tcs, x = id, col = tol)) + 
  geom_line() +
  ylab(expression("Frobenius norm between oct and ite"["tcs"])) + 
  xlab("Replication")+
  scale_color_manual(values = c(scales::hue_pal()(3)))+
  scale_y_log10()+
  theme_minimal()+
  theme(plot.margin = margin(r = 0, l = 5.5, b = 5.5, t = 2.5),
        legend.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(fill = NA, color = "black"),
        axis.ticks = element_line(),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_text(color = "black", face = "bold", size = 10),
        text = element_text(size = 10))

p2 <- data_norm %>%
  filter(tol != "10^{-10}") |>
  ggplot(aes(y = norm_tcs, x = tol, col = tol, fill = tol)) + 
  #facet_wrap(.~L2, scales = "free")+
  geom_boxplot(alpha = 0.2, outlier.alpha = 1)+
  xlab("") +
  labs(y = NULL) + 
  scale_color_manual(values = c(scales::hue_pal()(3)))+
  scale_fill_manual(values = c(scales::hue_pal()(3)))+ 
  #scale_y_log10()+
  scale_y_log10(position = "right")+
  scale_x_discrete(label = function(x) parse(text = x))+
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        #axis.text.x = element_text(color = "white"),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(l = 0, r = 5.5, b = 5.5, t = 2.5),
        panel.grid.major.x = element_blank(),
        #panel.border = element_rect(fill = NA, color = "black"),
        panel.border = element_blank(),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_text(color = "black", face = "bold", size = 10),
        text = element_text(size = 10))

dir.create("./img", recursive = TRUE, showWarnings = FALSE)
ggsave("./img/norm_wlsv.pdf", plot = gridExtra::arrangeGrob(grobs = list(p1, p2), widths = c(3,1), ncol=2), height = 4,
       width= 8)
