#Clear all workspace
rm(list = ls(all = TRUE))
libs <- c("FoReco", "progress")
invisible(lapply(libs, library, character.only = TRUE))

source("./R/fun.R")
load("./raw/info_reco.RData")

rep = 1:350
#rep = 1:2
comb <- c("ols", "str", "wlsv", "csstr", "testr")
pb <- progress_bar$new(format = paste0(" [:bar] :percent eta: :eta tot: :elapsed"),
                       total = max(rep), clear = FALSE, width= 80)
dir.create("./results/results_oct", recursive = TRUE, showWarnings = FALSE)
for(rp in rep){
  all <- load_replication(i = rp)
  free <- list()
  free[comb] <- rep(list(matrix(NA, ncol = NCOL(all$Yhat), nrow = NROW(all$Yhat), 
                                dimnames = dimnames(all$Yhat))), length(comb))
  
  time <- list()
  time[c("free")] <- NULL
  time[["free"]][comb] <- rep(list(NA), length(comb))
  
  mem <- list()
  mem[c("free")] <- NULL
  mem[["free"]][comb] <- rep(list(NA), length(comb))
  
  for(j in comb){
    tmp <- bench::mark(oct = FoReco::ctrec(base = t(all$Yhat), comb = j, agg_order = thf_info$m,
                                           agg_mat = hts_info$C, res = t(all$E)), 
                       iterations = 2, min_time = 0.0001, time_unit = "s", filter_gc = FALSE,
                       check = function(...) TRUE)
    
    free[[j]] <- drop_zeros(t(tmp$result[[1]]))
    time[["free"]][[j]] <- tmp$min[1]
    mem[["free"]][[j]] <- tmp$mem_alloc[1]
  }
  file.name <- paste("./results_oct/rep--", rp, "--oct--reco.RData", sep = "")
  save(free, mem, time, file = file.name)
  pb$tick()
}
