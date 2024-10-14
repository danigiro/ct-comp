#Clear all workspace
rm(list = ls(all = TRUE))
libs <- c("FoReco", "progress")
invisible(lapply(libs, library, character.only = TRUE))

source("./R/fun.R")
load("./raw/info_reco.RData")

rep = 1:350
#rep = 1:2
comb <- c("ols-ols", "str-str", "ols-str", "str-ols", "wlsv-wls")
pb <- progress_bar$new(format = paste0(" [:bar] :percent eta: :eta tot: :elapsed"),
                       total = max(rep), clear = FALSE, width= 80)
dir.create("./results/results_ite", recursive = TRUE, showWarnings = FALSE)
for(rp in rep){
  all <- load_replication(i = rp)
  tcs <- list()
  tcs[comb] <- rep(list(matrix(NA, ncol = NCOL(all$Yhat), nrow = NROW(all$Yhat), 
                               dimnames = dimnames(all$Yhat))), length(comb))
  cst <- tcs
  
  time <- list()
  time[c("cst", "tcs")] <- NULL
  time[["tcs"]][comb] <- rep(list(NA), length(comb))
  time[["cst"]][comb] <- rep(list(NA), length(comb))
  
  mem <- list()
  mem[c("cts", "tcs")] <- NULL
  mem[["tcs"]][comb] <- rep(list(NA), length(comb))
  mem[["cst"]][comb] <- rep(list(NA), length(comb))
  
  ite <- list()
  ite[c("cst", "tcs")] <- NULL
  ite[["tcs"]][comb] <- rep(list(NA), length(comb))
  ite[["cst"]][comb] <- rep(list(NA), length(comb))
  
  diff <- list()
  for(j in comb){
    splitcomb <- strsplit(j, "-")[[1]]
    tmp <- bench::mark(tcs = FoReco::iterec(base = t(all$Yhat), type = "tcs", verbose = FALSE,
                                            cslist = list(agg_mat = hts_info$C, comb = splitcomb[2]),
                                            telist = list(agg_order = thf_info$m, comb = splitcomb[1]), 
                                            res = t(all$E)), 
                       cst = FoReco::iterec(base = t(all$Yhat), type = "cst", verbose = FALSE,
                                            cslist = list(agg_mat = hts_info$C, comb = splitcomb[2]),
                                            telist = list(agg_order = thf_info$m, comb = splitcomb[1]), 
                                            res = t(all$E)),
                       iterations = 2, min_time = 0.0001, time_unit = "s", filter_gc = FALSE,
                       check = function(...) TRUE)
    
    tcs[[j]] <- drop_zeros(t(tmp$result[[1]]))
    cst[[j]] <- drop_zeros(t(tmp$result[[2]]))
    
    time[["tcs"]][[j]] <- tmp$min[1]
    time[["cst"]][[j]] <- tmp$min[2]
    
    mem[["tcs"]][[j]] <- tmp$mem_alloc[1]
    mem[["cst"]][[j]] <- tmp$mem_alloc[2]
    
    ite[["tcs"]][[j]] <- FoReco::recoinfo(tmp$result[[1]], verbose = F)$ite
    ite[["cst"]][[j]] <- FoReco::recoinfo(tmp$result[[2]], verbose = F)$ite
    
    diff[[j]] <- tcs[[j]]-cst[[j]]
  }
  
  file.name <- paste("./results_ite/rep--", rp, "--ite--reco.RData", sep = "")
  save(cst, tcs, time, diff, mem, ite, file = file.name)
  pb$tick()
}
