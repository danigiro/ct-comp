#Clear all workspace
rm(list = ls(all = TRUE))
libs <- c("FoReco", "progress")
invisible(lapply(libs, library, character.only = TRUE))

source("./R/fun.R")
load("./raw/info_reco.RData")

rep = 1:350
#rep = 1:2
tol = c(1e-5, 1e-6, 1e-8, 1e-10)
pb <- progress_bar$new(format = paste0(" [:bar] :percent eta: :eta tot: :elapsed"),
                       total = max(rep), clear = FALSE, width= 80)
dir.create("./results/results_ite_tol", recursive = TRUE, showWarnings = FALSE)
for(rp in rep){
  all <- load_replication(i = rp)
  tcs <- list()
  tcs[as.character(tol)] <- rep(list(matrix(NA, ncol = NCOL(all$Yhat), nrow = NROW(all$Yhat), 
                               dimnames = dimnames(all$Yhat))), length(tol))
  cst <- tcs
  
  time <- list()
  time[c("cst", "tcs")] <- NULL
  time[["tcs"]][as.character(tol)] <- rep(list(NA), length(tol))
  time[["cst"]][as.character(tol)] <- rep(list(NA), length(tol))
  
  mem <- list()
  mem[c("cts", "tcs")] <- NULL
  mem[["tcs"]][as.character(tol)] <- rep(list(NA), length(tol))
  mem[["cst"]][as.character(tol)] <- rep(list(NA), length(tol))
  
  ite <- list()
  ite[c("cst", "tcs")] <- NULL
  ite[["tcs"]][as.character(tol)] <- rep(list(NA), length(tol))
  ite[["cst"]][as.character(tol)] <- rep(list(NA), length(tol))
  
  diff <- list()
  for(j in tol){
    splitcomb <- c("wlsv", "wls")
    tmp <- bench::mark(tcs = FoReco::iterec(base = t(all$Yhat), type = "tcs", verbose = FALSE, tol = j,
                                            cslist = list(agg_mat = hts_info$C, comb = splitcomb[2]),
                                            telist = list(agg_order = thf_info$m, comb = splitcomb[1]), 
                                            res = t(all$E)), 
                       cst = FoReco::iterec(base = t(all$Yhat), type = "cst", verbose = FALSE, tol = j,
                                            cslist = list(agg_mat = hts_info$C, comb = splitcomb[2]),
                                            telist = list(agg_order = thf_info$m, comb = splitcomb[1]), 
                                            res = t(all$E)),
                       iterations = 1, min_time = 0.0001, time_unit = "s", filter_gc = FALSE,
                       check = function(...) TRUE)
    
    tcs[[as.character(j)]] <- drop_zeros(t(tmp$result[[1]]))
    cst[[as.character(j)]] <- drop_zeros(t(tmp$result[[2]]))
    
    time[["tcs"]][[as.character(j)]] <- tmp$min[1]
    time[["cst"]][[as.character(j)]] <- tmp$min[2]
    
    mem[["tcs"]][[as.character(j)]] <- tmp$mem_alloc[1]
    mem[["cst"]][[as.character(j)]] <- tmp$mem_alloc[2]
    
    ite[["tcs"]][[as.character(j)]] <- FoReco::recoinfo(tmp$result[[1]], verbose = F)$ite
    ite[["cst"]][[as.character(j)]] <- FoReco::recoinfo(tmp$result[[2]], verbose = F)$ite
    
    diff[[as.character(j)]] <- tcs[[as.character(j)]]-cst[[as.character(j)]]
  }
  
  file.name <- paste("./results_ite_tol/rep--", rp, "--ite--reco.RData", sep = "")
  save(cst, tcs, time, diff, mem, ite, file = file.name)
  pb$tick()
}
