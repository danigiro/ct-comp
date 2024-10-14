# clear ws
rm(list = ls(all = TRUE))
library(progress)
library(tidyverse)

# Load data
load("./raw/info_reco.RData")

# source file
source("./R/varname.R")
source("./R/fun.R")

# base and test ----
fls <- list.files("./results/Results_Time", full.names = TRUE)
base <- array(NA, dim = c(thf_info$kt*h, length(varname), 350), 
              dimnames = list(NULL, varname, NULL))
test <- array(NA, dim = c(thf_info$kt*h, length(varname), 350), 
              dimnames = list(NULL, varname, NULL))
pb <- progress_bar$new(
  format = "[:bar] :percent :eta in :elapsed", 
  total = length(fls), 
  width = 60,
  clear = FALSE
)
for(j in fls){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  base[,strsplit(basename(j), "--")[[1]][1],] <- apply(all$Y.hat, 2, 
                                                       function(x) unname(unlist(rev(split(matrix(x, ncol = 2), 
                                                                                           rep(thf_info$kset, 24/thf_info$kset))))))
  test[,strsplit(basename(j), "--")[[1]][1],] <- as.matrix(thf_info$R %*% all$Y.obs)
  pb$tick()
}
save(base, test, file = "./results/fore_base_test.RData")

# cross-temporal ----
fls <- list.files('./results/results_oct', full.names = TRUE)
data_oct <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  tibble(data = all$free) |>
    mutate(name = names(data),
           id = as.numeric(strsplit(basename(j), "--")[[1]][2]))
})
data_oct <- bind_rows(data_oct)
save(data_oct,
     file = "./results/fore_tibble_oct.RData")


# iterative ----
fls <- list.files('./results/results_ite', full.names = TRUE)
data_ite <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  bind_rows(tibble(data = all$tcs) |>
    mutate(name = names(data),
           type = "tcs",
           id = as.numeric(strsplit(basename(j), "--")[[1]][2])),
    tibble(data = all$cst) |>
      mutate(name = names(data),
             type = "cst",
             id = as.numeric(strsplit(basename(j), "--")[[1]][2])),)
})
data_ite <- bind_rows(data_ite)
save(data_ite,
     file = "./results/fore_tibble_ite.RData")

# ka ----
fls <- list.files('./results/results_ka', full.names = TRUE)
data_ka <- lapply(fls, function(j){
  all <- mget(load(j, envir=(NE. <- new.env())), envir=NE.)
  rm(NE.)
  bind_rows(tibble(data = all$tcs) |>
              mutate(name = names(data),
                     type = "tcs",
                     id = as.numeric(strsplit(basename(j), "--")[[1]][2])),
            tibble(data = all$cst) |>
              mutate(name = names(data),
                     type = "cst",
                     id = as.numeric(strsplit(basename(j), "--")[[1]][2])),)
})
data_ka <- bind_rows(data_ka)
save(data_ka,
     file = "./results/fore_tibble_ka.RData")







