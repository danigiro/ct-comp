# clear ws
rm(list = ls(all = TRUE))

# library
library(tidyverse)
library(data.table)

# source file
source("./R/varname.R")
source("./R/fun.R")

# Load data
load("./raw/info_reco.RData")
load("./results/fore_base_test.RData")
load("./results/fore_tibble_oct.RData")
load("./results/fore_tibble_ka.RData")

## OCT ----
RMSE <- function(actual, predict){ sqrt(mean((actual-predict)^2))/mean(actual)*100 }

kid <- rep(thf_info$kset, h*thf_info$m/thf_info$kset)
opday <- unlist(sapply(thf_info$m/thf_info$kset, function(x) rep(1:h, each =x)))

nRMSEtibble <- tibble(k = as.numeric(),
                      id = as.character(),
                      comb =  as.character(),
                      method =  as.character(),
                      type = as.character(),
                      value = as.numeric(), 
                      h = as.numeric())
for(i in unique(data_oct$name)){
  tmp <- data_oct |>
    filter(name == i) |>
    arrange(id) |>
    pull(data) |> unname()
  # free
  tilde <- simplify2array(tmp)
  
  # sntz
  tilde0_bts <- tilde[rep(thf_info$kset, h*thf_info$m/thf_info$kset) == 1,-c(1:6),]
  tilde0_bts[tilde0_bts<0] <- 0
  tmp <- apply(tilde0_bts, 3, function(x) t(as.matrix(hts_info$S %*% t(x))), simplify = FALSE)
  tmp <- apply(simplify2array(tmp), 3, function(x) as.matrix(thf_info$R %*% x), simplify = FALSE)
  tilde0 <- simplify2array(tmp)
  
  for(k in thf_info$kset){
    for(j in 1:NCOL(tilde)){
      nRMSEtibble <- add_row(nRMSEtibble, 
                             k = k,
                             id = colnames(tilde)[j],
                             comb =  i,
                             method =  "ct",
                             type = c("tilde", "tilde0"),
                             value = c(RMSE(test[kid==k,j,], tilde[kid==k,j,]),
                                       RMSE(test[kid==k,j,], tilde0[kid==k,j,])),
                             h = 0
      )
      
      nRMSEtibble <- add_row(nRMSEtibble, 
                             k = k,
                             id = colnames(tilde)[j],
                             comb =  i,
                             method =  "ct",
                             type = c("tilde", "tilde0"),
                             value = c(RMSE(test[kid==k & opday == 1,j,], 
                                            tilde[kid==k & opday == 1,j,]),
                                       RMSE(test[kid==k & opday == 1,j,], 
                                            tilde0[kid==k & opday == 1,j,])),
                             h = 1
      )
      
      nRMSEtibble <- add_row(nRMSEtibble, 
                             k = k,
                             id = colnames(tilde)[j],
                             comb =  i,
                             method =  "ct",
                             type = c("tilde", "tilde0"),
                             value = c(RMSE(test[kid==k & opday == 2,j,], 
                                            tilde[kid==k & opday == 2,j,]),
                                       RMSE(test[kid==k & opday == 2,j,], 
                                            tilde0[kid==k & opday == 2,j,])),
                             h = 2
      )
    }
    cat(k, " ")
  }
  cat(i, "\n")
}

## base ----
for(k in thf_info$kset){
  for(j in 1:NCOL(base)){
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(base)[j],
                           comb =  "base",
                           method =  "none",
                           type = "none",
                           value = c(RMSE(test[kid==k,j,], base[kid==k,j,])),
                           h = 0
    )
    
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(base)[j],
                           comb =  "base",
                           method =  "none",
                           type = "none",
                           value = c(RMSE(test[kid==k & opday == 1,j,], 
                                          base[kid==k & opday == 1,j,])),
                           h = 1
    )
    
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(base)[j],
                           comb =  "base",
                           method =  "none",
                           type = "none",
                           value = c(RMSE(test[kid==k & opday == 2,j,], 
                                          base[kid==k & opday == 2,j,])),
                           h = 2
    )
  }
  cat(k, " ")
}


## 3TIER ----
TIER_1 <- base[rep(thf_info$kset, h*thf_info$m/thf_info$kset) == 1,-c(1:6),]
tmp <- apply(TIER_1, 3, function(x) t(as.matrix(hts_info$S %*% t(x))), simplify = FALSE)
tmp <- apply(simplify2array(tmp), 3, function(x) as.matrix(thf_info$R %*% x), simplify = FALSE)
TIER <- simplify2array(tmp)
varname <- colnames(base)
colnames(TIER) <- varname

for(k in thf_info$kset){
  for(j in 1:NCOL(TIER)){
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(TIER)[j],
                           comb =  "3TIER",
                           method =  "ctbu",
                           type = "none",
                           value = c(RMSE(test[kid==k,j,], TIER[kid==k,j,])),
                           h = 0
    )
    
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(TIER)[j],
                           comb =  "3TIER",
                           method =  "ctbu",
                           type = "none",
                           value = c(RMSE(test[kid==k & opday == 1,j,], 
                                          TIER[kid==k & opday == 1,j,])),
                           h = 1
    )
    
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(TIER)[j],
                           comb =  "3TIER",
                           method =  "ctbu",
                           type = "none",
                           value = c(RMSE(test[kid==k & opday == 2,j,], 
                                          TIER[kid==k & opday == 2,j,])),
                           h = 2
    )
  }
  cat(k, " ")
}

## pers ----
meas <- as.data.frame(fread("./raw/meas.csv", header = TRUE, sep = ","))
split_row <- rep(1:365, each = 24)

fmeas <- lapply(1:365, function(x) as.matrix(meas[split_row%in%c(x, x+1),]))
fmeas <- fmeas[c(13:362)]
fmeas <- simplify2array(fmeas)

tmp <- apply(fmeas, 3, function(x){
  out <- t(as.matrix(hts_info$S %*% t(x)))
  colnames(out) <- varname
  rownames(out) <- NULL
  out
}, simplify = FALSE)
tmp <- apply(simplify2array(tmp), 3, function(x) as.matrix(thf_info$R %*% x), simplify = FALSE)
pers <- simplify2array(tmp)
for(k in thf_info$kset){
  for(j in 1:NCOL(pers)){
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(pers)[j],
                           comb =  "pers",
                           method =  "bf",
                           type = "none",
                           value = c(RMSE(test[kid==k,j,], pers[kid==k,j,])),
                           h = 0
    )
    
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(pers)[j],
                           comb =  "pers",
                           method =  "bf",
                           type = "none",
                           value = c(RMSE(test[kid==k & opday == 1,j,], 
                                          pers[kid==k & opday == 1,j,])),
                           h = 1
    )
    
    nRMSEtibble <- add_row(nRMSEtibble, 
                           k = k,
                           id = colnames(pers)[j],
                           comb =  "pers",
                           method =  "bf",
                           type = "none",
                           value = c(RMSE(test[kid==k & opday == 2,j,], 
                                          pers[kid==k & opday == 2,j,])),
                           h = 2
    )
  }
  cat(k, " ")
}

# KA
data_ka <- data_ka |>
  filter(name == "wlsv-wls") |>
  mutate(type = paste0("ka[", type, "]"))

for(i in unique(data_ka$type)){
  tmp <- data_ka |>
    filter(type == i) |>
    arrange(id) |>
    pull(data) |> unname()
  # free
  tilde <- simplify2array(tmp)
  
  # sntz
  tilde0_bts <- tilde[rep(thf_info$kset, h*thf_info$m/thf_info$kset) == 1,-c(1:6),]
  tilde0_bts[tilde0_bts<0] <- 0
  tmp <- apply(tilde0_bts, 3, function(x) t(as.matrix(hts_info$S %*% t(x))), simplify = FALSE)
  tmp <- apply(simplify2array(tmp), 3, function(x) as.matrix(thf_info$R %*% x), simplify = FALSE)
  tilde0 <- simplify2array(tmp)
  
  for(k in thf_info$kset){
    for(j in 1:NCOL(tilde)){
      nRMSEtibble <- add_row(nRMSEtibble, 
                             k = k,
                             id = colnames(tilde)[j],
                             comb =  i,
                             method =  "ka",
                             type = c("tilde", "tilde0"),
                             value = c(RMSE(test[kid==k,j,], tilde[kid==k,j,]),
                                       RMSE(test[kid==k,j,], tilde0[kid==k,j,])),
                             h = 0
      )
      
      nRMSEtibble <- add_row(nRMSEtibble, 
                             k = k,
                             id = colnames(tilde)[j],
                             comb =  i,
                             method =  "ka",
                             type = c("tilde", "tilde0"),
                             value = c(RMSE(test[kid==k & opday == 1,j,], 
                                            tilde[kid==k & opday == 1,j,]),
                                       RMSE(test[kid==k & opday == 1,j,], 
                                            tilde0[kid==k & opday == 1,j,])),
                             h = 1
      )
      
      nRMSEtibble <- add_row(nRMSEtibble, 
                             k = k,
                             id = colnames(tilde)[j],
                             comb =  i,
                             method =  "ka",
                             type = c("tilde", "tilde0"),
                             value = c(RMSE(test[kid==k & opday == 2,j,], 
                                            tilde[kid==k & opday == 2,j,]),
                                       RMSE(test[kid==k & opday == 2,j,], 
                                            tilde0[kid==k & opday == 2,j,])),
                             h = 2
      )
    }
    cat(k, " ")
  }
  cat(i, "\n")
}

nRMSEdata <- nRMSEtibble %>%
  mutate(group = ifelse(id %in% varname[c(1)], "L1",
                        ifelse(id %in% varname[c(2:6)], "L2", "L3")))
save(nRMSEdata, file = "./results/nRMSEdata.RData")















