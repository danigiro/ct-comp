#' extract_agg
#' 
#' Funzione per ricomporre la matrice cross-temporale Yhat [h(m+k*) x n], 
#' delle osservazioni al livello più disaggregato B1 [hm x n] e dei 
#' residui E [N(m+k*) x n].
#' 
#' @param i replicatione dell'esperimento
#' 
#' @return Una lista con le tre matrici Yhat, B1, E
load_replication <- function(i = 1){
  if(i<1 | i > 350){
    stop("Number of replications exceed (min = 1, max = 350)", call. = FALSE)
  }
  filesn <- list.files("./results/Results_Time", full.names = TRUE)
  Yhat <- matrix(NA, 120, length(filesn)-6)
  Yobs <- matrix(NA, 48, length(filesn)-6)
  Res <- matrix(NA, 840, length(filesn)-6)
  Yhat_uts <-  matrix(NA, 120, 6)
  Yobs_uts <- matrix(NA, 48, 6)
  Res_uts <- matrix(NA, 840, 6)
  k <- c(24, 12, 8, 6, 4, 3, 2, 1)
  colnames(Yhat) <- colnames(Yobs) <- colnames(Res) <- rep(NA, length(filesn)-6)
  colnames(Yhat_uts) <- colnames(Yobs_uts) <- colnames(Res_uts) <- c('Total', '01', '02', '03', '04', '05')
  for(n in 1:length(filesn)){
    all <- mget(load(filesn[n], envir=(NE. <- new.env())), envir=NE.)
    rm(NE.)
    id <- as.numeric(strsplit(filesn[n], "--")[[1]][2])
    if(id != 0){
      colnames(Yhat)[id] <- colnames(Yobs)[id] <- colnames(Res)[id] <- strsplit(basename(filesn[n]), "--")[[1]][1]
      Yhat[,id] <- unname(unlist(rev(split(matrix(all$Y.hat[,i], ncol = 2), rep(k, 24/k)))))
      Yobs[,id] <- all$Y.obs[,i]
      Res[,id] <- unlist(sapply(rev(all$Res.insamp[[i]]), as.vector))
    }else{
      Yhat_uts[, strsplit(basename(filesn[n]), "--")[[1]][1]] <- unname(unlist(rev(split(matrix(all$Y.hat[,i], ncol = 2), rep(k, 24/k)))))
      Yobs_uts[, strsplit(basename(filesn[n]), "--")[[1]][1]] <- all$Y.obs[,i]
      Res_uts[, strsplit(basename(filesn[n]), "--")[[1]][1]] <- unlist(sapply(rev(all$Res.insamp[[i]]), as.vector))
    }
  }
  return(list(Yhat = cbind(Yhat_uts, Yhat), 
              B1 = cbind(Yobs_uts, Yobs), 
              E = cbind(Res_uts, Res)))
}

drop_zeros <- function(x, tol = sqrt(.Machine$double.eps)){
  x1 <- as.matrix(Matrix::drop0(x, tol = tol))
  if(is.vector(x)){
    setNames(as.vector(x1), names(x))
  }else{
    x1
  }
}

#' extract_agg
#' 
#' Funzione per estrarre l'aggregazione temporale da una matrice 
#' (output res o bf di load_replication)
#' 
#' @param data matrice [h(m+k*) x n] (o per i residui [N(m+k*) x n])
#' @param aggregation ordine di aggreazione (m/k): 24, 12, 8, 6, 4, 3, 2, 1
#' 
#' @return matrix [h(m/k) x n] (o per i residui [N(m/k) x n])
extract_agg <- function(data, aggregation = 1){
  agg <- c(24, 12, 8, 6, 4, 3, 2, 1)
  h <- NROW(data)/sum(agg)
  return(data[rep(agg, h*max(agg)/agg) == aggregation, ,drop ])
}