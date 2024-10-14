# Information about the reconciliation structure
level2 <- c(27, 73, 101, 86, 31) # L2 division of the bts
# Cross-sectional (contemporaneous) matrix, (na x nb)
C <- bind(1, # L1 is the total
          Matrix::bdiag(lapply(level2, function(x) t(rep(1, x)))))
# List with all the cross-sectional information
hts_info <- FoReco::hts_tools(C = C)

m <- 24 # Highest available sampling frequency per seasonal cycle
h <- 2 # Forecast horizon
# List with all the temporal information
thf_info <- FoReco::thf_tools(m = m, h = h)

# Save
save(hts_info, thf_info, h, file = "./raw/info_reco.RData")
