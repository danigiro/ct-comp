# Insights into regression-based cross-temporal forecast reconciliation

## Overview

This README provides step-by-step instructions for reproducing the forecasting experiments 
detailed in the paper. The experiments utilize the PV324 dataset (available in the `raw` folder) 
and involve the implementation of various cross-temporal forecast reconciliation techniques.

- **Manuscript title**: *Insights into regression-based cross-temporal forecast reconciliation*
- **Authors**: Daniele Girolimetto, and Tommaso Di Fonzo

## Instructions for reproducing the experiment

To replicate the analysis, please execute the following scripts in the specified order.

 1. `./R/00_base_forecasts.R`: Calculation of base ETS forecasts for the 6 upper time series \
    `./R/00_info_reco.R`: Information on cross-temporal reconciliation

 2. `./R/01_ctrec.R`: cross-temporal optimal forecast reconciliation \
    `./R/01_karec.R`: cross-temporal Kourentzes and Athanasopoulos (2019) reconciliation \
    `./R/01_iterec.R`: cross-temporal iterative forecast reconciliation \
    `./R/01_iterec_tol.R`: cross-temporal iterative forecast reconciliation (convergence `ite(wlsv[te], wls[cs])`)

 3. `./R/02_extract_forecasts.R`: reorganizes forecasts to calculate indices
 4. `./R/03_nRMSEdata.R`: calculation of nRMSE
 5. `./R/04_plot_mem.R`: replicate computational memory plots \
    `./R/04_plot_time.R`: replicate computational time plots \
    `./R/04_tables_mcb.R`: replicate tables and plots
