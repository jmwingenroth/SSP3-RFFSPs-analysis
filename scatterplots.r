library(tidyverse)

# Load data

bn_rff_scc <- read_csv("../paper-biodiversity/output/BN-2020-RFF/scc.csv")
bn_ssp3_scc <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/scc.csv")
nunez_rff_scc <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/scc.csv")
nunez_ssp3_scc <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/scc.csv")
sectoral_rff_scc <- read_csv("./scghg-RFF-sectoral-2020-CO2-n10000/sc-CO2-n10000.csv")
sectoral_ssp3_scc <- read_csv("./scghg-SSP3-sectoral-2020-CO2-n10000/sc-CO2-n10000.csv")

