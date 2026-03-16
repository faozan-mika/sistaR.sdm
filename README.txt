# sistaR.sdm: Species Distribution Modeling

## Installation

```r
# Step 1: Install (quick, <1MB)
# Then install
if (!require("devtools")) install.packages("devtools")
library(devtools)
install_github("faozan-mika/sistaR.sdm")

# Step 2: Download data (one-time, 250MB)
library(sistaR.sdm)

# OPTION 1: Download everything at once, but often failed especially soil data
download_sdm_data

# OPTION 2 : Download specific components
download_sdm_climate()
download_sdm_elevation()
download_sdm_soil()
download_sdm_srad()
download_sdm_wind()
download_sdm_vapr()
download_sdm_future()

# Step 3: Run app (works offline after download)
sistaR.sdm()

# step 4: check downloaded data
check_sdm_data()
