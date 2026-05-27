# =====================================================
# Full project run pipeline
# =====================================================

# Clear the current R environmen
rm(list = ls())

# Make sure the working directory is the project root
# Uncomment and edit this if needed.
# setwd("path/to/your/project")

# =====================================================
# 0. Load constants
# =====================================================

source("./code/00_constants.R", echo = FALSE)

# =====================================================
# 1. Download data (in order)
# =====================================================

source("./code/01_WDI_data_download.R", echo = FALSE)

source("./code/01_data_download.R", echo = FALSE)

# =====================================================
# 2. Run main GDP SCM analysis
# =====================================================

source("./code/02_replication.R", echo = FALSE)

source("./code/02_OLS_replication.R", echo = FALSE)

source("./code/02_Average_replication.R", echo = FALSE)

# =====================================================
# 3. Run extension trade SCM analysis + placebo tests
# =====================================================

source("./code/03_extension_implementation.R", echo = FALSE)

source("./code/03_extension_placebo.R", echo = FALSE)



cat("\nFull pipeline complete.")
