# =====================================================
# Full project run pipeline
# =====================================================

# Clear the current R environment.
rm(list = ls())

# Make sure the working directory is the project root.
# Uncomment and edit this if needed.
# setwd("path/to/your/project")

# =====================================================
# 1. Load constants
# =====================================================

source("code/00_constants.R", echo = FALSE)

# =====================================================
# 2. Download / refresh raw WDI data
# =====================================================

source("code/01_WDI_data_download.R", echo = FALSE)

# =====================================================
# 3. Build processed panel
# =====================================================

source("code/01_data_download.R", echo = FALSE)

# =====================================================
# 4. Run main GDP SCM analysis
# =====================================================

source("code/02_replication.R", echo = FALSE)

source("code/02_OLS_replication.R", echo = FALSE)

source("code/02_Average_replication.R", echo = FALSE)

# =====================================================
# 5. Run extension trade SCM analysis + placebo tests
# =====================================================

source("code/03_extension_implementation.R", echo = FALSE)

source("code/03_extension_placebo.R", echo = FALSE)




cat("\nFull pipeline complete.\n")
