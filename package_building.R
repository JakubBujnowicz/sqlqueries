# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# Libraries --------------------------------------------------------------------
# 1: Documenting
devtools::document()
# roxygen2::roxygenise(clean = TRUE)

# 2: Checking
devtools::check(document = FALSE, cran = TRUE)

# 3: Testing
devtools::test()

# 4: Building
devtools::build(path = "tars")
# devtools::build_manual(path = "manuals")

# 5: Installing
devtools::install()

# Updating data
source("data-raw/data_updating.R")


# Release ######################################################################
spell_check()
check_rhub()
check_win_devel()

# 5: Release
release()

