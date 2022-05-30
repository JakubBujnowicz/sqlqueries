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
tic("#1: Documenting")
document()
# roxygenise(clean = TRUE)
toc()

tic("#2: Checking")
check(document = FALSE, cran = TRUE)
toc()

tic("#3: Building")
build(path = "tars")
# build_manual(path = "manuals")
toc()

tic("#4: Installing")
install()
toc()

tic("Updating data")
source("data-raw/data_updating.R")
toc()


# Release ######################################################################
spell_check()
check_rhub()
check_win_devel()

{
    tic("#5: Release")
    release()
    toc()
}
