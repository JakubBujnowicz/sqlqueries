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
tictoc::tic("#1: Documenting")
document()
# roxygenise(clean = TRUE)
tictoc::toc()

tictoc::tic("#2: Checking")
check(document = FALSE, cran = TRUE)
tictoc::toc()

tictoc::tic("#3: Building")
build(path = "tars")
# build_manual(path = "manuals")
tictoc::toc()

tictoc::tic("#4: Installing")
install()
tictoc::toc()

tictoc::tic("Updating data")
source("data-raw/data_updating.R")
tictoc::toc()


# Release ######################################################################
spell_check()
check_rhub()
check_win_devel()

{
    tictoc::tic("#5: Release")
    release()
    tictoc::toc()
}
