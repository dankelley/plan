requireNamespace(c("devtools", "urlchecker", "rhub", "revdepcheck"))
t <- devtools::spell_check()
stopifnot(t == "No spelling errors found.")
urlchecker::url_check()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_win_oldrelease()
rhub::check_for_cran()
# I cannot do revdepcheck() with a pre-release R, but
# I know there are no reverse dependencies, anyway.
#devtools::install_github("r-lib/revdepcheck")
#revdepcheck::revdep_reset()
#revdepcheck::revdep_check(num_workers=4)

