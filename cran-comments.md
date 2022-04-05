# Release 0.4-4

This is an attempt to the solve if-length-1 and class-test problems pointed out
in a helpful email from B. Ripley dated 05 Apr 2022 09:49:37.0039 (UTC). Hints
in that email showed how I could reproduce the bugs locally using 4.2.0-alpha
(see below). Having done that, I fixed the bugs and tested, to get some
assurance that things were okay.  I then used the package for a while to see if
I had broken anything (a lot relates to plotting, which is hard to check in
formal tests).

Based on all of this, I think the new version is suitable for CRAN.

Other changes have been more minor, e.g. changing to markdown for
roxygen-generated manpages, adding a WORDLIST file, and fixing some URLs.

## Test environments

The following all worked on build and check, without reporting problems.

* local macOS install, R version 4.2.0 alpha (2022-04-04 r82084)
* rhub::check_for_cran()
* devtools::check_win_devel()

## R CMD check results

0 errors | 0 warnings | 1 note

## Reverse dependencies

None.

