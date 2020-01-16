## Test environments
* local OS X install, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

## Resubmission 

This is a resubmission. In this version I have:

* added on.exit() to reset user's par options in gp_diagnostics.R

* create_gadget_infile.R no longer saves Rdata file to user's working directory by default. User now has to specify save location. Example saves to tempdir() directory instead of working directory.

* Removed \dontrun{} from create_gadget_infile.R, replaced \dontrun{} with \donttest{} in design_experiment.R and sequential_experiment examples

* Updated the License and Copyright year to 2020

* LICENSE file was corrected to conform with CRAN BSD-3 template

* moved additional copyright notice from LICENSE file to COPYRIGHTS file  

* removed middle initial from maintainer's name to conform with author's name

---

