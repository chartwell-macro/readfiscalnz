## Test environments

- Local: macOS Sequoia 15.6 (arm64), R 4.5.1, `R CMD check --as-cran`
  on the built source tarball.
- The GitHub Actions R-devel, release, and oldrel-1 Ubuntu jobs and R-release
  macOS and Windows jobs passed on an earlier release-preparation commit, as
  did the separate test-coverage and pkgdown build jobs. On the current
  documentation-amendment commit, GitHub rejected every job before allocating
  a runner because of an account payment or spending-limit condition. No
  current-commit remote check result is claimed; the workflows must be rerun
  after that external account condition is resolved.
- Win-builder, R-hub, and other external submission services have not been run.

## R CMD check results

The local built-tarball check completed with:

- 0 ERRORs
- 0 WARNINGs
- 2 NOTEs

The first NOTE identifies this as a new submission and reports unavailable
URLs. At check time the GitHub repository and issue tracker were restricted,
the GitHub Pages site had not been deployed, and the Treasury copyright page
returned HTTP 403 to the automated checker. These are external publication
state issues, not waived package-code findings. The repository, issue tracker,
and documentation site must be publicly reachable and rechecked before a CRAN
submission.

The second NOTE says that the check environment could not verify the current
time. It is specific to that isolated local check environment; no future-dated
package file was reported.

All examples, tests, installed files, vignettes, vignette rebuilding, the PDF
manual, and the HTML manual passed in the local `--as-cran` check. The package
tests and documentation workflows are offline by default.

## Maintainer metadata

The package author, maintainer, and copyright-holder display is Chartwell Macro. The
CRAN-required maintainer address in `DESCRIPTION` is a non-personal repository
noreply address; optional citation and public prose omit email. Package code
and documentation use the MIT licence, copyright Chartwell Macro, as recorded in
`LICENSE`. Treasury source data, official publications, and third-party
material retain their own rights and attribution and are not assigned to Chartwell
F.

## Downstream and reverse dependencies

This is a new package and has no published CRAN reverse dependencies, so a CRAN
reverse-dependency check was not applicable. The known read-only Chartwell
provider and release-artifact contract tests passed against an isolated install
of this source tree. Chartwell remains pinned to its reviewed package commit;
no downstream repository was changed.

## Submission status

This file records release-preparation results only. No CRAN submission has been
made. It must be updated with the final public URL checks and remote CI/check
results from submission services before submission.
