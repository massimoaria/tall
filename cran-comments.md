## R CMD check results

0 errors | 0 warnings | 0 notes

Checked with `R CMD check --as-cran` (CRAN incoming feasibility included) on
R 4.6.1 (2026-06-24), aarch64-apple-darwin23, macOS 27.2.

## Submission comments

* This is a patch release.

* It fixes a defect that made the Google Gemini integration unusable for any
  user with a recently created API key: key validation probed a hard-coded
  model that Google has withdrawn, so a valid key was reported as invalid.
  Validation now queries the model catalogue instead of invoking a model.
  Reported by a user.

* The remaining entries in NEWS.md are bug fixes only; there are no new
  features and no user-visible API changes.
