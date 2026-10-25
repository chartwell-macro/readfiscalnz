## Summary

<!-- What user problem does this change address? Keep the scope focused. -->

## Compatibility and data contract

- [ ] Public function names, signatures, defaults, and exports are preserved,
      or any intentional change is explained here:
- [ ] Output column names/order, classes, attributes, date semantics, errors,
      warnings, network behaviour, and cache behaviour are preserved, or any
      intentional change is explained here:
- [ ] Series identities, identity payloads, and identity hashes remain stable,
      or any intentional change is explained here:
- [ ] The six supported release IDs and their source semantics remain covered,
      or any catalogue change is explained here:

## Documentation and generated files

- [ ] Roxygen comments are complete for changed public objects.
- [ ] `devtools::document()` was run and generated `NAMESPACE`/`man/*.Rd` were
      inspected.
- [ ] If `README.Rmd` changed, `README.md` was regenerated and inspected.
- [ ] Changed vignettes were built through `R CMD build .` and inspected.
- [ ] Generated evidence under `inst/stage3_review/` was not hand-edited; any
      required evidence was regenerated and its focused contract tests pass.
- [ ] No `.Rcheck` directory, tarball, cache, downloaded workbook, private
      benchmark output, or generated site was added unintentionally.

## Validation

```text
R version / OS:
Offline test command and result:
R CMD build result:
R CMD check --as-cran result:
Documentation/pkgdown result (if relevant):
Live validation: not run / passed / unavailable (explain):
```

- [ ] The offline fixture-based test suite passes.
- [ ] Examples and vignettes do not require live network access for the normal
      check path.
- [ ] Any live validation was explicitly opted into, and no credentials or
      downloaded artifacts are included in this pull request.
- [ ] Runtime or parser changes include regression coverage; performance
      impact was measured when transport/parsing behaviour could be affected.

## Notes for reviewers

<!-- Mention limitations, migration notes, changed release metadata, or
follow-up work. -->
