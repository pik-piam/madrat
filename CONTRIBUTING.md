# How to Contribute to a mr* Package

Thanks for considering contributing to a mr* package!

For our coding style, see the CODE_STYLE.md file.

## Environment Setup
- By installing the full `lucode2` package, you get all needed tools:
```R
  pak::pak("lucode2", repos = c('https://pik-piam.r-universe.dev', 'https://cloud.r-project.org'))
  # OR
  install.packages("lucode2", repos = c('https://pik-piam.r-universe.dev', 'https://cloud.r-project.org'), dependencies = TRUE)
```

## Interactive Evaluation of Functions
To evaluate your changes interactively in an R session:
- Use `devtools::load_all()` from within the package folder to load the current version
- Use `madrat::setConfig(ignorecache = c("calcMyFunction", "readMyData"))` to force the re-execution of your function
- Never call read/correct/convert/calc/full functions directly, but via `readSource`/`calcOutput`/`retrieveData` (bad: `readLUH3()`, good: `readSource("LUH3")`)

## Note on Caching in madrat
- madrat computes cache keys from the source of a function and its dependencies (including `tool*` functions); auxiliaries defined outside a `calc*`/`read*`/`retrieveData` body are not in the cache key, so changing them won't invalidate cached outputs
- Therefore define auxiliary functions inside your `calc`/`read`/`retrieveData` function, or, if shared between functions, as a `tool*` function

## Testing
- Tests are in tests/testthat
- Fixtures should go into tests/testthat/fixtures
- Running tests
  - All tests: `devtools::test()`
  - Single test file: `devtools::test_file("tests/testthat/test_myFunction.R")`

## Overall Development Process
- Before pushing, run `lucode2::buildLibrary()` to check your code and update metadata
- Every published change needs a new version (we follow semantic versioning)
- For publishing your change, in general: open a PR, then merge it
- If your change is very small and the repository allows it: push directly

## Attribution
- When you have contributed to this package, feel free to add yourself to the DESCRIPTION file
