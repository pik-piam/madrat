## Coding Conventions
- General coding style is documented through linter rules (lucode2::lintrRules). Basic rules:
  - Camel case
  - Line length: 120 characters

- No non-standard evaluation
- Prefer `pkg::fun()` over `@importFrom pkg fun`
- If `pkg` is in `Depends:`, drop the `::` (it is already attached)
- Mapping files need to be put into inst/extdata in packages
- `magpiecell` cellular resolution is deprecated and should not be added anymore
- Use `terra` instead of `raster`
- Use `withr` calls instead of base tempdir and deferred function executions
- use `paste0` over `sprintf`
- avoid single line `if` (e.g. `if (condition) a <- x`), except for the form `if (cond) x else y`

- Aim to stay with magclass objects, only switch to data.frames or alike when external packages need it or operations can be expressed much more concisely
- comments should be concise
- no redundant comments (code or function docs already clearly communicate what the comment says)
- Packages follow the basic structure from [R Packages](https://r-pkgs.org/structure.html)
