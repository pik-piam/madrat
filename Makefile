.PHONY: help build check test lint lint-all format format-all install docs
.DEFAULT_GOAL = help

# extracts the help text and formats it nicely
HELP_PARSING = 'm <- readLines("Makefile");\
				m <- grep("\#\#", m, value=TRUE);\
				command <- sub("^([^ ]*) *\#\#(.*)", "\\1", m);\
				help <- sub("^([^ ]*) *\#\#(.*)", "\\2", m);\
				cat(sprintf("%-8s%s", command, help), sep="\n")'

help:           ## Show this help.
	@Rscript -e $(HELP_PARSING)

build:          ## Build the package using lucode2::buildLibrary(). You can pass the
                ## updateType with 'make build u=3'
	Rscript -e 'lucode2::buildLibrary(updateType = "$(u)")'

check:          ## Build documentation and vignettes, run testthat tests,
                ## and check if code etiquette is followed using lucode2::check().
	Rscript -e 'lucode2::check()'

test:           ## Run testthat tests
	Rscript -e 'devtools::test(show_report = TRUE)'

lint:           ## Check if code etiquette is followed using lucode2::lint().
                ## Only checks files you changed.
	Rscript -e 'lucode2::lint()'

lint-all:       ## Check if code etiquette is followed using lucode2::lint().
                ## Checks all files.
	Rscript -e 'lucode2::lint(".")'

format:         ## Apply auto-formatting to changed files and lint afterwards.
	Rscript -e 'lucode2::autoFormat()'

format-all:     ## Apply auto-formatting to all files and lint afterwards.
	Rscript -e 'lucode2::autoFormat(files=list.files("./R", full.names = TRUE, pattern = "\\.R"))'

install:        ## Install the package locally via devtools::install() after
                ## generating NAMESPACE and docs (see docs target).
	Rscript -e 'roxygen2::roxygenize(); devtools::install(upgrade = "never")'

docs:           ## Generate the package documentation (man/*.Rd files) and
                ## NAMESPACE via roxygen2::roxygenize(), view the generated
                ## documentation with `?package::function`.
	Rscript -e 'roxygen2::roxygenize()'
