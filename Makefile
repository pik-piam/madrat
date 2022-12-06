.PHONY: help build check test lint format
.DEFAULT_GOAL = help

help:           ## Show this help.
	@sed -e '/##/ !d' -e '/sed/ d' -e 's/^\([^ ]*\) *##\(.*\)/\1^\2/' \
		$(MAKEFILE_LIST) | column -ts '^'

build:          ## Build the package using lucode2::buildLibrary()
	Rscript -e 'lucode2::buildLibrary()'

check:          ## Build documentation and vignettes, run testthat tests,
                ## and check if code etiquette is followed using lucode2::check()
	Rscript -e 'lucode2::check()'

test:           ## Run testthat tests
	Rscript -e 'devtools::test(show_report = TRUE)'

lint:           ## Check if code etiquette is followed using lucode2::lint()
	Rscript -e 'lucode2::lint()'

format:         ## Apply auto-formatting to changed files and lint afterwards
	Rscript -e 'lucode2::autoFormat()'
