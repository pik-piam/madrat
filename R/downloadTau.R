#' @importFrom utils download.file unzip
downloadTau <- function(subtype = "paper") {
  # Define subtype-specific elements of the meta data. Elements that are common to all subtypes are added further down.
  settings <- list(paper = list(title = "Tau Factor (cellular, crop-specific)",
                                description = paste("Cellular (0.5deg), crop-specific land use intensity (tau)",
                                                    "for 1995 and 2000"),
                                url = paste0(c("https://rse.pik-potsdam.de/data/madrat/",
                                               "https://zenodo.org/record/4282581/files/"), "tau-paper.zip"),
                                doi = "10.5281/zenodo.4282581"),
                   historical = list(title = "Tau Factor (historic trends)",
                                     description = "Historic land use intensity (tau) development",
                                     url = paste0(c("https://rse.pik-potsdam.de/data/madrat/",
                                                    "https://zenodo.org/record/4282548/files/"), "tau-historical.zip"),
                                     doi = "10.5281/zenodo.4282548"))
  meta <- toolSubtypeSelect(subtype, settings)

  tryCatch({
    download.file(meta$url[1], destfile = "tau.zip",
                  quiet = requireNamespace("testthat", quietly = TRUE) && testthat::is_testing())
    meta$url <- meta$url[1]
  },
  error = function(e) {
    download.file(meta$url[2], destfile = "tau.zip",
                  quiet = requireNamespace("testthat", quietly = TRUE) && testthat::is_testing())
  })
  if (length(meta$url) == 2) meta$url <- meta$url[2]
  unzip("tau.zip")
  unlink("tau.zip")

  # Compose meta data by adding elements that are the same for all subtypes.
  return(list(url           = meta$url,
              doi           = meta$doi,
              title         = meta$title,
              description   = meta$description,
              author        = person("Jan Philipp", "Dietrich", email = "dietrich@pik-potsdam.de",
                                     comment = "https://orcid.org/0000-0002-4309-6431"),
              unit          = "1",
              version       = "1.0",
              release_date  = "2012-05-10",
              license       = "Creative Commons Attribution-ShareAlike 4.0 International License (CC BY-SA 4.0)",
              reference     = bibentry("Article",
                                       title = paste("Measuring agricultural land-use intensity -",
                                                     "A global analysis using a model-assisted approach"),
                                       author = c(person("Jan Philipp", "Dietrich", email = "dietrich@pik-potsdam.de",
                                                         comment = "https://orcid.org/0000-0002-4309-6431"),
                                                  person("Christoph", "Schmitz"),
                                                  person("Christoph", "Mueller"),
                                                  person("Marianela", "Fader"),
                                                  person("Hermann", "Lotze-Campen"),
                                                  person("Alexander", "Popp")),
                                       year = "2012",
                                       journal = "Ecological Modelling",
                                       volume = "232",
                                       pages = "109-118",
                                       url = "https://doi.org/10.1016/j.ecolmodel.2012.03.002",
                                       doi = "10.1016/j.ecolmodel.2012.03.002"))
  )
}
