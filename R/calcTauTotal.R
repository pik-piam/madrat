#' Calculate total tau
#' 
#' This function prepares total tau values for use. As the source data already
#' provides all required information this function purely removes not required
#' data and moves xref values to the weighting object which is required for
#' aggregation.
#' 
#' 
#' @return Total tau data and corresponding weights as a list of two MAgPIE
#' objects
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}, \code{\link{readTau}},
#' \code{\link{convertTau}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TauTotal")
#' 
#' }
#' @importFrom utils bibentry person

calcTauTotal <- function() {
  tau    <- readSource("Tau","paper")
  x      <- collapseNames(tau[,,"tau.total"])
  weight <- collapseNames(tau[,,"xref.total"])
  return(list(x=x,
              weight=weight,
              min=0,
              max=10,
              unit="-",
              description="Agricultural Land Use Intensity Tau",
              note=c('data based on Dietrich J.P., Schmitz C., M\uFCller C., Fader M., Lotze-Campen H., Popp A.,',
                     'Measuring agricultural land-use intensity - A global analysis using a model-assisted approach',
                     'Ecological Modelling, Volume 232, 10 May 2012, Pages 109-118, ISSN 0304-3800, 10.1016/j.ecolmodel.2012.03.002.',
                     'preprint available \u40 http://edoc.gfz-potsdam.de/pik/display.epl?mode=doc&id=5281'),
              source=bibentry("Article",
                              title="Measuring agricultural land-use intensity - A global analysis using a model-assisted approach",
                              author=c(person("Jan Philipp","Dietrich"),person("Christoph","Schmitz"),person("Christoph","Mueller"),person("Marianela","Fader"),
                                       person("Hermann","Lotze-Campen"),person("Alexander","Popp")),
                              year="2012",
                              journal="Ecological Modelling",
                              volume="232",
                              pages="109-118",
                              url="http://edoc.gfz-potsdam.de/pik/display.epl?mode=doc&id=5281",
                              doi="10.1016/j.ecolmodel.2012.03.002")))
}
