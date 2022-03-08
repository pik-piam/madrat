#' Tool: ISOhistorical
#'
#' This function expects a MAgPIE object with ISO country codes in the spatial
#' dimension. For this MAgPIE object the time of transition is calculated and
#' for each the historic time filled by using the mapping stored as
#' supplementary data in the madrat package. If you want to use a different
#' mapping please specify it in the argument mapping
#'
#'
#' @param m MAgPIE object with ISO country codes in the spatial dimension
#' @param mapping mapping of historical ISO countries to the standard ISO
#' country list. For the default setting (mapping=NULL) the mapping stored as
#' supplementary data in the madrat package is used. If provided as file the
#' mapping needs to contain three columns "fromISO", "toISO" and "lastYear".
#' @param additional_mapping vector or list of vectors to provide some specific
#' mapping, first the old country code, second the new country code and last
#' the last year of the old country, e.g. additional_mapping =
#' c("TTT","TTX","y1111") or additional_mapping =
#' list(c("TTT","TTX","y1111"),c("TTT","TTY","y1111"))
#' @param overwrite if there are already historical data in the data source for
#' years that are calculated in this function they will not be overwritten by
#' default. To overwrite all data (e.g. if there are meaningless "0") choose
#' overwrite=TRUE
#' @param additional_weight optional weight to be used for regional disaggregation,
#' if not provided, the values of m in the "lastYear" are used as weight
#' @return A MAgPIE object with spatial entries for each country of the
#' official ISO code country list. Historical time is filled up, old countries
#' deleted
#' @author Lavinia Baumstark
#'
#' @importFrom magclass getItems getYears setYears
#'
#' @export
toolISOhistorical <- function(m, mapping = NULL, additional_mapping = NULL, overwrite = NA, # nolint
                              additional_weight = NULL) { # nolint
  # m is magpie object, has to contain absolute values

  # mapping of historical countries and regions to the standard ISO-Country-List
  #            and last year of existence of the historical countries
  if (is.null(mapping)) {
    mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"), stringsAsFactors = FALSE)
  } else if (is.character(mapping)) {
    mapping <- read.csv(mapping, sep = ";", as.is = TRUE)
  }
  # add additional mapping, if provided
  if (!is.null(additional_mapping)) {
    if (is.data.frame(additional_mapping) || !is.list(additional_mapping)) {
      mapping <- rbind(mapping, additional_mapping)
    } else {
      for (elem in additional_mapping) {
        mapping <- rbind(mapping, elem)
      }
    }
  }

  # sort mapping(transitions) in historical order -> needed for the correct filling of data
  mapping <- mapping[robustOrder(mapping$lastYear), ]
  # delete transitions from mapping which are not in the time horizon of m
  if (length(intersect(mapping$lastYear, getYears(m))) == 0) {
    warning("toolISOhistorical cannot find reference data before/after the union/split of countries, ",
            "returning unchanged data")
    return(m)
  }
  mapping <- subset(mapping,
                    mapping$lastYear <= max(intersect(mapping$lastYear, getYears(m))) &
                      mapping$lastYear >= min(intersect(mapping$lastYear, getYears(m))))

  # identify transitions that are in m
  transitions <- list()
  # list of regions that are transition countries and in the data m
  fromISOM  <- intersect(mapping$fromISO, getItems(m, dim = 1.1))
  # create matrix of possible transitions
  ptr <- NULL
  for (i in fromISOM) {
    # loop over number of different years of transition for one fromISOM-country
    for (l in seq_along(unique(mapping$lastYear[mapping$fromISO == i]))) {
      ptr <- rbind(ptr, c(i, unique(mapping$lastYear[mapping$fromISO == i])[l]))
    }
  }
  # sort again based on transition year if more than one transition exists
  if (length(ptr[, 1]) > 1) {
    ptr <- ptr[robustOrder(ptr[, 2]), ]
  }
  numberOfTransitions <- 0
  h <- NULL
  fromISOYear <- list()
  for (i in seq_along(ptr[, 1])) {
    if (!length(mapping$toISO[mapping$fromISO == ptr[i, 1]]) == 1 | i == length(ptr[, 1])) {
      numberOfTransitions <- numberOfTransitions + 1
      fromISOYear[[numberOfTransitions]] <- cbind(h, ptr[i, ])
      h <- NULL
    } else if (length(mapping$toISO[mapping$fromISO == ptr[i, 1]]) == 1) {
      to1 <- mapping$toISO[mapping$fromISO == ptr[i, 1]]
      to2 <- mapping$toISO[mapping$fromISO == ptr[i + 1, 1]]
      if (length(to1) != length(to2) || to1 != to2) {
        numberOfTransitions <- numberOfTransitions + 1
        fromISOYear[[numberOfTransitions]] <- cbind(h, ptr[i, ])
        h <- NULL
      } else {
        h <- cbind(h, ptr[i, ])
      }
    }
  }

  # collect information for all transitions
  for (i in seq_len(numberOfTransitions)) {
    fromISO <- fromISOYear[[i]][1, ]
    toISO   <- mapping$toISO[mapping$fromISO == fromISO[1] & mapping$lastYear == fromISOYear[[i]][2]]
    # take the maximum year of m that is lower than the transition year
    fromY   <- max(getYears(m)[getYears(m) <= fromISOYear[[i]][2]])
    # take the minimum of years that are later than fromY
    toY     <- min(getYears(m)[getYears(m) > fromY])
    transitions[[i]] <- list(fromISO = fromISO, toISO = toISO, fromY = fromY, toY = toY)
  }

  vcat(2, "The following transitions are found in the data \n", paste(transitions, collapse = ", \n"))

  # loop over all transitions
  for (transition in transitions) {
    # check if new regions of transition exists in m
    toISOmissing   <- !all(is.element(transition$toISO, getItems(m, dim = 1.1)))
    if (toISOmissing) stop("there is no data for the following new countrys: ",
                           paste(transition$toISO[which(!is.element(transition$toISO, getItems(m, dim = 1.1)))],
                                 collapse = ", "))

    # create transformation matrix
    ## time span where the data need to be adjusted
    subTime <- getYears(m[, seq_len(which(getYears(m) == transition$fromY)), ])
    # disaggregation of countries
    if (length(transition$fromISO) == 1) {
      if (is.null(additional_weight)) {
        weight <- setYears(m[transition$toISO, transition$toY, ], NULL)
        if (anyNA(weight)) {
          weight[is.na(weight)] <- 0
          vcat(0, "Weight in toolISOhistorical contained NAs. Set NAs to 0!")
        }
      } else {
        if (!all(transition$toISO %in% getItems(additional_weight, dim = 1))) {
          stop(paste0(
            "Invalid additional weight, missing countries: ",
            paste0(setdiff(transition$toISO, getItems(additional_weight, dim = 1)), collapse = ", ")
          ))
        }
        weight <- additional_weight[transition$toISO, , ]
      }

      mTr <- toolAggregate(m[transition$fromISO, subTime, ],
                           mapping[is.element(mapping$toISO, transition$toISO), c("fromISO", "toISO")], weight = weight,
                           negative_weight = "allow")
      ## aggregation of countries
    } else {
      mTr <- toolAggregate(m[transition$fromISO, subTime, ],
                           mapping[is.element(mapping$toISO, transition$toISO), c("fromISO", "toISO")], weight = NULL)
    }
    # fill data
    if (is.na(overwrite)) {
      zeroYears <- Filter(x = subTime, function(year) all(m[transition$toISO, year, ] == 0))
      if (length(zeroYears) > 0) {
        warning("Not replacing data for [", paste(transition$toISO, collapse = ", "), "] for the years [",
                paste(zeroYears, collapse = ", "), "] although it is all zeros. Call toolISOhistorical with ",
                "overwrite = TRUE/FALSE to get the desired behavior without this warning.")
      }
      overwrite <- FALSE
    }

    if (overwrite) {
      m[transition$toISO, subTime, ] <- mTr
    } else {
      # only overwrite NAs
      selectNAs <- is.na(m[transition$toISO, subTime, ])
      m[transition$toISO, subTime, ][selectNAs] <- mTr[selectNAs]
    }
  }

  # delete old lines
  for (b in mapping$fromISO) {
    if (is.element(b, getItems(m, dim = 1.1))) {
      m <- m[-which(getItems(m, dim = 1.1) == b), , ]
    }
  }

  return(m)
}
