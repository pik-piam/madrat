#' toolAggregate
#'
#' (Dis-)aggregates a magclass object from one resolution to another based on a
#' relation matrix or mapping
#'
#' Basically toolAggregate is doing nothing more than a normal matrix
#' multiplication which is taking into account the 3 dimensional structure of
#' MAgPIE objects. So, you can provide any kind of relation matrix you would
#' like. However, for easier usability it is also possible to provide weights
#' for a weighted (dis-)aggregation as a MAgPIE object. In this case rel must
#' be a 1-0-matrix or a mapping between both resolutions. The weight
#' needs to be provided in the higher spatial aggregation, meaning for
#' aggregation the spatial resolution of your input data and in the case of
#' disaggregation the spatial resolution of your output data. The temporal and
#' data dimension must be either identical to the resolution of the data set
#' that should be (dis-)aggregated or 1. If the temporal and/or data dimension
#' is 1 this means that the same transformation matrix is applied for all years
#' and/or all data columns. In the case that a column should be just summed up
#' instead of being calculated as a weighted average you either do not provide
#' any weight (then all columns are just summed up) or your set this specific
#' weighting column to NA and mixed_aggregation to TRUE.
#'
#' @param x magclass object that should be (dis-)aggregated
#' @param rel relation matrix, mapping or file containing a mapping in a format
#' supported by \code{\link{toolGetMapping}} (currently csv, rds or rda).
#' A mapping object consists of any number of columns, where one column contains
#' all the elements in x. These elements are mapped to the corresponding values
#' in another column, as described below (see parameter 'from').
#' It is possible to not set \code{rel} as long as \code{to} is set and \code{dim}
#' is chosen appropriately. In that case the relation mapping is extracted from
#' the dimnames of the corresponding dimension, e.g. if your data contains a
#' spatial subdimension "country" you can aggregate to countries via
#' \code{toolAggregate(x, to = "country", dim = 1)}.
#' @param weight magclass object containing weights which should be considered
#' for a weighted aggregation. The provided weight should only contain positive
#' values, but does not need to be normalized (any positive number>=0 is allowed).
#' Please see the "details" section below for more information.
#' @param from Name of source column to be used in rel if it is a
#' mapping (if not set the first column matching the data will be used).
#' @param to Name of the target column to be used in rel if it is a
#' mapping (if not set the column following column \code{from} will be used
#' If column \code{from} is the last column, the column before \code{from is
#' used}). If data should be aggregated based on more than one column these
#' columns can be specified via "+", e.g. "region+global" if the data should
#' be aggregated to column regional as well as column global.
#' If \code{rel} is missing \code{to} refers to the aggregation target dimension name.
#' @param dim Specifying the dimension of the magclass object that should be
#' (dis-)aggregated. Either specified as an integer
#' (1=spatial,2=temporal,3=data) or if you want to specify a sub dimension
#' specified by name of that dimension or position within the given dimension
#' (e.g. 3.2 means the 2nd data dimension, 3.8 means the 8th data dimension).
#' @param wdim Specifying the according weight dimension as chosen with dim
#' for the aggregation object. If set to NULL the function will try to
#' automatically detect the dimension.
#' @param partrel If set to TRUE allows that the relation matrix does contain
#' less entries than x and vice versa. These values without relation are lost
#' in the output.
#' @param negative_weight Describes how a negative weight should be treated. "allow"
#' means that it just should be accepted (dangerous), "warn" returns a warning and
#' "stop" will throw an error in case of negative values
#' @param mixed_aggregation boolean which allows for mixed aggregation (weighted
#' mean mixed with summations). If set to TRUE weight columns filled with NA
#' will lead to summation.
#' @param verbosity Verbosity level of messages coming from the function: -1 = error,
#' 0 = warning, 1 = note, 2 = additional information, >2 = no message
#' @param zeroWeight Describes how a weight sum of 0 for a category/aggregation target should be treated.
#' "allow" accepts it and returns 0 (dangerous), "setNA" returns NA, "warn" throws a warning, "stop" throws an error.
#' @return the aggregated data in magclass format
#' @author Jan Philipp Dietrich, Ulrich Kreidenweis
#' @export
#' @importFrom magclass wrap ndata fulldim clean_magpie mselect setCells getCells mbind
#' @importFrom magclass setComment getNames getNames<- as.array
#' @importFrom magclass is.magpie getComment getComment<- dimCode getYears getYears<-
#' @importFrom magclass getDim getSets getSets<- as.magpie getItems collapseNames
#' @importFrom Matrix Matrix t rowSums
#' @importFrom withr local_options
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' # create example mapping
#' p <- magclass::maxample("pop")
#' mapping <- data.frame(from = magclass::getItems(p, dim = 1.1),
#'                       region = rep(c("REG1", "REG2"), 5),
#'                       global = "GLO")
#' print(mapping)
#'
#' # run aggregation
#' toolAggregate(p, mapping)
#' # weighted aggregation
#' toolAggregate(p, mapping, weight = p)
#' # combined aggregation across two columns
#' toolAggregate(p, mapping, to = "region+global")
#'
toolAggregate <- function(x, rel, weight = NULL, from = NULL, to = NULL, dim = 1, wdim = NULL, partrel = FALSE, # nolint
                          negative_weight = "warn", mixed_aggregation = FALSE, verbosity = 1, zeroWeight = "warn") { # nolint

  if (!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")

  comment <- getComment(x)

  # create missing rel information from dimension names if argument "to" is set
  if (missing(rel) && !is.null(to)) {
    if (round(dim) != dim) stop("Subdimensions in dim not supported if relation mapping is missing!")
    rel <- data.frame(c(dimnames(x)[dim], getItems(x, dim = dim, split = TRUE, full = TRUE)))
    if (dim == 1 && is.null(rel$global)) {
      rel$global <- "GLO"  # add global column
    }
    if (is.null(rel$all)) {
      rel$all <- "all"
    }
  }

  .reorder <- function(x, e, dim) {
    if (dim == 1) return(x[e, , ])
    if (dim == 2) return(x[, e, ])
    if (dim == 3) return(x[, , e])
  }

  .isMatrix <- function(x) {
    dsclasses <- c("dgCMatrix", "dsCMatrix", "dsyMatrix")
    return(any(is.element(class(x), dsclasses)) || ("Matrix" %in% attr(class(x), "package")))
  }

  if (!is.numeric(rel) && !.isMatrix(rel)) {
    .getAggregationMatrix <- function(rel, from = NULL, to = NULL, items = NULL, partrel = FALSE) {

      if ("tbl" %in% class(rel)) rel <- data.frame(rel)
      if (!(is.matrix(rel) || is.data.frame(rel))) {
        if (length(rel) > 1) stop("Malformed relation mapping!")
        if (!file.exists(rel)) {
          stop("Cannot find region mapping file: ", rel, " (working directory ", getwd(), ")")
        }
        rel <- toolGetMapping(rel, where = "local")
      }
      if (is.matrix(rel)) rel <- as.data.frame(rel)

      if (length(rel) < 2) stop("relation mapping has only ", length(rel), " column!")

      if (is.null(from)) {
        if (partrel) {
          from <- as.integer(which(sapply(lapply(rel, intersect, items), length) > 0)) # nolint
        } else {
          from <- as.integer(which(sapply(rel, setequal, items))) # nolint
        }
        if (length(from) == 0) {
          maxMatchColumn <- which.max(sapply(lapply(rel, intersect, items), length)) # nolint
          unmappedItems <- setdiff(items, rel[[maxMatchColumn]])
          missingItems <- setdiff(rel[[maxMatchColumn]], items)

          if (length(unmappedItems) > 0) {
            warning(paste("The following items were not found in the mapping:", toString(unmappedItems)))
          }
          if (length(missingItems) > 0) {
            warning(paste("The mapping expected the following items, but they are missing:", toString(missingItems)))
          }
          stop("Complete mapping failed. If you want a partial mapping, call toolAggregate(..., partrel = TRUE).")
        }
        if (length(from) > 1) from <- from[1]
      }
      if (is.null(to)) {
        if (from == length(rel)) {
          to <- from - 1
        } else {
          to <- from + 1
        }
      }

      regions <- as.character(unique(rel[, to]))
      countries <- as.character(unique(rel[, from]))
      m <- Matrix(data = 0, nrow = length(regions), ncol = length(countries),
                  dimnames = list(regions = regions, countries = countries))
      m[cbind(match(rel[, to], rownames(m)), match(rel[, from], colnames(m)))] <- 1
      if (is.numeric(to)) to <- dimnames(rel)[[2]][to]
      if (is.numeric(from)) from <- dimnames(rel)[[2]][from]
      names(dimnames(m)) <- c(to, from)
      return(m)
    }
    if (length(to) == 1 && grepl("+", to, fixed = TRUE)) {
      tmprel <- NULL
      toSplit <- strsplit(to, "+", fixed = TRUE)[[1]]
      for (t in toSplit) {
        tmp <- .getAggregationMatrix(rel, from = from, to = t, items = getItems(x, dim = dim), partrel = partrel)
        tmprel <- rbind(tmprel, tmp)
      }
      rel <- tmprel
    } else {
      rel <- .getAggregationMatrix(rel, from = from, to = to, items = getItems(x, dim = dim), partrel = partrel)
      if (is.null(to)) to  <- names(dimnames(rel))[1]
    }
  }

  # translate dim to dim code
  dim <- dimCode(dim, x, missing = "stop")


  # allow the aggregation, even if not for every entry in the initial dataset
  # there is a respective one in the relation matrix
  if (partrel) {
    datnames <-  getItems(x, dim)

    common <- intersect(datnames, colnames(rel))
    x <- .reorder(x, common, floor(dim))

    # datanames not in relnames
    noagg <- datnames[!datnames %in% colnames(rel)]
    if (length(noagg) > 0) {
      if (length(noagg) > 1) noagg[seq_len(length(noagg) - 1)] <- paste0(noagg[seq_len(length(noagg) - 1)], ", ")
      vcat(verbosity, noagg, " not mapped in aggregation!")
    }
    rel <- rel[, common, drop = FALSE]
    rel <- rel[rowSums(rel) > 0, , drop = FALSE]
  }

  if (!is.null(weight)) {
    if (!is.magpie(weight)) stop("Weight is not a MAgPIE object, weight has to be a MAgPIE object!")
    # get proper weight dim

    if (is.null(wdim)) {
      wdim <- union(getDim(rownames(rel), weight, fullmatch = TRUE),
                    getDim(colnames(rel), weight, fullmatch = TRUE))
      # wdim must be in same main dimension as dim
      wdim <- wdim[floor(wdim) == floor(dim)]
    }

    if (length(wdim) == 0) stop("Could not detect aggregation dimension in weight (no match)!")
    if (length(wdim) > 1) {
      # if full dimension and subdimension is matched, use only full dimension
      if (any(wdim == floor(dim))) wdim <- floor(dim)
      else stop("Could not detect aggregation dimension in weight (multiple matches)!")
    }

    if (floor(dim) == dim) wdim <- floor(wdim)

    if (anyNA(weight)) {
      if (!mixed_aggregation) {
        stop("Weight contains NAs which is only allowed if mixed_aggregation=TRUE!")
      } else {
        n <- length(getItems(weight, dim = wdim))
        r <- dimSums(is.na(weight), dim = wdim)
        if (!all(r %in% c(0, n))) stop("Weight contains columns with a mix of NAs and numbers which is not allowed!")
      }
    }
    if (nyears(weight) == 1) getYears(weight) <- NULL
    weight <- collapseNames(weight)
    if (negative_weight != "allow" && any(weight < 0, na.rm = TRUE)) {
      if (negative_weight == "warn") {
        warning("Negative numbers in weight. Dangerous, was it really intended?")
      } else {
        stop("Negative numbers in weight. Weight should be positive!")
      }
    }
    weightSum <- toolAggregate(weight, rel, from = from, to = to, dim = wdim, partrel = partrel, verbosity = 10)
    if (zeroWeight != "allow" && any(weightSum == 0, na.rm = TRUE)) {
      if (zeroWeight == "warn") {
        warning("Weight sum is 0, so cannot normalize and will return 0 for some ",
                "aggregation targets. This changes the total sum of the magpie object! ",
                'If this is really intended set zeroWeight = "allow", or "setNA" to return NA.')
      } else if (zeroWeight == "setNA") {
        weightSum[weightSum == 0] <- NA
      } else {
        stop("Weight sum is 0, so cannot normalize. This changes the total sum of the magpie object!")
      }
    }
    weight2 <- 1 / (weightSum + 10^-100)
    if (mixed_aggregation) {
      weight2[is.na(weight2)] <- 1
      weight[is.na(weight)] <- 1
    }

    if (wdim != floor(wdim)) {
      getSets(weight)[paste0("d", wdim)]  <- getSets(x)[paste0("d", dim)]
      getSets(weight2)[paste0("d", wdim)] <- getSets(x)[paste0("d", dim)]
    }

    if (setequal(getItems(weight, dim = wdim), getItems(x, dim = dim))) {
      out <- toolAggregate(x * weight, rel, from = from, to = to, dim = dim, partrel = partrel) * weight2
    } else {
      out <- toolAggregate(x * weight2, rel, from = from, to = to, dim = dim, partrel = partrel) * weight
    }
    getComment(out) <- c(comment, paste0("Data aggregated (toolAggregate): ", date()))
    return(out)
  } else {

    # convert rel for better performance
    rel <- Matrix(rel)

    # make sure that rel and weight cover a whole dimension (not only a subdimension)
    # expand data if necessary
    # set dim to main dimension afterwards
    if (round(dim) != dim) {
      .expandRel <- function(rel, names, dim) {
        # Expand rel matrix to full dimension if rel is only provided for a subdimension
        if (is.null(colnames(rel)) || is.null(rownames(rel))) {
          stop("colnames and/or rownames missing in relation matrix!")
        }

        if (round(dim) == dim || suppressWarnings(all(colnames(rel) == names))) return(rel)

        subdim <- round((dim - floor(dim)) * 10)
        maxdim <- nchar(gsub("[^\\.]", "", names[1])) + 1

        search <- paste0("^(", paste(rep("[^\\.]*\\.", subdim - 1), collapse = ""),
                         ")([^\\.]*)(", paste(rep("\\.[^\\.]*", maxdim - subdim), collapse = ""), ")$")
        onlynames <- unique(sub(search, "\\2", names))

        if (!setequal(colnames(rel), onlynames)) {
          if (!setequal(rownames(rel), onlynames)) {
            stop("The provided mapping contains entries which could not be found in the data: ",
                 paste(setdiff(colnames(rel), onlynames), collapse = ", "))
          } else  {
            rel <- t(rel)
          }
        }

        tmp <- unique(sub(search, "\\1#|TBR|#\\3", names))
        additions <- strsplit(tmp, split = "#|TBR|#", fixed = TRUE)
        add <- sapply(additions, function(x) return(x[1:2])) # nolint
        add[is.na(add)] <- ""
        .tmp <- function(add, fill) {
          return(paste0(rep(add[1, ], each = length(fill)), fill,
                        rep(add[2, ], each = length(fill))))
        }

        cnames <- .tmp(add, colnames(rel))
        rnames <- .tmp(add, rownames(rel))

        newRel <- Matrix(0, nrow = length(rnames), ncol = length(cnames), dimnames = list(rnames, cnames))

        for (i in seq_along(additions)) {
          newRel[seq_len(nrow(rel)) + (i - 1) * nrow(rel), seq_len(ncol(rel)) + (i - 1) * ncol(rel)] <- rel
        }
        return(newRel[, names, drop = FALSE])
      }
      rel <- .expandRel(rel, getItems(x, round(floor(dim))), dim)
      dim <- round(floor(dim))
    }

    if (dim(x)[dim] != dim(rel)[2]) {
      if (dim(x)[dim] != dim(rel)[1]) {
        stop("Relation matrix has in both dimensions a different number of entries (",
             dim(rel)[1], ", ", dim(rel)[2], ") than x has cells (", dim(x)[dim], ")!")
      } else {
        rel <- t(rel)
      }
    } else if (dim(x)[dim] == dim(rel)[1] && !setequal(colnames(rel), getItems(x, dim))) {
      rel <- t(rel)
    }

    # reorder MAgPIE object based on column names of relation matrix if available
    if (!is.null(colnames(rel))) x <- .reorder(x, colnames(rel), dim)

    # Aggregate data
    if (anyNA(x) || any(is.infinite(x))) {
      matrixMultiplication <- function(y, x) {
        if (any(is.infinite(y))) {
          # Special Inf treatment to prevent that a single Inf in x
          # is setting the full output to NaN (because 0*Inf is NaN)
          # Infs are now treated in a way that anything except 0 times Inf
          # leads to NaN, but 0 times Inf leads to NaN
          for (i in c(-Inf, Inf)) {
            j <- (is.infinite(y) & (y == i))
            x[, j][x[, j] != 0] <- i
            y[j] <- 1
          }
        }
        if (any(is.na(y)) && !all(is.na(y))) {
          # Special NA treatment to prevent that a single NA in x
          # is setting the full output to NA (because 0*NA is NA)
          # NAs are now treated in a way that anything except 0 times NA
          # leads to NA, but 0 times NA leads to 0
          x[, is.na(y)][x[, is.na(y)] != 0] <- NA
          y[is.na(y)] <- 0
        }
        return(as.array(x %*% y))
      }
      out <- apply(x, which(1:3 != dim), matrixMultiplication, rel)
      if (length(dim(out)) == 2) out <- array(out, dim = c(1, dim(out)), dimnames = c("", dimnames(out)))
    } else {
      local_options(matprod = "blas")
      notdim <- setdiff(1:3, dim)
      out <- rel %*% as.array(wrap(x, list(dim, notdim)))
      out <- array(out, dim = c(dim(rel)[1], dim(x)[notdim]))
      dimnames(out)[2:3] <- dimnames(x)[notdim]
    }

    # Write dimnames of aggregated dimension
    if (!is.null(rownames(rel))) {
      regOut <- rownames(rel)
    } else if (dim == 1) {
      regionList <- as.factor(getItems(x, dim = 1.1, full = TRUE))
      # Compute region vector for outputs after aggregation via sending
      # factor values through the relation matrix
      regOut <- factor(as.vector(round(rel %*% as.numeric(regionList) /
                                         (rel %*% rep(1, dim(rel)[2])))))
      levels(regOut) <- levels(regionList)
    } else {
      stop("Missing dimnames for aggregated dimension")
    }

    if (!any(grepl("\\.", regOut)) && anyDuplicated(regOut)) regOut <- paste(regOut, seq_len(dim(out)[1]), sep = ".")

    dimnames(out)[[1]] <- regOut

    if (dim == 2) out <- wrap(out, map = list(2, 1, 3))
    if (dim == 3) out <- wrap(out, map = list(2, 3, 1))

    sets <- getSets(x, fulldim = FALSE)
    # update set name if number of sub-dimensions reduced to 1
    if (ndim(out, dim = dim) == 1  && ndim(x, dim = dim) > 1) {
      sets[dim] <- ifelse(!is.null(to), to, NA)
    }
    getSets(out, fulldim = FALSE) <- sets

    getComment(out) <- c(comment, paste0("Data aggregated (toolAggregate): ", date()))
    out <- as.magpie(out, spatial = 1, temporal = 2)
    return(out)
  }
}
