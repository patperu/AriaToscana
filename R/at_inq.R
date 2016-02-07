
#' Measurements of Air Pollutants (2008 - 2014)
#'
#' This dataset contains the hourly measures of the following air pollutants:
#'
#' BENZENE 263040
#' CH4 184128
#' CO 1507846
#' EBENZENE 254256
#' H2S 210408
#' HCL 26304
#' M-XYLENE 192912
#' MP-XILENE 8784
#' MP-XYLENE 26280
#' N-EPTANO 70152
#' N-ESANO 70152
#' N-OTTANO 70152
#' NH3 61368
#' NMHC 184128
#' NO 3015888
#' NO2 3015888
#' NOX 3015888
#' O-XYLENE 245472
#' O3 1446620
#' P-XYLENE 227976
#' SO2 692616
#' TN 61368
#' TNX 61368
#' TOLUENE 263040
#'
#' @author Patrick Hausmann, Source: ARPAT Toscana
#' @format Data frame with columns
#' \describe{
#' \item{stazione}{Stazione, first two characters are the commune}
#' \item{parameter}{inquinante}
#' \item{year}{Year}
#' \item{month}{Month}
#' \item{day}{Day}
#' \item{hour}{Hour}
#' \item{value{Measure}
#' \item{valid}{0 = measure is not valid, 1 = measure is valid}
#' }
"at_inq"