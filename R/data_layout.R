#' Data layout_1
#'
#' This dataset contains information about participants, including demographic details and event-related data.
#'
#' @format A data frame with 6347 rows and 13 columns:
#' \describe{
#'   \item{ID}{participant ID}
#'   \item{PARAMCD}{parameter}
#'   \item{TSTART}{time study start}
#'   \item{TSTOP}{time stop observation}
#'   \item{TGAP}{time gap between events}
#'   \item{STATUS}{status of the event}
#'   \item{SEVENT}{sequence number of event per id}
#'   \item{AGE}{age of the participant}
#'   \item{SEX}{sex of the participant}
#'   \item{DISEASE_COURSE}{type of disease course}
#'   \item{RACE}{race of the participant}
#'   \item{TIME_SINCE_DIAGNOSIS}{time since diagnosis}
#' }
#' @source Simulated data for conditional and marginal rates models
"data_layout_1"

#' Data layout_2
#'
#' This dataset contains information about participants, including demographic details and event-related data.
#'
#' @format A data frame with 15765 rows and 12 columns:
#' \describe{
#'   \item{ID}{participant ID}
#'   \item{PARAMCD}{parameter}
#'   \item{TSTART}{time study start}
#'   \item{TSTOP}{time stop observation}
#'   \item{TGAP}{time gap between events}
#'   \item{STATUS}{status of the event}
#'   \item{SEVENT}{sequence number of event per id}
#'   \item{AGE}{age of the participant}
#'   \item{SEX}{sex of the participant}
#'   \item{DISEASE_COURSE}{type of disease course}
#'   \item{RACE}{race of the participant}
#'   \item{TIME_SINCE_DIAGNOSIS}{time since diagnosis}
#' }
#' @source Simulated data for marginal hazard models
"data_layout_2"

#' Data layout_3
#'
#' This dataset contains information about participants, including demographic details and event-related data.
#'
#' @format A data frame with 1313 rows and 9 columns:
#' \describe{
#'   \item{ID}{participant ID}
#'   \item{PARAMCD}{parameter}
#'   \item{COUNT}{number of confirmed events}
#'   \item{SEVENT}{sequence number of event per id}
#'   \item{AGE}{age of the participant}
#'   \item{SEX}{sex of the participant}
#'   \item{DISEASE_COURSE}{type of disease course}
#'   \item{RACE}{race of the participant}
#'   \item{TIME_SINCE_DIAGNOSIS}{time since diagnosis}
#' }
#' @source Simulated data for count-based models
"data_layout_3"
