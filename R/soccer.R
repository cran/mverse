#' Number of cards given for each referee-player pair in soccer.
#'
#' A dataset containing card counts between 2,053 soccer players
#' playing in the first male divisions of England, Germany, France,
#' and Spain in the 2012-2013 season and 3,147 referees
#' that these players played under in professional matches.
#' The dataset contains other covariates including 2 independent
#' skin tone ratings per player.
#' Each line represents a player-referee pair.
#'
#' @format A data frame with 146,028 rows and 26 variables:
#' \describe{
#'   \item{playerShort}{short player ID}
#'   \item{player}{player name}
#'   \item{club}{player club}
#'   \item{leagueCountry}{country of player club (
#'     England, Germany, France, and Spain)}
#'   \item{birthday}{player birthday}
#'   \item{height}{player height (in cm)}
#'   \item{weight}{player weight (in kg)}
#'   \item{position}{detailed player position}
#'   \item{games}{number of games in the player-referee dyad}
#'   \item{victories}{victories in the player-referee dyad}
#'   \item{ties}{ties in the player-referee dyad}
#'   \item{defeats}{losses in the player-referee dyad}
#'   \item{goals}{goals scored by a player in the player-referee dyad}
#'   \item{yellowCards}{number of yellow cards player received from referee}
#'   \item{yellowReds}{number of yellow-red cards player received from referee}
#'   \item{redCards}{number of red cards player received from referee}
#'   \item{rater1}{skin rating of photo by rater 1
#'     (5-point scale ranging from “very light skin” to “very dark skin”)}
#'   \item{rater2}{skin rating of photo by rater 2
#'     (5-point scale ranging from “very light skin” to “very dark skin”)}
#'   \item{refNum}{unique referee ID number
#'     (referee name removed for anonymizing purposes)}
#'   \item{refCountry}{unique referee country ID number
#'     (country name removed for anonymizing purposes)}
#'   \item{meanIAT}{mean implicit bias score (using the race IAT)
#'     for referee country, higher values correspond to faster
#'     white | good, black | bad associations}
#'   \item{nIAT}{sample size for race IAT in that particular country}
#'   \item{seIAT}{standard error for mean estimate of race IAT}
#'   \item{meanExp}{mean explicit bias score (using a racial thermometer task)
#'     for referee country, higher values correspond to greater feelings of
#'     warmth toward whites versus blacks}
#'   \item{nExp}{sample size for explicit bias in that particular country}
#'   \item{seExp}{standard error for mean estimate of explicit bias measure}
#' }
#' @details
#' The skin colour of each player was rated by two independent raters,
#' \code{rater1} and \code{rater2}, and the 5-point scale values were
#' scaled to 0 to 1 - i.e., 0, 0.25, 0.5, 0.75, 1.
#'
#' @source
#' Silberzahn R, Uhlmann EL, Martin DP, et al.
#' {Many Analysts, One Data Set: Making Transparent How Variations in Analytic 
#' Choices Affect Results}. Advances in Methods and Practices in Psychological 
#' Science. 2018;1(3):337-356. \doi{10.1177/2515245917747646}
"soccer"
