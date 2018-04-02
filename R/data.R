#' @title Italian Health Insurance Data
#' 
#' @description A list of data.frames containing transition probabilities by age (row) and year of projections
#' Transitions are split by males and females, and show probabilities of survival, death 
#' and transitions from Healty to Disabled
#' 
#' @format a list containing elevent items (data.frames), and an mdt data object (HealthyMaleTable2013)
#' @source PAOLO DE ANGELIS, LUIGI DI FALCO (a cura di). Assicurazioni sulla salute: caratteristiche, 
#' modelli attuariali e basi tecniche
"de_angelis_di_falco"

#' @title United States Social Security life tables
#' 
#' @description This data set contains period life tables for years 1990, 2000 and 2007. Both males and 
#' females life tables are reported.
#' @format A \code{data.frame} containing people surviving at the beginning of "age" at 2007, 2000, and 1990
#' split by gender
#' @details Reported age is truncated at the last age with lx>0.
#' @source See \url{http://www.ssa.gov/oact/NOTES/as120/LifeTables_Body.html}
#' @examples
#' data(demoUsa)
#' head(demoUsa)
"demoUsa"

#' @title Society of Actuaries life table
#' @description This table has been used by the classical book Actuarial Mathematics and by the Society 
#' of Actuaries for US professional examinations.
#' @usage data(soaLt)
#' @format A \code{data.frame} with 111 obs on the following 2 variables:
#' \describe{ \item{\code{x}}{a numeric vector} \item{\code{Ix}}{a numeric vector} }
#' @details Early ages have been found elsewere since miss in the original data sources; SOA did not 
#' provide population at risk data for certain spans of age (e.g. 1-5, 6-9, 11-14 and 16-19)
#' @references Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., 
#' Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
#' @examples 
#' data(soaLt)
#' head(soaLt)
"soaLt"