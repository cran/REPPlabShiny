#' Exploratory Projection Pursuit with a Shiny App
#'
#' Performs exploratory projection pursuit as implemented in 'REPPlab' via a shiny app where the user can calculate up to five EPPlab objects and work with the results. Suitable for outlier detection or cluster identification. The actions in the Shiny app can be saved for further processing in R.
#' @import shiny
#' @import REPPlab
#' @importFrom DT renderDT DTOutput
#'
#' @param x data matrix or data frame. Can also contain non-numerical variables. The user can choose in the app which rows and columns will be used.
#'
#' @return Returns an object of class \code{epplabshiny}. The object consists of a list of length five containing the calculated \code{\link[REPPlab]{EPPlab}} objects each having class \code{epplab}. If not five \code{epplab} objects were computed, the corresponding list entry is an empty list.  Note that print here only summarizes the output in order not to clutter the screen.
#' 
#' @references 
#' Fischer, D., Berro, A, Nordhausen, K. and Ruiz-Gazen, A. (2019), 
#' \emph{REPPlab: An R package for detecting clusters and outliers using exploratory projection pursuit}, 
#' Communications in Statistics - Simulation and Computation, <doi:10.1080/03610918.2019.1626880>.
#' 
#' @seealso 
#' \href{https://shiny.posit.co/}{Shiny}, 
#' \code{\link[REPPlab]{EPPlab}}
#'
#' @keywords multivariate
#'
#' @examples
#' if(interactive()){
#' 
#'  data(ReliabilityData)
#'  str(ReliabilityData)
#'  Repplablistshiny <- REPPlabShiny(ReliabilityData)
#'  Repplablistshiny
#'  str(Repplablistshiny)
#'  }
#' @export
REPPlabShiny <- function (x)
{
  G <- .GlobalEnv
  assign("x", x, envir = G)
  appDir <- system.file("shiny-examples", "myapp", package = "REPPlabShiny")
  if (appDir == "") {
  stop("Could not find example directory. Try re-installing `REPPlabShiny`.", call. = FALSE)
  }

 shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}

#' Prints the 'REPPlabShiny' Results
#'
#' Prints some information about objects of class \code{epplabshiny}, typically the result of a call to \code{\link{REPPlabShiny}}. Printed is only a table with the names, indices and algorithms used for calculated \code{EPPlab} object, not the whole content of the object.
#' @import REPPlab
#' @import shiny
#'
#' @param x an object of class epplabshiny.
#' @param ... further arguments passed to or from other methods.

#' @seealso 
#' \code{\link[REPPlabShiny]{REPPlabShiny}}
#'
#' @keywords print
#'
#' @examples
#' if(interactive()){
#'  data(ReliabilityData)
#'  Repplablistshiny <- REPPlabShiny(ReliabilityData)
#'  print(Repplablistshiny)
#'  }
#' @export
print.epplabshiny <- function(x, ...) {
  eppalgo <- list(x$Epplabobject1$PPalg,
                  x$Epplabobject2$PPalg,
                  x$Epplabobject3$PPalg,
                  x$Epplabobject4$PPalg,
                  x$Epplabobject5$PPalg)
  
  eppindex <- list(x$Epplabobject1$PPindex,
                   x$Epplabobject2$PPindex,
                   x$Epplabobject3$PPindex,
                   x$Epplabobject4$PPindex,
                   x$Epplabobject5$PPindex)
  
  epptable <- cbind("Object" = names(x),"Index" = eppalgo, "Algorithm" = eppindex)
  print(epptable, ...)
}
