% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/REPPlabShiny.R
\name{REPPlabShiny}
\alias{REPPlabShiny}
\title{Exploratory Projection Pursuit with a Shiny App}
\usage{
REPPlabShiny(x)
}
\arguments{
\item{x}{data matrix or data frame. Can also contain non-numerical variables. The user can choose in the app which rows and columns will be used.}
}
\value{
Returns an object of class \code{epplabshiny}. The object consists of a list of length five containing the calculated \code{\link[REPPlab]{EPPlab}} objects each having class \code{epplab}. If not five \code{epplab} objects were computed, the corresponding list entry is an empty list.  Note that print here only summarizes the output in order not to clutter the screen.
}
\description{
Performs exploratory projection pursuit as implemented in 'REPPlab' via a shiny app where the user can calculate up to five EPPlab objects and work with the results. Suitable for outlier detection or cluster identification. The actions in the Shiny app can be saved for further processing in R.
}
\examples{
if(interactive()){

 data(ReliabilityData)
 str(ReliabilityData)
 Repplablistshiny <- REPPlabShiny(ReliabilityData)
 Repplablistshiny
 str(Repplablistshiny)
 }
}
\references{
Fischer, D., Berro, A, Nordhausen, K. and Ruiz-Gazen, A. (2019),
\emph{REPPlab: An R package for detecting clusters and outliers using exploratory projection pursuit},
Communications in Statistics - Simulation and Computation, \url{doi:10.1080/03610918.2019.1626880}.
}
\seealso{
\href{https://shiny.posit.co/}{Shiny},
\code{\link[REPPlab]{EPPlab}}
}
\keyword{multivariate}
