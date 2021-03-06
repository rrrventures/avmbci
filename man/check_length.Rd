% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funciones_habits.R
\name{check_length}
\alias{check_length}
\title{Corrige número de caracteres que debería tener la ID de rigor}
\usage{
check_length(obj, id)
}
\arguments{
\item{obj}{vector con las IDs a algun nivel (comuna, distrito, manzana)}

\item{id}{a que ID corresponde el vector ("ID_COM", "ID_DIS", "ID_MAN")}
}
\description{
A partir de un dataframe de lng/lat (en ese orden y sin NA) encuentra la ID del distrito al que corresponden las coordenadas
}
\examples{

}
\keyword{distrito}
\keyword{habits}
\keyword{id}
