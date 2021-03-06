% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funciones_entorno.R
\name{entorno_colegios}
\alias{entorno_colegios}
\title{entorno_colegios}
\usage{
entorno_colegios(registro, base_entorno, distancia = 2000)
}
\arguments{
\item{registro}{propiedad geolocalizada para la que se buscan las variables de colegios. Deberia ser un data frame de una fila con columnas lat y lng}

\item{base_entorno}{base de datos de los colegios geolocalizados}

\item{distancia}{distancia máxima de propiedad a un colegio para generar las variables. Default 2000 metros}
}
\description{
Crea variables relacionada a la distancia de la propiedad a la base geolocalizada de colegios. Variables: distancia minima a colegio, existe colegio municipal cerca, existe colegio particular cerca, existe particular subvencionado cerca
}
\keyword{colegios}
\keyword{entorno}
\keyword{variables}
