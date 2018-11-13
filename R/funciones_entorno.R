#' entorno_colegios
#'
#' Crea variables relacionada a la distancia de la propiedad a la base geolocalizada de colegios. Variables: distancia minima a colegio, existe colegio municipal cerca, existe colegio particular cerca, existe particular subvencionado cerca
#' @param registro propiedad geolocalizada para la que se buscan las variables de colegios. Deberia ser un data frame de una fila con columnas lat y lng
#' @param base_entorno base de datos de los colegios geolocalizados
#' @param distancia distancia máxima de propiedad a un colegio para generar las variables. Default 2000 metros
#' @keywords variables entorno colegios 
#' @export
#' @examples

entorno_colegios <- function(registro, base_entorno, distancia = 2000){
   dist_todos <- dist_imap(base_entorno,registro)
   ind_muni <- base_entorno$DEPENDENCIA =="Municipal"
   ind_part <- base_entorno$DEPENDENCIA =="Particular Pagada"
   ind_sub <- base_entorno$DEPENDENCIA =="Particular Subvencionada" 


   minx <- min(dist_todos,na.rm=TRUE)
   muni_cerca <- sum(dist_todos[ind_muni] < distancia) > 0
   part_cerca <- sum(dist_todos[ind_part] < distancia) > 0
   sub_cerca <- sum(dist_todos[ind_sub] < distancia) > 0

   df <- data.frame(dist_min = minx, muni_cerca = muni_cerca, part_cerca = part_cerca, sub_cerca = sub_cerca)
   return(df)
}

#' entorno_farmacias
#'
#' Crea variables relacionada a la distancia de la propiedad a la base geolocalizada de farmacias. Variables: se encuentra farmacia cerca, se encuentra farmacia a distancia media, cantidad de farmacias a distancia media
#' @param registro propiedad geolocalizada para la que se buscan las variables de colegios. Deberia ser un data frame de una fila con columnas lat y lng
#' @param base_entorno base de datos de los colegios geolocalizados
#' @param distancia distancia a buscar farmacias. Default 500 para "cerca", distancia media se toma el doble, default 1000
#' @keywords variables entorno farmacias 
#' @export
#' @examples

entorno_farmacias <- function(registro, base_entorno, distancia = 500){
   dist_todos <- dist_imap(base_entorno,registro)
   
   farm_cerca <- sum(dist_todos < distancia, na.rm=TRUE) > 0
   farm_cerca2 <- sum(dist_todos < distancia*2, na.rm=TRUE) > 0
   cant_cerca <- sum(dist_todos < distancia*2, na.rm=TRUE) 

   df <- data.frame(farm_cerca = farm_cerca, farm_cerca2 = farm_cerca2, cant_cerca = cant_cerca)

   return(df)
}