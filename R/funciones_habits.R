# Funciones paquete ilde ais AVM: Habits

#' Encuentra ID del distrito para un dataframe de coordenadas
#'
#' A partir de un dataframe de lng/lat (en ese orden y sin NA) encuentra la ID del distrito al que corresponden las coordenadas
#' @param coordenadas dataframe con columnas lat y lng numéricas sin NAs
#' @param mapa objeto SpatialPolygonsDataFrame (i.e un .shp leido en R) para la región de las coordenadas
#' @param nivel a que nivel se busca la ID ('COD_DISTRI', 'COMUNA' o 'MANZENT' para distrito, comuna y manzana respectivamente para mapas usados en WebService 2018). Default 'COD_DISTRI'
#' @keywords id distrito habits 
#' @export
#' @examples
#' 
get_id <- function(coordenadas, mapa, nivel = 'COD_DISTRI'){
  coordinates(coordenadas) <- coordenadas
  proj4string(coordenadas) <- proj4string(mapa)

  ## Recuperar id manzana, ir actualizando el vector de NAs con las manzanas que va encontrando en cada iteracion
  out <- over(coordenadas, mapa)
  kk <- (as.character(out[, nivel]))
  #distritos[!is.na(kk)] <- kk[!is.na(kk)]
  return(kk)

}

#' Corrige número de caracteres que debería tener la ID de rigor
#'
#' A partir de un dataframe de lng/lat (en ese orden y sin NA) encuentra la ID del distrito al que corresponden las coordenadas
#' @param obj vector con las IDs a algun nivel (comuna, distrito, manzana)
#' @param id a que ID corresponde el vector ("ID_COM", "ID_DIS", "ID_MAN")
#' @keywords id distrito habits 
#' @export
#' @examples
#' 
check_length <- function(obj, id){
  n_car <- nchar(obj)

  if (id=="ID_COM"){
    obj_fixed <- ifelse(n_car == 4, paste(0,obj,sep=""), obj)
  } else if (id=="ID_DIS"){
    obj_fixed <- ifelse(n_car == 1, paste(0,obj,sep=""), obj)
  } else {
    obj_fixed <- ifelse(n_car == 13, paste(0,obj, sep=""), obj)
  }

  return(obj_fixed)
}

#' Que ID corresponde usar para pegar habits
#'
#' A partir de un dataframe que contiene las columnas ID_COM, ID_DIS e ID_MAN devuelve una columna con la ID que corresponde usar para pegar habits
#' @param df dataframe con las columnas ID_COM, ID_DIS e ID_MAN obtenidas a partir de la geolocalización y los mapas del país
#' @keywords id habits 
#' @export
#' @examples
#' 
cual_id <- function(df){
  #Se supone que el df tiene los codigos de manzana, distrito y comuna
  id_usada  <- ifelse(!is.na(df$ID_MAN), "ID_MAN", ifelse(!is.na(df$ID_DIS), "ID_DIS", "ID_COM"))
  return(id_usada) 
}


#' Elegir ID que finalmente se va a utilizar
#'
#' A partir de un dataframe que contiene las columnas cual_id_usar, ID_COM, ID_DIS e ID_MAN selecciona la ID para hacer mas facil un siguiente merge con habits
#' @param df dataframe con las columnas cual_id_usar, ID_COM, ID_DIS e ID_MAN obtenidas a partir de la geolocalización y los mapas del país
#' @keywords id habits 
#' @export
#' @examples
#' 
elegir_id <- function(df){
  #Se supone que el df tiene los codigos de manzana, distrito y comuna
  id_elegida  <- ifelse(!is.na(df$ID_MAN), df$ID_MAN, ifelse(!is.na(df$ID_DIS), df$ID_DIS, df$ID_COM))
  return(id_elegida) 
}
