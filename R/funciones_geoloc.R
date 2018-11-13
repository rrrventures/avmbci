# Funciones paquete ilde ais AVM: Normalizacion y Geolocalizacion

#' norm1
#'
#' Estandariza una direccion arreglando errores comunes y exceso de informacion
#' @param direcciones direccion (caracter) a geolocalizar
#' @keywords direccion geolocalizar 
#' @export
#' @examples
#' norm1("Pje. Dominic Dicoco 15 bod 15 dp 34 r ")

norm1 <- function(direcciones){
	##To upper case
	cDireccion <- stringr::str_to_upper(direcciones)

	##Un par de pruebas. 
	comas <- "\\,"
	gatos <- "\\#"
	puntos <- "\\."

	##Se cambian los gatos y puntos por espacios vacios
	cDireccion <- stringr::str_replace_all(cDireccion,gatos," ")
	cDireccion <- stringr::str_replace_all(cDireccion,puntos," ")
	cDireccion <- stringr::str_replace_all(cDireccion,comas," ") ## Las comas a veces delimitan ciudades/pais, como está escrito acá se ignora esa info

	## Pensé que las diferentes maneras de escribir pasajes iba a ser problema pero en general google es inteligente PSJE PSJ y sus variaciones
	## google las toma como iguales
	## si se quisiera estandarizar de todas formas  
	#Quedarse solo con letras y numeros
	pattern <- "[^a-zA-Z0-9\\/ÁÉÍÓÚñÑ]"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")


	#estandarizar algunas cosas
	cDireccion <- stringr::str_replace(cDireccion," *PSJE | *PSJ | *PJ | *PJE " ," PASAJE ")
	cDireccion <- stringr::str_replace(cDireccion," *AVD | *AVDA | *AV " ," AVENIDA ")
	cDireccion <- stringr::str_replace(cDireccion," D | *DEPTO | *DPTO | DP | DEP " ," DEPARTAMENTO ")
	cDireccion <- stringr::str_replace(cDireccion," CS " ," CASA ")

	#Eliminar letras sueltas
	pattern <- " [A-Z]{1} "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")


	## Eliminar infos varias 
	pattern <- " BX +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " BX +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")


	pattern <- " MT +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " MT +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")

	pattern <- " LT +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " LT +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")


	pattern <- " EST +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " EST +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")

	pattern <- " ST +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " ST +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")

	pattern <- " MZ +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " MZ +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")

	pattern <- " BD +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " BD +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")

	pattern <- " BOD +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " BOD +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")

	pattern <- " DEPARTAMENTO +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " DEPARTAMENTO +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")

	pattern <- " CASA +[0-9]+ "
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")
	pattern <- " CASA +[0-9]+$"
	cDireccion <- stringr::str_replace_all(cDireccion,pattern," ")



	return(cDireccion)

}


#' norm2
#'
#' Se queda solo con la parte de la direccion relevante DIRECCION + NUMERO
#' @param direcciones direccion (caracter) a geolocalizar
#' @keywords direccion geolocalizar 
#' @export
#' @examples
#' norm2("CALLE SECRETA 123 DP 1293 BOD 45")

norm2 <- function(direccion){

	## Este patron simplifica la direccion. Elimina numeros extras e informacion despues del numero de casa/edificio
	pattern <- "[A-z]+ +[0-9]+ ?"
	##dires_limpias <- as.vector(sapply(cDireccion, function(x) str_sub(x,1,str_locate(x,pattern)[1,2])))
	fin <- stringr::str_locate(direccion,pattern)[,2]
	direccion <- stringr::str_sub(direccion, 1, fin)
	direccion <- gsub("\\s+"," ",direccion)
	dires_limpias <- direccion

	return(dires_limpias)

} 




#' geocode_it
#'
#' Geolocaliza una dirección (texto) devolviendo un data frame con latitud y longitud
#' @param direccion direccion de la propiedad. Idealmente Calle + Numero + Comuna + Pais
#' @param key identificación para poder utilizar el servicio de google. Para mas información ver como obtener key en google. Actualmente hay una asociada a cuenta aischile.pruebas@gmail.com, pass aischile902
#' @keywords direccion geolocalizar 
#' @export
#' @examples
#' 
geocode_it <- function(direccion, key = NULL){
  
  df_geo <-  data.frame(lat = NA, lng = NA)
  loc <- googleway::google_geocode(direccion, key = key, simplify = TRUE)
  if (length(loc$results) > 0) {
    df_geo[1,c("lat","lng")] <- loc$results$geometry$location
  }
  
  return(df_geo)
}