
#' reducir_base_comparar
#'
#' Encuentra las distancias a los comparables (propiedades similares) de una propiedad en particular a la base de testigos
#' @param registro propiedad para la cual se quiere encontrar comparables. Deberia ser un data frame de una fila con la variable sol.sol_tip_bie, ("URCS CASAS"), cod_comuna_habits y cod_region_habits
#' @param df propiedad para la cual se quiere encontrar comparables. Deberia ser un data frame de una fila con la variable sol.sol_tip_bie, ("URCS CASAS")
#' @param distancia distancia maxima a propiedades comparables. Default 500 metros
#' @param variable_metraje el nombre de la variable de metraje en registro. Default sum_V8_b porque ese es el nombre del metraje en base SII
#' @param similitud valor entre 0-1 para comparar propiedades. Default 0.2 lo que significa que busca comparables que sean +-20% del metraje del registro
#' @keywords direccion geolocalizar 
#' @export
#' @examples

reducir_base_comparar <- function(registro, df, distancia = 500, variable_metraje = "sum_V8_b", similitud = 0.2, n_testigos = 4) {
  #registro vendria siendo una fila
  if(registro$sol.sol_tip_bie == "URCS CASAS"){
    ind <- df$tipo == "casa"
  } else {
    ind <- df$tipo == "depto"
  }

  if(registro$cod_region_habits != 13){
    ind2 <- df$region == registro$cod_region_habits
    ind3 <- ind2 & ind
  } else { 
  ind2 <- df$region == registro$cod_region_habits & as.numeric(df$comuna) == registro$cod_comuna_habits
    ind3 <- ind2 & ind
  }

  ###FALTA EL FILTRO POR METRAJE PARECIDO
  df_f <- df[ind3,]
  vec <- rep(NA,length(ind3))
  dist <- dist_imap(df_f,registro)
  vec[ind3] <- dist

  #FILTRO DE METRAJE
  indx <- vec < distancia
  df$distancias <- vec
  df_filt <- df[indx & !is.na(indx),]
  df_filt <- df_filt %>% dplyr::filter(metros < registro[[variable_metraje]]*(1 + similitud), metros > registro[[variable_metraje]]*(1 - similitud))
  df_filt <- df_filt %>% arrange_(~ distancias) %>% group_by_(~ arriendo) %>% slice(1:n_testigos)

  return(df_filt)
}


#' dist_imap
#'
#' Encuentra las distancias a los comparables (propiedades similares) de una propiedad en particular a la base de testigos
#' @param df_f dataframes de comparables. Debe tener columnas lat y lng sin NAs
#' @param registro propiedad para la cual se buscan testigos
#' @keywords distancias levershine propiedades testigos
#' @export
#' @examples

dist_imap <- function(df_f, registro){
  dist <- mapply(gdist,registro$lat,registro$lng,df_f$lat,df_f$lng, units="m")
  return(dist)
}


#' valores
#'
#' Calcula variables a partir de base de testigos. Precio promedio, precio m2 promedio, metraje promedio y variable secreta
#' @param df dataframes de testigos
#' @keywords distancias levershine propiedades testigos
#' @export
#' @examples
valores <- function(df){

  df_f <- df %>% select(arriendo, precio, p_m2, metros, n_banos, n_dorms)
  df_arriendo <- df %>% filter(arriendo)
  df_venta <- df %>% filter(!arriendo)

  valores_arriendo <- valores_aux(df_arriendo)
  valores_venta <- valores_aux(df_venta)

  vals_testigos <- c(valores_arriendo, valores_venta)
  names(vals_testigos) <- c("precio_prom_arriendo", "precio_m2_prom_arriendo", "m2_prom_arriendo", "ilde_arriendo",
                            "precio_prom_venta", "precio_m2_prom_venta", "m2_prom_venta", "ilde_venta")
  return(vals_testigos)
}



#' valores_aux
#'
#' Encuentra las distancias a los comparables (propiedades similares) de una propiedad en particular a la base de testigos
#' @param df dataframes de comparables. Debe tener columnas lat y lng sin NAs
#' @keywords distancias levershine propiedades testigos
#' @export
#' @examples
valores_aux <- function(df){
  precio <- mean(df$precio, na.rm=TRUE)
  p_m2 <- mean(df$p_m2,na.rm=TRUE)
  m2 <- mean(df$metros, na.rm=TRUE)
  ilde <- mean(df$n_banos + df$n_dorms, na.rm=TRUE)

  return(c(precio,p_m2,m2,ilde))
}