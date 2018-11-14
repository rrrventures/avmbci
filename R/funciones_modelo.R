#' dist_imap
#'
#' Encuentra las distancias a los comparables (propiedades similares) de una propiedad en particular a la base de testigos
#' @param registro propiedad para la cual se buscan testigos
#' @param varmod nombres de variables que el modelo necesita
#' @param vars_onehot nombres de variables onehot
#' @param modelo nombres de variables onehot
#' @keywords distancias levershine propiedades testigos
#' @export
#' @examples

predecir <- function(registro, varmod, vars_onehot, modelo){
  
  #Aqum hay que poner los cambios de formatos y si hay algzn filtro de algo que no se tenga que evaluar
  registro$cod_comuna_habits <- as.character(registro$cod_comuna_habits)
  registro$cod_region_habits <- as.character(registro$cod_region_habits)
  registro$grupo_comuna <- as.character(registro$grupo_comuna)
  registro[,c('V5_b', 'V6_b', 'V5_c', 'V6_c')] <- NA
  factorFeatures <- c("cod_comuna_habits", "cod_region_habits", "sol.sol_tip_bie", "grupo_comuna", 'V5_b', 'V6_b', 'V5_c', 'V6_c')
  
  transform2Dummy <- function(variable, prefix = NULL) {
    # Transforma una variable categorica a una matriz de 1's y 0's
    # Didac, 20160909
    # variable: vector categorico a transformar a dummy
    # prefix: si queremos anyadir un prefijo a los colnames de la variable (formato character)
    
    # Prevenim per quan hi ha NA's
    variable <- as.character(variable)
    variable[is.na(variable)] <- "NA"
    
    result <- data.frame(sapply(unique(variable), function(x) {
      ifelse(variable == x, 1, 0)
    }))
    colnames(result) <- unique(as.character(variable))
    if (ncol(result) > 1) {
      result <- result[, order(colnames(result))]
    }
    colnames(result) <- gsub(" ", "", colnames(result))
    if (!is.null(prefix)) {
      colnames(result) <- paste(prefix, colnames(result), sep = "")
    }
    return(result)
  }

 
  oneHotData <- registro[, factorFeatures]

  #Como yo haria esto
  nombres <- names(oneHotData)
  oneHotData <- lapply(1:length(nombres), function(x) transform2Dummy(oneHotData[,x],nombres[x]))
  registro <- cbind.data.frame(registro, oneHotData)

  variables_faltantes <- vars_onehot
  
  #Las variables que no se crean en el transform2Dummy porque no existen todas las categorias, se crean poniendo un 0
  variables <- variables_faltantes[!variables_faltantes %in% colnames(registro)]
  for (var in variables) {
    registro[, var] <- 0
  }


  #Eliminamos las categoricas
  var.mod <- varmod
  registro <- registro %>% select(-one_of(factorFeatures))
  registro[,var.mod[!var.mod %in% names(registro)]] <- NA 
  matrix_train <- xgboost::xgb.DMatrix(as.matrix(registro[, var.mod]), missing=NA)

  #prediccion
  PRECIO_ESTIMADO <- predict(modelo, matrix_train, ntreelimit = 800)

  return(PRECIO_ESTIMADO)

}