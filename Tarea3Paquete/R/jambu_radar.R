#
# El paquete que propongo sirve para visualizar dos graficos importantes en el manejo de cluster: el codigo de Jambo y el grafico de araña.
#
# EJEMPOLO DE USO
# data("iris")
# datos <- iris
# jambu(datos[,-5], color = "red")
# modelo <- kmeans(datos[,-5], centers = 4, nstart=25)
# radar(modelo,
#       etiquetas = c("Grupo 1","Grupo 2","Grupo 3", "Grupo 4"),
#       colores = c("green","blue","red","yellow"),
#       titulo = "Comparación de clústeres")


library(fmsb) # RADARCHART
library(testthat) # TEST


#' El titulo de la funcion.
#'
#' @param datos Un Dataframe numerico.
#' @param color Un color.
#' @examples
#' jambu(datos, color = "red")
#' @export

jambu <- function(datos, color){
  context("Validación de los parametros")
  expect_is(datos, "data.frame")
  #expect_true(length(color) = 1)

  InerciaIC<-rep(0,30)

  for(k in 1:30) {
    grupos <- kmeans(datos, centers=k, nstart=25)
    InerciaIC[k] <- grupos$tot.withinss
  }

  plot(InerciaIC, col=color, type="b")
}



#' El titulo de la funcion.
#'
#' @param modelo Un modelo de Cluster.
#' @param etiquetas Un arreglo de etiquetas.
#' @param colores Un arreglo de colores.
#' @param titulo Un texto.
#' @examples
#' radar(modelo, etiquetas = c("Grupo 1","Grupo 2","Grupo 3", "Grupo 4"), colores = c("green","blue","red","yellow"), titulo = "Comparación de clústeres")
#' @export

radar <- function(modelo, etiquetas, colores, titulo){

  context("Validación de los parametros")
  expect_true(length(etiquetas) >= 1)
  expect_true(length(colores) >= 1)

  centros <- as.data.frame(modelo$centers)

  maximos <- apply(centros, 2, max)
  minimos <- apply(centros, 2, min)
  centros <- rbind(minimos, centros)
  centros <- rbind(maximos, centros)

  fmsb::radarchart(centros,
                   maxmin=TRUE,
                   axistype=4,
                   axislabcol="slategray4",
                   centerzero=FALSE,
                   seg=8,
                   cglcol="gray67",
                   pcol=colores,
                   plty=1,
                   plwd=2,
                   title=titulo)

  legenda <-legend(1.5,
                   1,
                   legend=etiquetas,
                   seg.len=-1.4,
                   #title="Clústeres",
                   pch=21,
                   bty="n",
                   lwd=3,
                   y.intersp=1,
                   horiz=FALSE,
                   col=colores)
}
