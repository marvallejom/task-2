#==============================================================================#
# Autores: Maria Vallejo, Andrea Cortes, Miguel Castillo
# # Fecha elaboracion:18 de abril de 2021
# Ultima modificacion: 19 de abril de 2021
# Version de R: 4.0.3
#==============================================================================#
#Se cargan las librerías
pacman::p_load(tidyverse,viridis,forcats,gapminder,readxl,data.table)
#==============================================================================#
#1. Loops
#Se carga la base de datos
lista_df=readRDS(file = 'data/input/lista.rds')

# Verificar visualmente los datos: primer, décimo y vigésimo dataframe
df1=lista_df[[1]]  
df10=lista_df[[10]] 
df20=lista_df[[20]]

#Vector que almacena el tipo de delito
tipo_delito=c()

#Limpiar la base de datos, almacenar el tipo de delito y cambiar el nombre de las columnas
nombrecolumnas=tolower(lista_df[[10]][8,])
numero_filas=0
for (i in 1:length(lista_df)){
  tipo_delito=c(tipo_delito,tolower(lista_df[[i]][6,1]))
  lista_df[[i]]=subset(lista_df[[i]],is.na(...2)==F)
  colnames(lista_df[[i]])=nombrecolumnas
  lista_df[[i]]=lista_df[[i]][-1,]
  numero_filas=numero_filas+nrow(lista_df[[i]])
}
View(tipo_delito)
numero_filas

#Dataframe con los elementos de la lista
df=rbindlist(lista_df,fill=TRUE)

