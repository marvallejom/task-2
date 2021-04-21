#==============================================================================#
# Autores: Maria Vallejo, Andrea Cortes, Miguel Castillo
# # Fecha elaboracion:18 de abril de 2021
# Ultima modificacion: 21 de abril de 2021
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
nombrecolumnas=tolower(lista_df[[10]][8,]) #Solamente serían los nombres de las columnas?
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
inicio=length(nombrecolumnas)+1
fin=ncol(df)
#Eliminación de columnas nulas
for (i in inicio:fin){
  df=df[,-21]
}
#==============================================================================
#2. Familia apply
#Tabla de frecuencia para cada una de las variables
lapply(df, function(x)table(x)) #Con esto es suficiente?

#==============================================================================
#3. Lapply
#Función que convierte en minúsculas los  elementos de un vector de caracteres
converter_tolower=function(vector_caracteres){
  es_caracter=TRUE
  for (i in 1:length(vector_caracteres)){
    if(is.character(vector_caracteres[i])==FALSE){
      es_caracter=FALSE
    }
  }
  if(es_caracter==TRUE){
    return(tolower(vector_caracteres))
  }
  else{
    return(vector_caracteres) #Se puede retornar el mismo vector? o hay que enviar mostrar algún mensaje?
  }                   
}

#Aplicar la función en cada una de las columnas del dataframe
lapply(df, function(x)converter_tolower(x))
