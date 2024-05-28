###LECTURA DE DATOS

setwd("C:/Users/Usuario/Desktop/FIDIS/Enfermería_final")
getwd()
library(readxl)

control<-read_excel("Tabla Evaluadora 2022.xlsx", sheet = "Control")
control<-control[-c(11,12), ]  #Eliminamos las filas en las que han escrito las medias, solo queremos los datos sin tratar

PDF<-read_excel("Tabla Evaluadora 2022.xlsx", sheet = "PDF")
PDF<-PDF[-c(12,13,14), ]

video<-read_excel("Tabla Evaluadora 2022.xlsx", sheet = "VIDEO")
video<-video[-c(8,9), ]

video_PDF<-read_excel("Tabla Evaluadora 2022.xlsx", sheet = "VIDEO+PDF")
video_PDF<-video_PDF[-c(10,11), ]


###PRELIMINARES

grupos<-c("control","PDF","video","video_PDF") #guardamos los grupos
variables<-colnames(control) #guardamos las variables, que nos será útil en los bucles
variables<-variables[-c(1)] #quitamos la primera que no es una variable "real" (son los nombres de los grupos)

variables_num<-variables[-c(2,12,13,14,15,16,17,18)] #guardamos aquí solo con las variables numéricas

#como R ha leído las variables como caracteres, las pasamos a numéricas para que no den errores después
for (i in 1:length(variables_num)) {
  control[[variables_num[i]]]<-as.numeric(control[[variables_num[i]]])
}

for (i in 1:length(variables_num)) {
  PDF[[variables_num[i]]]<-as.numeric(PDF[[variables_num[i]]])
}

for (i in 1:length(variables_num)) {
  video[[variables_num[i]]]<-as.numeric(video[[variables_num[i]]])
}

for (i in 1:length(variables_num)) {
  video_PDF[[variables_num[i]]]<-as.numeric(video_PDF[[variables_num[i]]])
}

#creamos una nueva tabla (datos) con todos los grupos juntos, y una nueva variable (grupo) para distinguirlos
control<-cbind(control, grupo= "control")
PDF<-cbind(PDF, grupo= "PDF")
video<-cbind(video, grupo= "video")
video_PDF<-cbind(video_PDF, grupo= "video_PDF")
datos<-rbind(control,PDF,video,video_PDF)



###PARTE 1: ¿Hay el mismo número de NAs en los diferentes grupos?

nas<-function(x){
  if (is.na(x)==TRUE) {
    1
  } else {
    0
  }
}

#con la función nas escribimos 1 donde hay un NA y 0 donde no lo hay, lo hacemos para todas las variables
datos_nas<-datos
for (i in 1:length(variables)) {
datos_nas[ ,i+1]<-sapply(datos[[variables[i]]],FUN = nas,simplify=TRUE)
}

pvalchi<-cbind(variables,p_valor=rep(1,length(variables))) #aquí guardaremos los p-valores del test

for (i in 1:length(variables)) {
  if (sum(datos_nas[[variables[i]]])>0){
    pvalchi[i,2]<-chisq.test(datos_nas$grupo,datos_nas[[variables[i]]])$p.value
  }
}


###PARTE 2: ¿Las medias de las variables son iguales?

#en esta tabla escribiremos los resultados 
tabla<-matrix(nrow=length(variables_num),ncol=9)
tabla[ ,1]<-variables_num
colnames(tabla)<-c('Variables','Anova','Kruskal','Wilcox control - PDF','Wilcox control - video','Wilcox control - video_PDF','Wilcox PDF - video','Wilcox PDF - video_PDF','Wilcox video - video_PDF')

shap<-matrix(nrow=4,ncol=length(variables_num),) #lo usamos para ver los p.val del shapiro.test en cada iteracion
colnames(shap)<-variables_num
rownames(shap)<-grupos
for (i in 1:length(variables_num)) {
  shapiro<-rep(1,4) #al poner un 1 por defecto, para las varibles que no tienen suficientes muestras para hacer el shapiro.test se acepta la H0 (se toman como distribuciones normales)
  if (sum(datos_nas[datos$grupo=="control", ][[variables_num[i]]]) < nrow(control)-2){
    shapiro[1]=shapiro.test(control[[variables_num[i]]])$p.value
  }
  if (sum(datos_nas[datos$grupo=="PDF", ][[variables_num[i]]]) < nrow(PDF)-2){
    shapiro[2]=shapiro.test(PDF[[variables_num[i]]])$p.value
  }
  if (sum(datos_nas[datos$grupo=="video", ][[variables_num[i]]]) < nrow(video)-2){
    shapiro[3]=shapiro.test(video[[variables_num[i]]])$p.value
  }
  if (sum(datos_nas[datos$grupo=="video_PDF", ][[variables_num[i]]]) < nrow(video_PDF)-2){
    shapiro[4]=shapiro.test(video_PDF[[variables_num[i]]])$p.value
  }
  
  shap[ ,i]<-shapiro
  
  if(min(shapiro)>0.05){  #en shapiro.test, H0:Distribución normal
    modelo<-lm(datos[[variables_num[i]]] ~ grupo, data = datos)
    tabla[i,2]<-anova(modelo)$"Pr(>F)"[1]
    if (tabla[i,2]<0.05) { #si en el anova aceptamos Ha, estudiamos con Tukey qué medias concretamente son distintas
      Tukey<-matrix(nrow=6,ncol=2)
      Tukey[ ,1]<-rownames(TukeyHSD(aov(datos$`Doble antiagreg DICO` ~ grupo, data = datos))$grupo)
      Tukey[ ,2]<-TukeyHSD(aov(datos$`Doble antiagreg DICO` ~ grupo, data = datos))$grupo[ ,4]
      colnames(Tukey)<-c("grupos","p-valores (ajustados)")
      assign(paste("Tukey",variables_num[i],sep="_"),Tukey)
    }
  }
  else{
    tabla[i,3]<-kruskal.test(list(control[[variables_num[i]]],PDF[[variables_num[i]]],video[[variables_num[i]]],video_PDF[[variables_num[i]]]))$p.value
    
    wilcox<-rep(0,6)
    wilcox[1]<-wilcox.test(control[[variables_num[i]]],PDF[[variables_num[i]]])$p.value
    wilcox[2]<-wilcox.test(control[[variables_num[i]]],video[[variables_num[i]]])$p.value
    wilcox[3]<-wilcox.test(control[[variables_num[i]]],video_PDF[[variables_num[i]]])$p.value
    wilcox[4]<-wilcox.test(PDF[[variables_num[i]]],video[[variables_num[i]]])$p.value
    wilcox[5]<-wilcox.test(PDF[[variables_num[i]]],video_PDF[[variables_num[i]]])$p.value
    wilcox[6]<-wilcox.test(video[[variables_num[i]]],video_PDF[[variables_num[i]]])$p.value
    for (j in 1:6){
      if (wilcox[j]<0.05){
        tabla[i,3+j]<-wilcox[j]
      }
    }
  }
}


# RESUMEN:
# En pvalchi están guardados los p-valores del test chisq para ver si el número de NAs en cada variable es
# independiente del grupo al que pertenecen. Los unos significan que no hay ningún NA en esa variable y por
# tanto no nos interesan en el test. En shap están guardados los p-valores del test de Shapiro, se puede
# comprobar que los resultados son los mismos a los que llegó Álex. En tabla están recogidos los p-valores
# de los distintos test (anova, kruskal y wilcox) para comprobar si las medias de las variables a través
# de los grupos son iguales o distintas. Anova se usó en las variables que probaron seguir una 
# distribución normal y los otros dos en los casos contrarios. Las celdas en blanco de la columna de anova
# singifican que ha fallado la normalidad y se han hecho los otros test. En las celdas correspondientes
# al test de wilcox sólo se han cubierto aquellas en las que el p-valor es inferior a 0.05 para no tener 
# que buscar entre tantos números cuáles nos interesan. En Tukey_"variable" están recogidos los 
# p-valores del test Tukey, usado para averiguar concretamente qué pares de grupos tienen medias 
# distintas en el caso en el que en el anova aceptemos Ha.


##Cálculo alterativo: sustituir los NAs por 1200s
##En caso de usarse, introducir el siguiente código al inicio de la Parte 2

#Ojo que estamos teniendo también en cuenta los NAs de las variables no numéricas, si esto da problemas tenemos que corregirlo usando variables_num
for (i in 1:length(variables)) {
  for (j in 1:nrow(control)) {
    if (is.na(control[j,i+1])==TRUE) {
      control[j,i+1]<-1200
    }
  }
}

for (i in 1:length(variables)) {
  for (j in 1:nrow(PDF)) {
    if (is.na(PDF[j,i+1])==TRUE) {
      PDF[j,i+1]<-1200
    }
  }
}

for (i in 1:length(variables)) {
  for (j in 1:nrow(video)) {
    if (is.na(video[j,i+1])==TRUE) {
      video[j,i+1]<-1200
    }
  }
}

for (i in 1:length(variables)) {
  for (j in 1:nrow(video_PDF)) {
    if (is.na(video_PDF[j,i+1])==TRUE) {
      video_PDF[j,i+1]<-1200
    }
  }
}
