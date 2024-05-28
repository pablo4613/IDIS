rm(list = ls()) #borrar todos los datos en la sesión

setwd("C:/Users/Usuario/Desktop/FIDIS/Cardio_final")
getwd()
library(readxl)

datos1<-read_excel('practicas_pablo.xls', sheet= 'Características basales - Carac') #Leer priemra hoja
datos2<-read_excel('practicas_pablo.xls', sheet= 'Post-op inmediato _ Ingreso - P') #Leer segunda

datos<-cbind(datos1[1:dim(datos2)[1], ],datos2) #unimos las dos hojas

datos<-datos[datos$`FA post-op`<2 & is.na(datos$`FA post-op`)==FALSE, ] #nos quedamos con los datos que son útiles


##Calculamos correlaciones

Marcadores_interes<-c('EuroSCORE II','Sexo (0 hombre, 1 mujer)','Diabetes','CHADS2-VASc','Hb','Leucocitos','Glucosa','Creatinina','NTproBNP','FEVI','Área aurícula izquierda','Hb24','Creatinina24','Lactato ingreso','Lactato 6h','Lactato 12h','Lactato 24h','SvO2 24h')
correlaciones<-rbind(Marcadores_interes,rep(0,length(Marcadores_interes)),rep(0,length(Marcadores_interes)))
rownames(correlaciones)<-c('Marcadores','Coeficiente de correclación','p-valor')

for (i in 1:length(Marcadores_interes))
{
  correlaciones[2,i]=cor(datos$`FA post-op`,datos[[Marcadores_interes[i]]],use='complete.obs')
  correlaciones[3,i]=cor.test(datos$`FA post-op`,datos[[Marcadores_interes[i]]],method='pearson')$p.value
}

correlaciones<-t(correlaciones)

#orden por coef de correlación decreciente
orden_corr<-correlaciones[order(correlaciones[ ,2],decreasing=TRUE), ]

#orden por p-valor creciente
orden_pval<-correlaciones[order(correlaciones[ ,3],decreasing=FALSE), ]


##Pruebas con posibles scores

#nos quedamos con los datos de los pacientes que tienen todos los marcadores interesantes
datos<-datos[is.na(datos$`Lactato 6h`)==FALSE & is.na(datos$Hb)==FALSE & is.na(datos$`Sexo (0 hombre, 1 mujer)`)==FALSE & is.na(datos$`Área aurícula izquierda`)==FALSE & is.na(datos$NTproBNP)==FALSE, ]

#función lactato 6h
cuant_lac6h<-quantile(datos$`Lactato 6h`, prob=seq(0,1,1/5))

lac6h<-function(x){
  if (x<cuant_lac6h[2]) {
    0
  } else {
    if (x<cuant_lac6h[3]) {
      2
    } else {
      if (x<cuant_lac6h[4]) {
        -4
      } else {
        if (x<cuant_lac6h[5]) {
          -3
        } else {
          9
        }
      }}}
}

#función Hb
cuant_Hb<-quantile(datos$Hb, prob=seq(0,1,1/3))

Hb<-function(x){
  if (x<cuant_Hb[2]) {
    1
  } else {
    if (x<cuant_Hb[3]) {
      -2
    } else {
      1
    }
  }
}


#función sexo
sexo<-function(x){
  if (x==0) {
    0
  } else { 
    5
  }
}

#función Área auricular izquierda (AAI)
cuant_AAI<-quantile(datos$`Área aurícula izquierda`, prob=seq(0,1,1/5))

AAI<-function(x){
  if (x<cuant_AAI[2]) {
    -1
  } else {
    if (x<cuant_AAI[3]) {
      2
    } else {
      if (x<cuant_AAI[4]) {
        -2
      } else {
        if(x<cuant_AAI[5]) {
          0
        } else {
          3
        }
      }
    }
  }
}


#función NTproBNP
cuant_NT<-quantile(datos$NTproBNP, prob=seq(0,1,1/5))

NT<-function(x){
  if (x<cuant_NT[2]) {
    1
  } else { 
    if (x<cuant_NT[3]) {
      -2
    } else {
      if (x<cuant_NT[4]) {
        -1
      } else {
        if (x<cuant_NT[5]) {
          1
        } else {
          3
        }
      }
    }
  }
}


#posible score
score<-sapply(datos$`Lactato 6h`,FUN = lac6h,simplify = TRUE)+sapply(datos$Hb,FUN = Hb,simplify = TRUE)+sapply(datos$`Sexo (0 hombre, 1 mujer)`,FUN = sexo,simplify = TRUE)+sapply(datos$`Área aurícula izquierda`,FUN = AAI,simplify = TRUE)+sapply(datos$NTproBNP,FUN = NT,simplify = TRUE)
score<-score+rep(8,length(score)) #para eliminar valores negativos del score, alternativamente sumamos 8 a todos los valores en todas las posibilidades de cada variable


cuant_score<-quantile(score, prob=seq(0,1,1/5))

fscore<-function(x){
  if (x<cuant_score[2]) {
    1
  } else {
    if (x<cuant_score[3]) {
      1
    } else {
      if (x<cuant_score[4]) {
        2
      } else {
        if (x<cuant_score[5]) {
          3
        } else {
          if (x< cuant_score[6]) {
            4
          } else {
            5
          }
        }
      }}}
}

score<-sapply(score,FUN = fscore,simplify = TRUE)
