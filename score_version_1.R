rm(list = ls()) #borrar todos los datos en la sesión

setwd("C:/Users/Usuario/Desktop/FIDIS/Cardio_final")
getwd()
library(readxl)

datos1<-read_excel('practicas_pablo.xls', sheet= 'Características basales - Carac') #Leer priemra hoja
datos2<-read_excel('practicas_pablo.xls', sheet= 'Post-op inmediato _ Ingreso - P') #Leer segunda

datos<-cbind(datos1[1:dim(datos2)[1], ],datos2) #unimos las dos hojas

datos<-datos[datos$`FA post-op`<2 & is.na(datos$`FA post-op`)==FALSE, ] #nos quedamos con los datos que son útiles


##Pruebas con posibles scores

#nos quedamos con los datos de los pacientes que tienen todos los marcadores interesantes
datos<-datos[is.na(datos$Fibrinógeno)==FALSE & is.na(datos$`Tiempo CEC (min)`)==FALSE & is.na(datos$`Días de aminas`)==FALSE & is.na(datos$Hb)==FALSE & is.na(datos$Levosimendan)==FALSE & is.na(datos$Dobuta)==FALSE, ]

#función Días de aminas (DA)
cuant_DA<-quantile(datos$`Días de aminas`, prob=seq(0,1,1/5))

DA<-function(x){
  if (x<cuant_DA[2]) {
    0  #no cambia la correlación
  } else {
    if (x<cuant_DA[3]) {
      0  #no cambia la correlación
    } else {
      if (x<cuant_DA[4]) {
        0
      } else {
        if (x<cuant_DA[5]) {
          3
        } else {
          9
        }
      }
    }
  }
}

#función Tiempo CEC
cuant_tiempo<-quantile(datos$`Tiempo CEC (min)`, prob=seq(0,1,1/5))

tiempo<-function(x){
  if (x<cuant_tiempo[2]) {
    3
  } else {
    if (x<cuant_tiempo[3]) {
      4
    } else {
      if (x<cuant_tiempo[4]) {
        4
      } else {
        if (x<cuant_tiempo[5]) {
          7
        } else {
          11
        }
      }
    }
  }
}

#función Fibrinógeno
cuant_fib<-quantile(datos$Fibrinógeno, prob=seq(0,1,1/5))

fib<-function(x){
  if (x<cuant_fib[2]) {
    -1
  } else {
    if (x<cuant_fib[3]) {
      -2
    } else {
      if (x<cuant_fib[4]) {
        5
      } else {
        if (x<cuant_fib[5]) {
          0  #este número no cambia la correlación
        } else {
          7
        }
      }
    }
  }
}


#función Levosimendan
lev<-function(x){
  if (x==0) {
    2
  } else {
    0
  }
}

#función Dobuta
cuant_dob<-quantile(datos$Dobuta, prob=seq(0,1,1/3))

dob<-function(x){
  if (x==0) {
    4
  } else {
    0
  }
}

#posible score
score<-sapply(datos$Fibrinógeno,FUN = fib,simplify = TRUE)+sapply(datos$`Tiempo CEC (min)`,FUN = tiempo,simplify = TRUE)+sapply(datos$`Días de aminas`,FUN = DA,simplify = TRUE)+sapply(datos$Levosimendan,FUN = lev,simplify = TRUE)+sapply(datos$Dobuta,FUN = dob,simplify = TRUE)
#score<-score+rep(8,length(score)) #para eliminar valores negativos del score, alternativamente sumamos 3 a todos los valores en todas las posibilidades de cada variable

cuant_score<-quantile(score, prob=seq(0,1,1/7))

fscore<-function(x){
  if (x<cuant_score[2]) {
    1
  } else {
    if (x<cuant_score[3]) {
      2
    } else {
      if (x<cuant_score[4]) {
        3
      } else {
        if (x<cuant_score[5]) {
          4
        } else {
          if (x< cuant_score[6]) {
            5
          } else {
            if (x<cuant_score[7]) {
              6
            } else {
              7
            }
          }
        }
      }}}
}

score<-sapply(score,FUN = fscore,simplify = TRUE)
