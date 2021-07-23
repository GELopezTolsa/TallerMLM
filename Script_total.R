rm(list=ls()) #sirve para borrar el ambiente

#Llamamos los 3 paquetes que descargamos anteriormente
library(psych)
library(lme4)
library(lmerTest)

#Especificamos el directorio con el que vamos a estar trabajando (se puede hacer
#desde el menú session ó poniéndola entre paréntesis con el comando setwd())

setwd("~/Dropbox/aNew Statistics/Tutorial BLMM para AEC/Taller")

#Creamos una nueva variable para nuestros datos.

datos2 <- read.csv("Data_Total.csv")

#Empezamos con un modelo nulo, en el que la VD solo dependa de los sujetos.
#Nòtese que utilizamos un nombre diferente "mo0" en lugar de "m0", esto es sólo
#para no confundirnos con el script anterior, pero si vamos a trabajar con un
#solo script no es necesario.

mo0 = lmer(PPR ~ (1|Sujeto),     
          data = datos2)   

summary(mo0)

#En el siguiente modelo añadimos una de nuestras variables independientes, en
#este caso el grupo (A vs B); y mantenemos el intercepto aleatorio por sujetos.

mo1 = lmer(PPR ~ Grupo + (1|Sujeto),
           data = datos2)

summary(mo1)

#Luego creamos un modelo que sume el efecto de nuestras dos VIs. Mantenemos el
#intercepto aleatorio por sujetos.

mo2 = lmer(PPR ~ IF + Grupo + (1|Sujeto),
           data = datos2)

summary(mo2)

#Aquí vamos a mover la variable "IF" y la vamos a utilizar para hacer pendientes
#aleatorias para cada grupo en funciòn del IF.

mo3 = lmer(PPR ~ Grupo + (1|Sujeto) + (Grupo|IF),
           data = datos2)

summary(mo3)

#Llamamos a los coeficientes, solo por curiosidad, para ver còmo se ve una
#pendiente (del grupo) distinta para cada IF.

coef(mo3)

#Hacemos la anova, incluyendo a mo3, aunque sabemos que no lo usaremos, ya que
#es confiable.

anova(mo0,mo1,mo2,mo3)

summary(mo2)
#Listo, podemos redactar estos resultados.
