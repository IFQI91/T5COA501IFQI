
##Tarea 5
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez



#1. Realice lo siguiente (previamente investigue y ejecute completamente el script_3 visto en clase)

#a) Cree un script T5iniciales.R con R-Studio dentro de un archivo Project en R

#b) Ejecute la función data() desde la plataforma R-STUDIO e importe los conjuntos de datos
#trees y npk de R; luego cree dos nuevos objetos data.frame datos.arboles y datos.npk;
#renombre en español, las columnas de ambos objetos.

data("trees")
data("npk")

datos.arboles <- trees
datos.npk <- npk

names(datos.arboles) <- c("edad","altura","volumen")
names(datos.npk) <- c("bloque","N","P","K","rendimiento")
attach(datos.arboles)
attach(datos.npk)

#c) Para cada conjunto de datos, realice un análisis exploratorio de las variables respuesta
#(boxplots, histogramas, frecuencias, summary, gráficas de dispersión, etc) considere los
#conjuntos de datos completos.

#Base datos.arboles
hist(altura)
hist(edad)
hist(volumen)

summary(datos.arboles)

pairs(datos.arboles)



#Base datos.npk

summary(datos.npk)
table(bloque)
table(N)
table(P)
table(K)

boxplot(rendimiento~bloque)
media <- tapply(rendimiento,bloque,mean)
points(media,pch=16,col="black")

boxplot(rendimiento~N)
media2 <- tapply(rendimiento,N,mean)
points(media2,pch=16,col="black")

boxplot(rendimiento~P)
media3 <- tapply(rendimiento,P,mean)
points(media3,pch=16,col="black")

boxplot(rendimiento~K)
media4 <- tapply(rendimiento,K,mean)
points(media4,pch=16,col="black")

hist(datos.npk$rendimiento)

pairs(datos.npk)

#ANOVA
library(agricolae)
av <- aov(rendimiento~N+P+K+bloque,data=datos.npk)
summary(av)

#d) Para el conjunto de datos datos.npk

#i.calcule las medias y desviaciones estándar por bloque.

tapply(rendimiento,bloque,mean)
tapply(rendimiento,bloque,sd)

#ii.cree dos subconjuntos de datos, uno que incluya los bloques pares y otro que incluya
#los bloques impares.

#indicadores
p <- as.numeric(bloque)%% 2 == 0
i <- as.numeric(bloque)%% 2 == 1

p_bloques <- subset(datos.npk,p==TRUE)

i_bloques <- subset(datos.npk,i==TRUE)


#e) Para el conjunto de datos datos.arboles

#iii.grafique por pares de variables

pairs(datos.arboles)

#iv.efectúe regresiones lineales entre el volumen (variable dependiente) y (altura,
#perímetro) variables independientes.

rl1 <- lm(volumen~altura+edad,data=datos.arboles)
summary(rl1)

rl2 <- lm(volumen~altura,data=datos.arboles)
summary(rl2)

rl3 <- lm(volumen~edad,data=datos.arboles)
summary(rl3)

AIC(rl1,rl2,rl3)



#v. grafique los valores predichos versus valores observados

plot(rl1$fitted.values,datos.arboles$volumen)
abline(lm(rl1$fitted.values~datos.arboles$volumen),col="red")

#f) Exporte los conjuntos de datos, datos.arboles y datos.npk a archivos .csv

write.csv(datos.arboles,"datos.arboles.csv")
write.csv(datos.npk,"datos.npk.csv")

#g) Exporte los valores predichos, observados y los residuales da cada modelo a un archivo .csv.

pred1 <- rl1$fitted.values
obs1 <- datos.arboles$volumen
res1 <- rl1$residuals

mod1 <- cbind(pred1,obs1,res1); write.csv(mod1,"mod1.csv")

pred2 <- rl2$fitted.values
obs2 <- datos.arboles$volumen
res2 <- rl2$residuals

mod2 <- cbind(pred2,obs2,res2); write.csv(mod2,"mod2.csv")

pred3 <- rl3$fitted.values
obs3 <- datos.arboles$volumen
res3 <- rl3$residuals

mod3 <- cbind(pred3,obs3,res3); write.csv(mod3,"mod3.csv")


#h)Genere 50 valores de pesos de borregos con una media de 25 kg y sd de 6 kg (asuma
#normalidad). Genere 50 datos de diámetro de cintura de borregos en cm con media 48 cm
#y sd 7cm (asuma normalidad). Grafique los datos peso (variable respuesta) vs. diámetro
#(variable predictora). Efectúe una regresión lineal, analice y guarde sus resultados en
#archivos en forma ordenada y comente sus resultados.

set.seed(123)
pesos_borregos <- rnorm(50,mean=25,sd=6)
summary(pesos_borregos)
sd(pesos_borregos)

diametro_borregos <- rnorm(50,mean=48,sd=7)
summary(diametro_borregos)
sd(diametro_borregos)

rlb <- lm(pesos_borregos~diametro_borregos)
summary(rlb)


#diagrama de dispersion de peso de borregos vs diametro de borregos
plot(pesos_borregos,diametro_borregos)

#comentarios: la variable predictora diametro de borregos no es buena predictora
#del peso de borregos, al menos en este ejercicio hipotetico con valores aleatorios normalmente
#distribuidos.

#i) Cree 5 pares de objetos tmin1, tmax1, tmin2, tmax2, …, tmin5, tmax5 de longitud 5. Donde
#tmin son temperaturas mínimas diarias y tmax son temperaturas máximas diarias. Luego,
#cree una matriz temperatura (dim 5 x 10) con los objetos creados. Utilice la función
#tpromedio vista en clase para calcular los promedios de temperatura tp1, tp2, …,tp5, cree
#una matriz para guardar estos promedios. Defina un ciclo for para hacer los cálculos en forma
#cíclica.

set.seed(123)
tmin1 <- sample(0:10,5,replace=T)
tmax1 <- sample(25:32,5,replace = T)

tmin2 <- sample(0:10,5,replace=T)
tmax2 <- sample(25:32,5,replace = T)

tmin3 <- sample(0:10,5,replace=T)
tmax3 <- sample(25:32,5,replace = T)

tmin4 <- sample(0:10,5,replace=T)
tmax4 <- sample(25:32,5,replace = T)

tmin5 <- sample(0:10,5,replace=T)
tmax5 <- sample(25:32,5,replace = T)

temperatura <- cbind(tmin1,tmax1,tmin2,tmax2,tmin3,tmax3,tmin4,tmax4,tmin5,tmax5)
dim(temperatura)

temperatura_r <- rbind(temperatura[,c(1,2)],temperatura[,c(3,4)],
                temperatura[,c(5,6)],temperatura[,c(7,8)],temperatura[,c(9,10)])

colnames(temperatura_r) <- c("tmin","tmax")

t.promedio <- function(tmin, tmax) 
{ 
  ( tmax + tmin ) / 2
}

tp1 <- t.promedio(temperatura[,1],temperatura[,2])
tp2 <- t.promedio(temperatura[,3],temperatura[,4])
tp3 <- t.promedio(temperatura[,5],temperatura[,6])
tp4 <- t.promedio(temperatura[,7],temperatura[,8])
tp5 <- t.promedio(temperatura[,9],temperatura[,10])

tp1;tp2;tp3;tp4;tp5

tp <- cbind(tp1,tp2,tp3,tp4,tp5)


#Opcion 1 funcion por filas con matrix temperatura_r y print como array
 for ( row in 1:nrow(temperatura_r)) { 
   
   tmin <- temperatura_r[row, "tmin"]
   tmax  <- temperatura_r[row, "tmax"]
   
    t.prom <- (tmin+tmax)/2
    
    #print(paste("Promedio de",
    #t.prom,"°C"))
    
    print(array(t.prom))
    
 }



# Opcion 2 Aplicar funcion ordenada en filas con matrix temperatura y print como array
for ( row in 1:nrow(temperatura)) { 
    
  tmin <- temperatura[row,seq_len(ncol(temperatura))%%2==1]#c(1,3,5,7,9)
  tmax  <- temperatura[row,seq_len(ncol(temperatura))%%2==0]#2,4,6,8,10
  
  t.prom <- (tmin+tmax)/2
  
  #print(paste("Promedio de",
  #t.prom,"°C","en la fila",row))
  print(array(t.prom))
}


#Ordenar de forma vertical (tmin1,tmax1,...) de filas a columna
for(row in 1:nrow(temperatura)) {
  for(col in 1:ncol(temperatura)) {
    print(array(temperatura[row, col]))
  }
}




#j) Exporte a un archivo .RData el área de trabajo completa.

save.image("T5IFQI.RData")


#k)Comente sus resultados en general.

#R es un lenguaje de programación que nos permite hacer analisis estadisticos,
#así como generar nuestras propias funciones y bucles con  estructuras de control
#(i.e. for, while,if else,break, next,repeat) utiles en procesos iterativos.                                                                            
                                                                            