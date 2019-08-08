# Adrian Botello Montoya 
# 06/08/2019
# importar datos de vivero ------------------------------------------------

vivero <- read.csv("C:/Maestria/vivero.csv", header = T)
head(vivero)
summary(vivero)


# prueba de t una muestra  ------------------------------------------------
par(mfrow=c(1,1))
boxplot(vivero$IE)
t.test(vivero$IE, mu = 0.85)
# La media observada no es diferente estadisticamente 
# ya que el valor de p es mayor que el valor
# el alfa establecido de (0.05), ademas de la media teoretica se
# encuentra dentro del rango de los valores de intervalos de confianza.

t.test(vivero$IE, mu = 0.90)
# La media observada es diferente a la media teorica, por lo que eceptamos 
# la hi. 1 es valor de p (0.01)es menor que el alfa establecido (0.05)



# Prueba de muestras independientes  --------------------------------------

boxplot(vivero$IE ~ vivero$Tratamiento, Col= "green", xlab = "tratamiento",
        ylab = "IE")

shapiro.test(vivero$IE)

var.test(vivero$IE ~ vivero$Tratamiento)
# las varianza de ambos tratamientos son iguales segun el el valor P
# el cual se obtuvo mediante una prueba de vairanza 

t.test(vivero$IE ~ vivero$Tratamiento, var.equal =T)
# existe una diferencia significativa entre el indice de esveltes 
#de las plantulas fertilizadas 
# el valor de la prueba p comprueva nuestra hipotesis de que el fertilizante "power", mejora el IE
t.test(vivero$IE ~ vivero$Tratamiento)




# Pruebas de t muestras dependientes  -------------------------------------

t.test(vivero$IE ~ vivero$Tratamiento, paired = T)


# inventario de produccion ------------------------------------------------

produccion <- read.csv("C:/Maestria/produccion.csv")
summary(produccion)

t.test(produccion$Germ ~ produccion$Tiempo, paired = T)
boxplot(produccion$Germ)


# restriccion  ------------------------------------------------------------

tapply(produccion$Germ, produccion$Tiempo, mean)

boxplot(produccion$Germ, produccion$Tiempo)

t.test(produccion$Germ ~ produccion$Tiempo, paired = T)