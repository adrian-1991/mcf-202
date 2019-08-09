# Adrian Botello Montoya 
# 07/08/19
# Clase 4


# Correlacion  ------------------------------------------------------------


erupciones <- read.csv("C:/Maestria/erupcion.csv", header = T)
head(erupciones)

plot(log(erupciones$waiting), log(erupciones$eruptions), xlab = "Tiempo de espera (min)",
     ylab = "Duración (min)",pch = 19)
library(pastecs)

stat.desc(erupciones$eruptions,basic=FALSE, norm =TRUE)
shapiro.test(erupciones$eruptions)

shapiro.test(erupciones$eruptions)

cor.test(erupciones$eruptions, erupciones$waiting)

# La correlacion es significativa, ya que el p_value es menor que el 0.05
# de los niveles de confianza


# Correlacion  ------------------------------------------------------------

# comando lm para realizar la regresion 

lm.erup <- lm(erupciones$eruptions ~ erupciones$waiting)

plot(erupciones$eruptions ~ erupciones$waiting, pch= 19,  
     col= "red", xlab = "tiempo de espera (min)", 
     ylab = "duracion (min)")
abline(lm.erup, col= "blue")
text(46, 4.5, "Y = -1.87 + 0.07*x")
text(52, 4, "r^2 = 0.81")
lm.erup
summary(lm.erup)
Y.60 <- -1.87 + 0.07*60
Y.60




# los valores residuales son los valores que se encuentran dentro de los valores
# observados y la linea

length(erupciones$eruptions)


# Valor de la R cuadrada  -------------------------------------------------

squrt(.90)

# Datos de regresion ------------------------------------------------------
espera <- erupciones$waiting
duracion <- erupciones$eruptions
res <- resid(lm.erup)
pre <- fitted(lm.erup)
res.2 <- res^2
cuadro <- round(data.frame(espera, duracion, pre, res,
                           res.2),4)
SSE <- sum ((cuadro$res)^2)
SSE
 vari <- SSE/(length(erupciones$waiting)-2)
vari
var(espera)

an.erup <- anova(lm.erup)
an.erup

# Los resultados de f_ value se rechasa h0 y se acepta H1
# lo que nos menciona que la regresion es significativa 