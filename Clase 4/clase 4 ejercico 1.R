# Adrian Botello Montoya 
# 07/08/19
# Clase 4.1


# Extraer Datos  ----------------------------------------------------------


ebanos <- read.csv("C:/Maestria/ebanos.csv", header = T)
head(ebanos)


# comparar graficamemte  ------------------------------------------------------


plot(ebanos$altura ~ ebanos$diametro, col= "red", ylab = "altura (cm)", xlab = "diametro (cm)",pch= 17)

# observando la grafica, se puede analizar visualmente que los datos provienen de 
# una distribucion normal

stat.desc(ebanos$altura,basic=FALSE, norm =TRUE)


# Pruebas de normalidad  --------------------------------------------------


shapiro.test(ebanos$altura)

shapiro.test(ebanos$diametro)

# de acuerdo a los datos obtenidos en la prueba de normalidad de los datos,
# donde nos da un resultado de P_ value es menor a 0.05 se dice
# que los datos son anormales, por lo que se rechaza H0 y se acepta H1



# Analisis de Correlacion -------------------------------------------------


cor.test(ebanos$diametro, ebanos$altura)
# La correlacion es significativa, ya que el p_value es menor que el 0.05
# de los niveles de confianza, por lo que se concluye que las variables analizadas 
# tienen relacion altamente significativa, por lo que se rechaza Ho y se acepta HI
