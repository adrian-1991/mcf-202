# Adrian Botello
#05/08/2019
#clase 0

# pasosbasicos ------------------------------------------------------------
a<- 2
2+2
a * a
a + 5

# importar datos  ---------------------------------------------------------

diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)
diametro
# Medidas de tendencia central 
mean(diametro)
median(diametro)
# Medidas dispersión 
var(diametro)
sd(diametro)

# graficas ----------------------------------------------------------------

boxplot(diametro, horizontal = TRUE, col="lightblue", main="diametro", xlab="d (cm)")



