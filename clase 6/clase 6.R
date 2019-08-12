# Adrian Botello Montoya 
# 07/08/19
# Clase 5

edad <- read.csv("C:/Maestria/datosc.csv", header = T)
head(edad)

#identificar columna sp como factor
edad$SP <- factor((edad$SP))
str(edad)



# separar factor  ---------------------------------------------------------

ariz <-subset(edad, SP == "arizonica")
ariz.lm <- lm(ariz$EDAD ~ ariz$DAP)
summary(ariz.lm)
dura <-subset(edad, SP == "duranguensis")


# regresion de factor -----------------------------------------------------
cov.edad <- lm(edad$EDAD ~ edad$DAP + edad$SP)
summary(cov.edad)

plot(edad$DAP[edad$SP == "arizonica"], edad$EDAD[edad$SP == "arizonica"],
     col="sky blue", pch="A", xlim=c(0,50),ylim=c(0,130))

abline(cov.edad$coefficients[1], cov.edad$coefficients[2], col="light green")
text(30,20, "Ya = -7.65+1.98*x")

points(edad$DAP[edad$SP == "durangensis"], edad$EDAD[edad$SP == "durangensis"],
       col="blue", pch="D")

abline(cov.edad$coefficients[1] + cov.edad$coefficients[3],
       cov.edad$coefficients[2], col="red", type = "dashed")
text(19, 100, "Yd = 19.06 + 1.98 * x")
