# Adrian Botello Montoya 
# 07/08/19
# Clase 5


arena <- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo <- c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)


y.ton <- c(arena, arcilla, limo)

suelo <-gl(3, 10, 30, labels=c("arena", "arcilla", "limo"))
prod <- data.frame(suelo, y.ton)
head(prod)


tapply(prod$y.ton, prod$suelo, mean)
tapply(prod$y.ton, prod$suelo, var)



# pruebas de varianza -----------------------------------------------------


shapiro.test(prod$y.ton)
# los ddatos provienen de una distribucion normal

bartlett.test(prod$y.ton, prod$suelo)

# para determinar la homeginedad de varaianzas


# visualizacion de los datos  ---------------------------------------------

boxplot(prod$y.ton ~ prod$suelo, xlab = "tipo de suelo", ylab = "ton/ha", col="red")

aov.suelo <- aov(prod$y.ton ~ prod$suelo)
aov.suelo
summary(aov.suelo)


par(mfrow=c(2,2))
plot(aov(prod$y.ton ~ prod$suelo))
par(mfrow=c(1,1))


TukeyHSD(aov.suelo, conf.level = 0.95)

plot(TukeyHSD(aov.suelo))
summary.lm(aov.suelo)

# el analisis de varianza es significativo

# H° que la produccion en toneladas de ceral es el mismo
# en los tres tipos de suelo.

# H1 que al menos uno de los tratamientos es diferente 
pol
