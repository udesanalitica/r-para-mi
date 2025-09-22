## =================================================================================
# Diseño Completamente al azar (DCA)
## =================================================================================

# Problema a analizar:Escribir contexto del Diseño Experimental 
# Fuente de los datos:Escribir nombre del estudiante, Tesis URL

# Seleccionar el directorio donde tenemos nuestros archivos #
getwd () ## Para ver donde se estan guardando los archivos

#Especifico el directorio que me interesa. 
#NOTA: R lee / (slash o division) y no el de Windows \
setwd ("D:/OneDrive - Universidad de Santander/Material Docente 2025/CodigoR")

#Lectura de datos
library(readxl)
DCA2 <- read_excel("DCA2.xlsx")
View(DCA2)

attach(DCA2)
names(DCA2)
str(DCA2)
summary(DCA2)

#ANOVA
Anova<-aov(Resultado~Tratamiento, data=DCA2)
summary(Anova)

#Modelo Lineal
modelo=lm(Resultado~(Tratamiento))
summary(modelo)

#Grafico Boxplot
boxplot(Resultado ~ Tratamiento, data=DCA2)


#Supuestos del diseño

#Para verificar la normalidad de los residuos utilizaremos la prueba de Shapiro-Wilks cuyo script es el siguiente:
shapiro.test(residuals(Anova))

#Para construir el gr?fico QQ plot y verificar la normalidad, se invoca el paquete "car"

library(car)
#Grafico de QQ plot

qqPlot(Anova)

#Para verificar el supuesto de homocedasticidad de las varianzas utilizaremos la prueba de Bartlett script es el siguiente:
bartlett.test(Resultado~Tratamiento, data=DCA2)

#Pruebas aposteriori
#Utilizaremos la tecnica LSD (Least Signifficant Difference), para lo cual debemos cargar la libreria agricolae.

library(agricolae)
Grupos<- LSD.test(y = Anova, trt = "Tratamiento", group = T, console = T)

#Otra opcion cuando cambiamos el argumento "group" a F(false), se interpreta a mi parecer de forma mas sencilla la diferencia entre las medias
Grupos<- LSD.test(y = Anova, trt = "Tratamiento", group = F, console = T)

TukeyHSD(Anova)
plot(TukeyHSD(Anova))

scheffe.test(Anova, "Tratamiento",console=TRUE)

