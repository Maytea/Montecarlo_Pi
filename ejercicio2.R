# Numero de puntos 
N = 1000

# Puntos aleatorios en un cuadrado unidad
x= runif(N)
y= runif(N)

# Dibujo un circulo
curve(sqrt(1-x^2),0,1)

# Los puntos estan dentro de un circulo unidad si x^2 + y^2 < 1
under = ifelse (x^2 + y^2 < 1, 1, 0)

# Dibujo los puntos coloreados segun caen fuera o dentro
points(x,y,col=under+1,cex=0.2)

# Calculo el area del cuadrante de circunferencia
(area = sum (under)/N)

# Que sabemos que deberia ser pi/4
# Podemos estimar pi como area*4
(pi_est= area*4)

# Definimos una funcion para hacer el calculo de pi. El unico
# numero de entrada sera el numero de puntos que se generen. 
# Cuantos mas puntos, mas preciso sera el calculo de pi.
funcion_pi <- function(n){
  
  # Puntos aleatorios en un cuadrado unidad
  x= runif(n)
  y= runif(n)
  
  # Los puntos estan dentro de un circulo unidad si x^2 + y^2 < 1
  under = ifelse (x^2 + y^2 < 1, 1, 0)

  # Calculo el area del cuadrante de circunferencia
  area = sum (under)/n
  
  # Que sabemos que deberia ser pi/4
  # Podemos estimar pi como area*4
  pi_est= area*4
  pi_est
  
}


# Responde a las siguientes preguntas:

# Usando la funcion sapply estima pi, usando 10,100,1000,10000 y 100000.

numero_puntos = c(10, 100, 1000, 10000, 100000)
sapply(numero_puntos, funcion_pi)

# Ahora genera una matriz por filas tenga 100 estimaciones de pi usando 
# 10, 100, 1000, 10000 y 100000 puntos respectivamente. La matriz tendra
# 5 filas y 100 columnas.

matrix_vacia= matrix(NA, nrow=5, ncol=100)
for (i in 1:100) matrix_vacia [1,i]= funcion_pi(10)
for (i in 1:100) matrix_vacia [2,i]= funcion_pi(100)
for (i in 1:100) matrix_vacia [3,i]= funcion_pi(1000)
for (i in 1:100) matrix_vacia [4,i]= funcion_pi(10000)
for (i in 1:100) matrix_vacia [5,i]= funcion_pi(100000)


# Calcula con apply, la media (mean) y la desviacion tipica (sd) de las filas 
# de la matriz.

matrix_vacia [1,]
media_10puntos = mean(matrix_vacia [1,])
media_100puntos = mean(matrix_vacia [2,])
media_1000puntos = mean(matrix_vacia [3,])
media_10000puntos = mean(matrix_vacia [4,])
media_100000puntos = mean(matrix_vacia [5,])


desviacion_estandar_10puntos = sd(matrix_vacia [1,])
desviacion_estandar_100puntos = sd(matrix_vacia [2,])
desviacion_estandar_1000puntos = sd(matrix_vacia [3,])
desviacion_estandar_10000puntos = sd(matrix_vacia [4,])
desviacion_estandar_100000puntos = sd(matrix_vacia [5,])


# Dibuja un boxplot con las estimaciones de pi

boxplot(matrix_vacia [1,], horizontal = TRUE)
boxplot(matrix_vacia [2,], horizontal = TRUE)
boxplot(matrix_vacia [3,], horizontal = TRUE)
boxplot(matrix_vacia [4,], horizontal = TRUE)
boxplot(matrix_vacia [5,], horizontal = TRUE)


