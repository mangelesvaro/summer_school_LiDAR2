# summer_school_LiDAR2

[![DOI](https://zenodo.org/badge/694534100.svg)](https://zenodo.org/doi/10.5281/zenodo.10454197)

# Capítulo 10: Sensores activos en ciencias forestales: LiDAR

En el presente ejercicio se va a aprender a realizar un inventario forestal con datos LiDAR.

## 4. Extracción de métricas de parcela y modelización de variables de inventario

Siguiendo el flujo de trabajo habitual de un inventario LiDAR trabajando a nivel de masa, tras la normalización de los pulsos, se llevaría a cabo la extracción de las métricas de las parcelas y su modelización con las variables de campo.

Es importante que durante la fase de campo se hayan seleccionado las parcelas cubriendo toda la variabilidad de la masa. De esta manera el modelo que se genere, represente todo el rango de datos del monte.

### 4.1. Introducir los datos necesarios

Se introducen los datos LiDAR normalizados en el ejercicio anterior. Para seleccionar sólo los que están normalizados y evitar los originales, se selecciona empleando el patrón del sufijo *norm* con los que lse guardaron.

```r
library(lidR)

#Introducir los archivos LiDAR como un catalogo de datos
catalogo_norm<-readLAScatalog(folder="E:/DESCARGA/",     #Adaptar a la ruta de descarga utilizada
                              pattern = "norm.las")
```

Se necesita una muestra de parcelas de campo donde se realicen mediciones de las variables de masa que se quieran estimar y de donde tengamos información LiDAR. Se introducen, por tanto, los datos de las mediciones de las parcelas de campo, a través del excel [colgado en la plataforma](https://github.com/Libro-GEOFOREST/Capitulo13_LiDAR_y_Radar/tree/main/DatosTrabajodeCampo) y que se debe haber descargado previamente.

```r
library(readxl)

#Leer archivo con datos de campo
campo<-read_excel("E:/DESCARGA/DatosTrabajodeCampo/resultado parcelas.xls")  #Adaptar a la ruta de descarga utilizada

#Convertir la tabla en un data frame
campo<-as.data.frame(campo)
```

Se comprueba la tabla de datos de campo.

```r
#Ver la tabla de datos
View(campo)
```

Parcela | Radio | N_pies_ha | G_m2_ha | Dg_cm | dmedio_cm | Ho_Assman_m | hmedia_m
--- | --- | --- | --- | --- | --- | --- | ---
1 | 11 | 552.4426 | 13.81511 | 17.84379 | 17.17500 | 9.144 | 8.230550
10 | 11 | 499.8290 | 11.93160 | 17.43380 | 17.23158 | 8.870 | 7.367421
11 | 11 | 1236.4191 | 28.62428 | 17.16872 | 17.17391 | 8.670 | 8.840000
12 | 11 | 894.4309 | 26.16349 | 19.29869 | 18.76765 | 9.410 | 8.480647
13 | 11 | 1052.2716 | 24.97441 | 17.38350 | 17.25641 | 10.310 | 8.847025
14 | 11 | 1078.5784 | 20.72051 | 15.63968 | 15.39756 | 7.460 | 6.714585
  
A continuación, se introducen ahora las coordenadas con la localización de las parcelas. Es necesario que dicha localización debe ser lo suficientemente precisa como para evitar que se confundan las zonas medidas en campo y las zonas de las que se extrae la estadística LiDAR. Por ejemplo, un GPS con un error de medición de varios metros podría "transportar" la parcela medida a un cortafuegos o un camino forestal, cuyas métricas LiDAR no tendrían nada que ver con la de la vegetación que se ha medido en campo.

```r
#Introducir coordenadas de las parcelas
library(sf)
```

```r
coord.parc<-st_read("E:/DESCARGA/DatosTrabajodeCampo/coordenadas_parcelas.shp")  #Adaptar a la ruta de descarga utilizada
```

Y se visualiza la tabla asociada al archivo shapefile.

```r
#Ver la tabla de datos
View(as.data.frame(coord.parc))
```

ID | Parcela | geometry
--- | --- | ---
1 | 1 | POINT (535750 4118454)
2 | 2 | POINT (535350 4118554)
3 | 3 | POINT (535050 4118654)
4 | 4 | POINT (535350 4118754)
5 | 5 | POINT (534750 4118654)

La representación cartográfica de las parcelas sobre la zona de vuelo LiDAR descargada quedaría así:

```r
#Representación cartográfica de las parcelas
library(mapview)

mapa1<-mapview(catalogo_norm, alpha.regions = 0, color = "red", 
        lwd = 2, layer.name = "Datos LiDAR",
        map.type = "Esri.WorldImagery",legend=FALSE)

mapa2<-mapview(coord.parc,zcol="Parcela", layer.name="Parcelas")

mapa1+mapa2
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/parcelas.png)

### 4.2. Extracción de métricas LiDAR de las parcelas

Las métricas de una nube de puntos LiDAR consisten en una serie de estadísticas que describen y resumen la distribución de las alturas y/o intensidades de los puntos que a ella pertenecen. 

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/metricas.png)

La librería *lidR* en R permite el cálculo una serie de estadísticos predefinidos en la nube de puntos. Pero además, aporta la enorme ventaja de su capacidad para definir nuevas estadísticas a través de líneas de código. Cualquier usuario puede crearlas en función de las características de la masa que se pretenda modelizar. Como ejemplo, se ha incluido una función para calcular métricas bastante utilizadas en el mundo forestal.

```r
#Definir métricas a extraer
library(moments)
metricas=function(z){
        n <- length(z)
        zmin <- min(z)
        zmean <- mean(z)
        zmax <- max(z)
        zsd <- sd(z)
        zskew <- skewness(z)
        zkurt <- kurtosis(z)
        zq25 <- quantile(z,prob=0.25)
        zq50 <- quantile(z,prob=0.50)
        zq60 <- quantile(z,prob=0.60)
        zq75<-quantile(z,prob=0.75)
        zq80<-quantile(z,prob=0.80)
        zq90<-quantile(z,prob=0.90)
        zpabovezmean<-round(length(which(z>zmean))/n*100,2)
        zpabovez2<-round(length(which(z>2))/n*100,2)
        metrics=list(n=n,
                     zmin=zmin,
                     zmean=zmean,
                     zmax=zmax,
                     zsd=zsd,
                     zskew=zskew,
                     zkurt=zkurt,
                     zq25=zq25,
                     zq50=zq50,
                     zq60=zq60,
                     zq75=zq75,
                     zq80=zq80,
                     zq90=zq90,
                     zpabovezmean=zpabovezmean,
                     zpabovez2=zpabovez2
                     )
        return(metrics)
}
```

Y posteriormente, se aplica la función definida por nosotros sobre los datos del catalogo LiDAR, conociendo las coordenadas de las parcelas y su radio, que es de 11 metros, como se observa en la columna *Radio* de la tabla de datos de campo.

```r
#Ejecutar la extracción de métricas de parcelas
metricas.parcelas<-plot_metrics(catalogo_norm,
                                ~metricas(Z),coord.parc,
                                radius=11)

metricas.parcelas
```

```r annotate
## Simple feature collection with 27 features and 17 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 534750 ymin: 4118454 xmax: 537166.1 ymax: 4121237
## Projected CRS: WGS 84 / UTM zone 30N
## First 10 features:
##    ID Parcela    n   zmin    zmean   zmax      zsd       zskew    zkurt
## 1   1       1  144 -0.191 2.923757  9.410 3.268588  0.46609380 1.566404
## 2   2       2  204 -0.032 3.719201  8.737 3.114305 -0.21569186 1.281997
## 3   3       3  353 -0.065 3.091776 10.429 3.463975  0.49533538 1.622113
## 4   4       4  152 -0.072 2.127980  6.527 2.201560  0.34726012 1.552400
## 5   5       5  275  0.000 3.635869  9.283 3.333343  0.05467518 1.299853
## 6   6       6  322 -0.242 4.202984 10.574 3.597308 -0.18899969 1.253465
## 7   7       7  216 -0.221 4.842528 12.460 4.159956 -0.09964796 1.383319
## 8   8       8  127 -0.055 9.030512 16.891 5.879556 -0.61464270 1.821459
## 9   9       9  949 -0.934 4.776403 14.726 5.076684  0.25732807 1.358689
## 10 10      10 1184 -0.210 3.042573  8.385 2.540536 -0.03463205 1.420987
##        zq25    zq50    zq60     zq75    zq80    zq90 zpabovezmean zpabovez2
## 1   0.00000  0.4720  4.1244  6.01425  6.4426  7.7618        45.83     46.53
## 2   0.00000  5.2375  5.7302  6.42125  6.6620  7.0938        58.33     60.78
## 3   0.00000  0.2930  3.9592  6.46600  7.0558  8.1462        43.91     48.44
## 4   0.00000  2.0710  3.1992  4.13425  4.3982  4.9930        50.00     50.00
## 5   0.00000  4.4230  5.5990  6.77200  7.0300  7.9190        53.82     55.27
## 6   0.00000  6.1435  6.5456  7.28950  7.4842  8.1028        58.39     59.01
## 7   0.00000  6.5120  7.4010  8.23900  8.5730  9.7370        56.94     60.19
## 8   0.05650 11.1010 12.2496 13.57800 14.2812 15.1366        63.78     74.02
## 9  -0.15700  2.9930  7.4816  9.81000 10.5664 11.6532        48.16     51.84
## 10  0.17975  3.7800  4.4750  5.25450  5.5080  6.0960        56.08     60.30
##                    geometry
## 1    POINT (535750 4118454)
## 2    POINT (535350 4118554)
## 3    POINT (535050 4118654)
## 4    POINT (535350 4118754)
## 5    POINT (534750 4118654)
## 6  POINT (535349.3 4119656)
## 7  POINT (535355.6 4118945)
## 8  POINT (535842.8 4119051)
## 9    POINT (536150 4119154)
## 10   POINT (536750 4119154)
```

Como se observa, el resultado consiste en un shapefile de puntos con tantos registros como datos introducidos y cuya tabla asociada contiene las métricas de la nuble LiDAR en las parcelas.

```r
#Ver resultados
View(as.data.frame(metricas.parcelas))
```

ID | Parcela | n | zmin | zmean | zmax | zsd | zskew | zkurt | zq25 | zq50 | zq60 | zq75 | zq80 | zq90 | zpabovezmean | zpabovez2 | geometry
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
1 | 1 | 144 | -0.191 | 2.923757 | 9.410 | 3.268588 | 0.4660938 | 1.566404 | 0.00000 | 0.4720 | 4.1244 | 6.01425 | 6.4426 | 7.7618 | 45.83 | 46.53 | POINT (535750 4118454)
2 | 2 | 204 | -0.032 | 3.719201 | 8.737 | 3.114305 | -0.2156919 | 1.281997 | 0.00000 | 5.2375 | 5.7302 | 6.42125 | 6.6620 | 7.0938 | 58.33 | 60.78 | POINT (535350 4118554)
3 | 3 | 353 | -0.065 | 3.091776 | 10.429 | 3.463975 | 0.4953354 | 1.622113 | 0.00000 | 0.2930 | 3.9592 | 6.46600 | 7.0558 | 8.1462 | 43.91 | 48.44 | POINT (535050 4118654)
4 | 4 | 152 | -0.072 | 2.127980 | 6.527 | 2.201560 | 0.3472601 | 1.552400 | 0.00000 | 2.0710 | 3.1992 | 4.13425 | 4.3982 | 4.9930 | 50.00 | 50.00 | POINT (535350 4118754)
5 | 5 | 275 | 0.000 | 3.635869 | 9.283 | 3.333343 | 0.0546752 | 1.299853 | 0.00000 | 4.4230 | 5.5990 | 6.77200 | 7.0300 | 7.9190 | 53.82 | 55.27 | POINT (534750 4118654)
6 | 6 | 322 | -0.242 | 4.202985 | 10.574 | 3.597308 | -0.1889997 | 1.253465 | 0.00000 | 6.1435 | 6.5456 | 7.28950 | 7.4842 | 8.1028 | 58.39 | 59.01 | POINT (535349.3 4119656)
7 | 7 | 216 | -0.221 | 4.842528 | 12.460 | 4.159956 | -0.0996480 | 1.383319 | 0.00000 | 6.5120 | 7.4010 | 8.23900 | 8.5730 | 9.7370 | 56.94 | 60.19 | POINT (535355.6 4118945)
8 | 8 | 127 | -0.055 | 9.030512 | 16.891 | 5.879556 | -0.6146427 | 1.821459 | 0.05650 | 11.1010 | 12.2496 | 13.57800 | 14.2812 | 15.1366 | 63.78 | 74.02 | POINT (535842.8 4119051)
9 | 9 | 949 | -0.934 | 4.776402 | 14.726 | 5.076684 | 0.2573281 | 1.358689 | -0.15700 | 2.9930 | 7.4816 | 9.81000 | 10.5664 | 11.6532 | 48.16 | 51.84 | POINT (536150 4119154)
10 | 10 | 1184 | -0.210 | 3.042573 | 8.385 | 2.540536 | -0.0346321 | 1.420987 | 0.17975 | 3.7800 | 4.4750 | 5.25450 | 5.5080 | 6.0960 | 56.08 | 60.30 | POINT (536750 4119154)
11 | 11 | 748 | -0.154 | 4.310402 | 9.888 | 3.230078 | -0.2512194 | 1.460595 | 0.39750 | 5.5025 | 6.0682 | 7.04125 | 7.3482 | 8.0897 | 60.29 | 64.57 | POINT (535775.8 4119217)
12 | 12 | 636 | -1.046 | 4.379083 | 10.200 | 3.687601 | -0.3245495 | 1.408023 | -0.15100 | 5.9290 | 6.6580 | 7.44925 | 7.7940 | 8.4210 | 61.64 | 64.15 | POINT (536250 4119254)
13 | 13 | 219 | -0.338 | 4.076247 | 10.436 | 3.789519 | -0.0311945 | 1.219511 | 0.00000 | 5.6370 | 6.4870 | 7.56650 | 7.8414 | 8.5152 | 54.79 | 55.71 | POINT (535953.6 4119854)
14 | 14 | 1288 | -0.124 | 3.123131 | 8.086 | 2.517762 | -0.1518637 | 1.332295 | 0.24200 | 4.1160 | 4.6804 | 5.28425 | 5.5144 | 5.9930 | 58.23 | 59.94 | POINT (535350 4119254)
15 | 15 | 253 | -0.075 | 2.202676 | 8.089 | 2.696216 | 0.6312236 | 1.734134 | 0.00000 | 0.0960 | 2.6680 | 4.66100 | 5.3998 | 6.2700 | 41.50 | 41.90 | POINT (535551.9 4119551)
17 | 17 | 973 | -0.778 | 3.636291 | 9.295 | 3.250584 | -0.2172299 | 1.341470 | -0.13800 | 5.0840 | 5.6572 | 6.44600 | 6.7434 | 7.3112 | 59.61 | 61.05 | POINT (535832.1 4119442)
19 | 19 | 182 | 0.000 | 2.539478 | 7.613 | 2.436899 | 0.2274244 | 1.527509 | 0.00000 | 2.7600 | 3.5696 | 4.80125 | 5.0422 | 5.6211 | 51.65 | 54.40 | POINT (536146.8 4119961)
20 | 20 | 344 | -0.162 | 3.340602 | 9.113 | 3.042132 | 0.0065676 | 1.315977 | 0.00000 | 4.5365 | 5.3136 | 6.00000 | 6.2712 | 6.9967 | 54.65 | 56.10 | POINT (535550 4119754)
22 | 22 | 252 | 0.000 | 5.267294 | 11.989 | 4.275847 | -0.2205838 | 1.346517 | 0.00000 | 6.9810 | 8.0558 | 8.87800 | 9.2620 | 10.1436 | 58.33 | 62.70 | POINT (536850 4120054)
23 | 23 | 238 | -0.119 | 5.677706 | 11.009 | 4.075608 | -0.5861394 | 1.509746 | 0.00000 | 7.8875 | 8.2462 | 8.84100 | 8.9986 | 9.5375 | 65.55 | 67.23 | POINT (535750 4120054)
21 | 21 | 248 | -0.282 | 3.095867 | 9.478 | 3.027750 | 0.2350487 | 1.428990 | 0.00000 | 3.2025 | 4.4820 | 5.92500 | 6.4574 | 7.1041 | 50.40 | 53.23 | POINT (536350 4120054)
25 | 25 | 250 | -0.091 | 5.678732 | 12.471 | 4.480034 | -0.2634613 | 1.354276 | 0.00000 | 7.4740 | 8.6360 | 9.54900 | 9.9506 | 10.6841 | 58.40 | 65.20 | POINT (536849.2 4120257)
27 | 27 | 204 | -0.230 | 3.936181 | 9.604 | 2.940838 | -0.2960101 | 1.555978 | 0.00000 | 4.9800 | 5.5200 | 6.31175 | 6.6996 | 7.2727 | 59.31 | 67.16 | POINT (537166.1 4121150)
28 | 28 | 170 | -0.298 | 11.277694 | 21.323 | 6.500379 | -0.4875670 | 1.870323 | 5.41425 | 13.7210 | 14.7012 | 16.71275 | 17.0700 | 18.5512 | 58.82 | 85.88 | POINT (536855.7 4121123)
29 | 29 | 190 | 0.000 | 12.130379 | 18.495 | 5.335021 | -1.5152373 | 3.947334 | 11.76325 | 13.9845 | 14.5300 | 15.47425 | 15.7718 | 16.5263 | 71.58 | 85.79 | POINT (537039.8 4121237)
30 | 30 | 264 | -0.284 | 4.149523 | 10.040 | 3.861291 | -0.0408131 | 1.178377 | 0.00000 | 5.7985 | 6.8646 | 7.87925 | 8.0740 | 8.6989 | 54.92 | 54.92 | POINT (536950.8 4119754)
31 | 31 | 920 | -0.186 | 3.186741 | 8.393 | 2.688232 | -0.1099419 | 1.308814 | 0.13675 | 4.2870 | 4.8738 | 5.55875 | 5.7694 | 6.2943 | 57.39 | 58.59 | POINT (536839.2 4119197)

### 4.3. Modelización de las variables

Un modelo lineal o regresión lineal se utiliza para predecir el resultado de una variable *y*, sobre la base de una o varias variables predictoras *x*. El objetivo consiste en construir una fórmula matemática que defina el comportamiento de la variable *y* en función de la variable *x*. 

$$y = b_{0} + b_{1}·x$$

Después de construir el modelo, estadísticamente significativo, es posible usarlo para predecir resultados con nuevos valores de la variable *x*.

En los inventarios LiDAR, la variable *y* será cualquiera de las variables dasométricas medidas en campo, por ejemplo, altura dominante, área basimétrica, volumen de madera, biomasa, etc... La variable *x* será una variable conocida de la que se tiene toda la información de forma continua en todo el área de estudio, una métrica LiDAR. A través de dicha métrica se predecirá la variable de campo de forma continua en toda la superficie.

```r
#Unir tablas
parcelas<-merge(campo,metricas.parcelas)
```

#### 4.3.1. Estudio de la variable respuesta área basimétrica G (m2/ha)

Primero se va a comprobar la distribución de la variable respuesta. El tipo de modelos que se están empleando necesitan que siga una distribución normal. Para comprobarlo, se va a visualizar el histograma de distribución que sigue la variable, un gráfico cualtil-cuantil o qqplot que compara la distribución de la variable con la distribución normal teórica y finalmente se ejecuta un test de normalidad con la prueba de Shapiro-Wilks.

```r
#Nombres de los campos de la tabla
names(parcelas)
```

```r annotate
##  [1] "Parcela"      "Radio"        "N_pies_ha"    "G_m2_ha"      "Dg_cm"       
##  [6] "dmedio_cm"    "Ho_Assman_m"  "hmedia_m"     "ID"           "n"           
## [11] "zmin"         "zmean"        "zmax"         "zsd"          "zskew"       
## [16] "zkurt"        "zq25"         "zq50"         "zq60"         "zq75"        
## [21] "zq80"         "zq90"         "zpabovezmean" "zpabovez2"    "geometry"
```

```r
#Histograma de frecuencias
hist(parcelas$G_m2_ha,freq=FALSE)
lines(density(parcelas$G_m2_ha))
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/histograma_g.png)

```r
#Gráfico cuantil-cuantil
qqnorm(parcelas$G_m2_ha)
qqline(parcelas$G_m2_ha)
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/qq.png)

```r
#Prueba de Shapiro-Wilks
shapiro.test(parcelas$G_m2_ha) 
```

```r annotate
## 
##  Shapiro-Wilk normality test
## 
## data:  parcelas$G_m2_ha
## W = 0.94102, p-value = 0.1291
```

Como el *p-valor* del resultado de la prueba de Shapiro-Wilks es superior a 0.05, no se rechaza la hipótesis nula, es decir, que la distribución de la variable de área basimétrica se considera normal. 

```r
library (car)

#Gráfico de cajas y bigotes
Boxplot(parcelas$G_m2_ha)
```

```r annotate
## [1] 25
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/boxplot.png)

Existe un outlier o valor atípico en la parcela 25. Puede ser un error de la medición en campo o de la introducción de datos. Como desconocemos cuál es la causa real y desconocemos si es posible corregirlo, se elimina la parcela, que no formará parte del modelo.

```r
#Eliminar outlier
parcelas.o<-parcelas[-c(25),]
```

Ahora, se comprueba que este valor atípico no influye en la distribución normal de la variable.

```r
#Estudio normalidad
shapiro.test(parcelas.o$G_m2_ha)
```

```r annotate
## 
##  Shapiro-Wilk normality test
## 
## data:  parcelas.o$G_m2_ha
## W = 0.95011, p-value = 0.2333
```

El *p-valor* resultante del test continua siendo superior a 0.05.

Seguidamente, se realiza un estudio de normalidad para las métricas LiDAR predictoras. Sólo las cumplan con este riquisito podrán usarse en el modelo lineal. Se emplea para ello un bucle que va a repetir la prueba de Shapiro-Wilks a todos los campos que contienen variables LiDAR, entre el número de columna 10 y el 24, e imprime en pantalla sólo el resultado de las variables normales.

```r
for (i in 10:24){
        a<-shapiro.test(parcelas.o[,i])
        if (a$p.value>0.05){
                print(paste0(colnames(parcelas.o)[i],"->",a$p.value))
        }
}
```

```r annotate
## [1] "zskew->0.467304417253028"
## [1] "zkurt->0.0622810435575442"
## [1] "zq50->0.0898344423406685"
## [1] "zpabovezmean->0.155392876207957"
## [1] "zpabovez2->0.263918639516189"
```

Todas estas variables serán las que se utilicen en el modelo. Para determinar la mejor relación bivariante entre el área basimétrica y el resto de variables predictoras LiDAR, primero se va a determinar qué variables tienen una correlación estadísticamente significativa, esto es, con *p-valor* inferior a 0.05.

```r
library(Hmisc)

#Generación de matrices de correlaciones
correlaciones<-rcorr(as.matrix(parcelas.o[,c("G_m2_ha",
                                           "zskew",
                                           "zkurt",
                                           "zq50",
                                           "zpabovezmean",
                                           "zpabovez2")]),
                     type="pearson")

pp<-as.data.frame(correlaciones$P)
rr<-as.data.frame(correlaciones$r)

names(pp[which(pp$G_m2_ha<0.05)])
```

```r annotate
## [1] "zskew"        "zq50"         "zpabovezmean" "zpabovez2"
```

Y, a continuación, se visualizan las correlaciones entre las variables LiDAR y el área basimétrica.

```r
#Ver resultados
View(rr)
```

Var | G_m2_ha | zskew | zkurt | zq50 | zpabovezmean | zpabovez2
 --- | --- | --- | --- | --- | --- | ---
G_m2_ha | 1.0000000 | -0.8314934 | 0.0987390 | 0.9193272 | 0.7379081 | 0.8226766
zskew | -0.8314934 | 1.0000000 | 0.0257963 | -0.8804376 | -0.9734431 | -0.9040074
zkurt | 0.0987390 | 0.0257963 | 1.0000000 | 0.1722931 | -0.1534406 | 0.2689428
zq50 | 0.9193272 | -0.8804376 | 0.1722931 | 1.0000000 | 0.7788641 | 0.9319718
zpabovezmean | 0.7379081 | -0.9734431 | -0.1534406 | 0.7788641 | 1.0000000 | 0.8161738
zpabovez2 | 0.8226766 | -0.9040074 | 0.2689428 | 0.9319718 | 0.8161738 | 1.0000000

La variable explicativa LiDAR con la correlación estadísticamente significativa más alta con el área basimétrica medida en campo es el percentil 50 de las alturas de los puntos, *zq50*. Será esta métrica la que se emplee para crear el modelo.

Es importante analizar qué tipo de variable se va a emplear en el modelo para que éste tenga un sentido físico y no sea un artificio matemático que no aporte una explicación a la interacción entre la variable respuesta y predictora. En este ejemplo, al modelizar el área basimétrica, una variable que tiene que ver con la cantidad y el tamaño de los diámetros medidos a la altura del pecho, es lógico que muestre sensiblilidad a la distribución de los puntos centrales de la nube LiDAR.

```r
#Generar modelo lineal
modelo.G<-lm(G_m2_ha~zq50,data=parcelas.o)
summary(modelo.G)
```

```r annotate
## 
## Call:
## lm(formula = G_m2_ha ~ zq50, data = parcelas.o)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.2323 -1.0048  0.3196  1.6915  5.6732 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.9512     1.2448   8.797 5.64e-09 ***
## zq50          2.4372     0.2129  11.446 3.31e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.218 on 24 degrees of freedom
## Multiple R-squared:  0.8452, Adjusted R-squared:  0.8387 
## F-statistic:   131 on 1 and 24 DF,  p-value: 3.306e-11
```

Fijándonos en los resultados del modelo, en el valor de *Estimate* se obtendrá la estimación del parámetro $b_{0}$ y del $b_{1}$ de la ecuación lineal, que quedaría así:

$$G=10.9512+2.4372·zq50$$

#### Bondad del ajuste

Cuando se crea un modelo de regresión, se necesita evaluar el rendimiento predictivo del modelo, es decir, qué tan bien predice el resultado. Para ello se suelen usar las siguientes estadísticas:

```r
#Residuos del modelo
residuos<-modelo.G$fitted.values-parcelas.o$G_m2_ha
residuos
```

```r annotate
##           1           2           3           4           5           6 
## -1.71352196  0.39661607 -2.95295059  4.95062133 -5.67317561  0.02934902 
##           7           8           9          10          11          12 
## -4.06589366 -1.21215103 -0.67379562  8.23226486 -4.26232666 -0.76207056 
##          13          14          15          16          17          18 
## -0.28465318  0.26226050 -0.21189263 -2.46115493  1.96594842 -1.62538066 
##          19          20          21          22          23          24 
##  5.76720643 -0.35459661 -1.12147530  3.40566849  0.44982711  3.01868003 
##          26          27 
## -2.29323660  1.18983334
```

```r
#Residual sum of squares
RSS <- c(crossprod(residuos))
RSS
```

```r annotate
## [1] 248.4719
```

```r
#Mean squared error 
MSE <- RSS / length(modelo.G$fitted.values)
MSE
```

```r annotate
## [1] 9.556611
```

```r
#Root mean squared error
RMSE <- sqrt(MSE)
RMSE
```

```r annotate
## [1] 3.091377
```

```r
#Total sum of squares
TSS <- sum((parcelas.o$G_m2_ha - mean(parcelas.o$G_m2_ha)) ^ 2) 
TSS
```

```r annotate
## [1] 1604.727
```

```r
#R cuadrado
RSQ<-1-(RSS/TSS)
RSQ
```

```r annotate
## [1] 0.8451626
```

#### Gráfico predicho vs observado

Este gráfico muestra de forma muy descriptiva la dispersión respecto de la recta de la ecuación lineal para cada valor predicho, es decir, cuánto se alejan del ajuste calculado. Se espera no observar ningún tipo de patrón en los residuos y no ver datos atípicos que significarían datos con residuos muy grandes.

```r
#correlaciones predicho vs observado
correlaciones<-round(cor(modelo.G$fitted.values,
                         parcelas.o$G_m2_ha),3)

#Gráfico predicho vs observado
plot(parcelas.o$G_m2_ha, predict(modelo.G),
     xlab=as.expression(bquote("Área basimétrica observada ("~m^2~"/ha)")),
     ylab=as.expression(bquote("Área basimétrica predicha ("~m^2~"/ha)")))
abline(0,1) 
legend("topleft",legend=c(paste0("r=",round(correlaciones,2)),
                          as.expression(bquote(R^2 ==.(round(RSQ,2)))),
                          paste0("RMSE=",round(RMSE,2))),
       bty = "n")
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/predicho_observado.png)

Finalmente, se da por bueno el modelo generado. 

## 5. Extracción de métricas de superficie y resultados de variables de inventario

### 5.1. Calculo del tamaño de pixel

Una de los puntos clave de la extrapolacion de los resultados del modelo a toda la superficie es la decision del tamaño de pixel del raster que va a representar la variable calculada. Generalmente, se establece que el tamaño de celda debe ser equivalente al de tamaño de la parcela medida en campo para que los parametros estadisticos sean coherentes.

Por eso, si el radio de las parcelas era de 11 metros.

```r
#Superficie de la parcela
R=11
Tamano.parcela<-(R^2)*pi

#Lado del pixel
tamano.lado.pixel<-round(sqrt(Tamano.parcela),0)
tamano.lado.pixel
```

```r annotate
## [1] 19
```

El tamaño de celda sera, por tanto, de 19 m.

### 5.3. Extraccion de las metricas de superficies

Si para computar las metricas a nivel de parcela se empleaba la funcion *plot_metrics()*, para hacerlo a nivel de superficies se utiliza la funcion *grid_metrics()*, que calcula las estadisticas que se han definido en el ejercicio anterior para el conjunto de datos LiDAR dentro de cada pixel de un raster. En el parametro *res* se indica el tamaño de celda de 19 m.

```r
#Activacion de las librerias necesarias
library(lidR)
library(moments)

#Calculo de metricas en toda la superficie
metricas.superf <- grid_metrics(catalogo_norm,
                                ~metricas(Z), res = 19)
metricas.superf
```

```r annotate
## class      : RasterBrick 
## dimensions : 212, 211, 44732, 15  (nrow, ncol, ncell, nlayers)
## resolution : 19, 19  (x, y)
## extent     : 533995, 538004, 4117984, 4122012  (xmin, xmax, ymin, ymax)
## crs        : +proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
## source     : memory
## names      :          n,       zmin,      zmean,       zmax,        zsd,      zskew,      zkurt,       zq25,       zq50,       zq60,       zq75,       zq80,       zq90, zpabovezmean,  zpabovez2 
## min values :  17.000000, -22.684000,  -1.419597,   0.000000,   0.000000,  -2.548080,   1.101600,  -1.948000,  -1.410000,  -1.260800,  -1.032750,  -0.947000,  -0.715600,     0.000000,   0.000000 
## max values : 5502.00000,    0.00000,   16.43545,  280.16400,   13.47157,   19.48124,  401.31393,   16.60000,   19.65100,   20.23920,   21.66200,   22.70220,   24.43440,     82.97000,   94.39000
```

El resultado consiste en un objeto multicapa raster con la resolucion de 19 m y con la misma extension y sistema de referencia que los datos LiDAR descargados. Cada una de las capas corresponde a cada metrica definida en la funcion *metricas* creada en el ejercicio anterior.

```r
#Capas de metricas
names(metricas.superf)
```

```r annotate
##  [1] "n"            "zmin"         "zmean"        "zmax"         "zsd"         
##  [6] "zskew"        "zkurt"        "zq25"         "zq50"         "zq60"        
## [11] "zq75"         "zq80"         "zq90"         "zpabovezmean" "zpabovez2"
```

Y tambien se pueden visualizar geograficamente.

```r
#Capas de percentil 50
plot(metricas.superf$zq50)
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/zq50.png)

### 5.4. Resultados de variables de inventario

#### 5.4.1. Con la funcion *predict()*

La funcion *predict()* se utiliza para predecir resultados de un modelo sobre nuevos valores. En este ejemplo, los nuevos valores corresponden al raster de percentil 50 de toda la superficie de estudio.

```r
#Aplicar la prediccion del modeloal raster de percentil 50
G.predict<-predict(metricas.superf$zq50,modelo.G)
G.predict
```

```r annotate
## class      : RasterLayer 
## dimensions : 212, 211, 44732  (nrow, ncol, ncell)
## resolution : 19, 19  (x, y)
## extent     : 533995, 538004, 4117984, 4122012  (xmin, xmax, ymin, ymax)
## crs        : +proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
## source     : memory
## names      : layer 
## values     : 7.514767, 58.84476  (min, max)
```

El resultado consiste en una capa raster cuyos valores corresponden directamente a valores de area basimetrica expresados en $m^{2}/ha$, que es la misma unidad de medida con la que se introdujeron los valores desde la tabla de datos de campo al modelo.

```r
#Visualizar raster de area basimetrica
library(mapview())
mapview(G.predict,layer.name = "area basimetrica",
        map.type = "Esri.WorldImagery")
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/G.png)

#### 5.4.2. Aplicando la ecuacion de la regresion lineal

El resultado del modelo lineal simple que se ha cread es una ecuacion lineal. Se pueden conocer cual es la estimacion del parametro $b_{0}$ y del $b_{1}$ llamando a los coeficientes del modelo

```r
#Coeficientes del modelo modelo lineal
modelo.G$coefficients
```

```r annotate
## (Intercept)        zq50 
##   10.951227    2.437206
```

Y calcular el modelo siguiendo dicha ecuacion.

```r
#Coeficientes del modelo modelo lineal
G.predict2<-modelo.G$coefficients[1] +
        modelo.G$coefficients[2]*metricas.superf$zq50
G.predict2
```

```r annotate
## class      : RasterLayer 
## dimensions : 212, 211, 44732  (nrow, ncol, ncell)
## resolution : 19, 19  (x, y)
## extent     : 533995, 538004, 4117984, 4122012  (xmin, xmax, ymin, ymax)
## crs        : +proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
## source     : memory
## names      : zq50 
## values     : 7.514767, 58.84476  (min, max)
```

El resultado sigue siendo un raster cuyos valores corresponden a la estimacion segun el modelo en cada unidad de celda del area basimetrica expresados en $m^{2}/ha$.

Y su representacion cartografica es exactamente igual a la anterior.

```r
#Visualizar raster de area basimetrica
mapview(G.predict2,layer.name = "area basimetrica",
        map.type = "Esri.WorldImagery")
```

![](https://github.com/Libro-GEOFOREST/Capitulo10_LiDAR_y_Radar/blob/main/Auxiliares/G.png)

### 5.5. Guardar resultados

Finalmente, se guardan los resultados para poder emplearlo en un programa de sistema de informacion geografica como ArcGIS o QGIS.

```r
#Guardar raster
writeRaster(G.predict,
            filename="E:/DESCARGA/G.tif",                    #Adaptar a la ruta deseada
            format = "GTiff", # guarda como geotiff
            datatype='FLT4S') # guarda en valores decimales
```
