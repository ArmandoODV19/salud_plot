#ENSANUT 2018: Analisis nacional
#Fecha: 07/07/2021
#Laura Rodríguez


library(tidyverse)
library(plyr)
library(wesanderson)

#Cargar ENSANUT y cambiar nombres de columnas

raw_data <- read.csv("/home/Lau/Documentos/ENSANUT_nutricion/CN_ALIMENTOS_ADU.csv")


#Cambiar los nombres de las variables

colnames(raw_data) <- c("upm", "viv_sel", "hogar", "numren", "grupo", "subgrupo",
                       "fcsemana", "fc/dia", "tamano/porcion", 
                       "numero/porcion", "codigo", "suplemento", "edad", 
                       "sexo", "entidad", "dominio", "altitud", "region", 
                       "estrato" , "f_alim_com", "f_alim_com_insp", "est_dis",
                       "upm_dis")

##Limpiar dataset

#Para barras
data_filtrado <- raw_data %>% select("grupo", "subgrupo", "codigo", "edad","region",
                                     "sexo", "estrato", "fcsemana", "dominio", "entidad")


#Crear una nueva columna para los grupos de alimentos
data_filtrado <- data_filtrado %>% 
  mutate(alimentos = grupo)

data_filtrado <- data_filtrado %>% 
  mutate(zona = region)




#Cambiar cuantitativos a cualitativas

data_filtrado$alimentos <- as.factor(data_filtrado$alimentos)
data_filtrado$zona <- as.factor(data_filtrado$zona)
data_filtrado$estrato <- as.factor(data_filtrado$estrato)
data_filtrado$sexo <- as.factor(data_filtrado$sexo)
data_filtrado$entidad <- as.factor(data_filtrado$entidad)
data_filtrado$dominio <- as.factor(data_filtrado$dominio)


str(data_filtrado$grupo)
str(data_filtrado$alimentos)
str(data_filtrado)
summary(data_filtrado$grupo)
#Asignar nombre a alimentos en ali
data_filtrado$alimentos <- revalue(data_filtrado$alimentos, c("1"= "Lacteos", "2"="Frutas", "3"="Verduras",
                                    "4"="Comida rapida", "5"="Origen animal",
                                    "6"="Alimentos del mar", "7"="Leguminosas",
                                    "8"= "Cereales", "9"="Productos del maíz", 
                                    "10"="Bebidas", "11"="Botanas, dulces y postres",
                                    "12"="Sopas, cremas y pastas", "13"="Misceláneos",
                                    "14"="Tortilla", "15"="Cantidad de consumo reportada",
                                    "16"= "Suplementos"))
#Asignar nombre a regiones
data_filtrado$zona <- revalue(data_filtrado$zona, c("1"="Norte", "2"="Centro", "3"="CdMx",
                                          "4"="Sur"))

#Asignar nombre a estrato socioeconómico
data_filtrado$estrato <- revalue(data_filtrado$estrato, c("1"="Bajo", "2"="Medio bajo",
                                            "3"="Medio alto", "4"="Alto"))
#Asignar nombre a sexo
data_filtrado$sexo <- revalue(data_filtrado$sexo, c("1"="Hombre", "2"="Mujer"))

#Asignar valores a dominio
data_filtrado$dominio <- revalue(data_filtrado$dominio, c("1"="Urbano", "2"="Rural"))

#Asignar valores a entidad
data_filtrado <- data_filtrado %>% mutate(entidades = entidad)

data_filtrado$entidades <- revalue(data_filtrado$entidades, c("1"= "Aguascalientes", 
                                                          "2"="Baja California Norte", 
                                                          "3"="Baja California Sur",
                                                          "4"="Campeche", "5"="Coahuila",
                                                          "6"="Colima", "7"="Chiapas",
                                                          "8"= "Chihuahua", 
                                                          "9"="CdMx", 
                                                          "10"="Durango", 
                                                          "11"="Guanajuato",
                                                          "12"="Guerrero", 
                                                          "13"="Hidalgo",
                                                          "14"="Jalisco", 
                                                          "15"="Edo. México",
                                                          "16"= "Michoacán",
                                                          "17"="Morelos",
                                                          "18"="Nayarit",
                                                          "19"= "Nuevo León",
                                                          "20"="Oaxaca",
                                                          "21"= "Puebla",
                                                          "22"="Querétaro", 
                                                          "23"="Quintana Roo",
                                                          "24"="San Luis Potosí",
                                                          "25"= "Sinaloa",
                                                          "26"="Sonora",
                                                          "27"="Tabasco",
                                                          "28"="Tamaulipas",
                                                          "29"="Tlaxcala", 
                                                          "30"="Veracruz",
                                                          "31"="Yucatán", 
                                                          "32"="Zacatecas"))
#Intervalos por edades

data_filtrado <- data_filtrado %>% 
  mutate(edades= case_when(edad <= 19 ~"Adolescentes",
                           edad >= 20 & edad <= 59 ~"Adultos",
                           edad >= 60 ~"Adultos mayores"))
bigotes <- data_filtrado

data_filtrado <- data_filtrado[!(data_filtrado$`fcsemana`==0),]


#Lista para consumo

gg_consumo <- list(geom_bar(stat= "count"),
                   theme_minimal(),
                   theme(axis.text.x = element_text(angle=45, hjust = 1),),
                   scale_y_continuous(labels = function(x) format(x, scientific = FALSE)),
                   ylab("Frecuencia de consumo"),
                   xlab("Grupo de alimentos"))

#############La medida de mis plots son 750*400

                      #### N A C I O N A L ####

#Gráfica para el consumo de alimentos a nivel nacional
ggplot(data_filtrado, aes(x=alimentos, fill=alimentos))+
  geom_bar(stat = "count")+
  gg_consumo+
  theme(legend.position = "none")+
  #geom_text(aes(label=..count..), stat="count", vjust= -0.1, colour= "black",
   #         size=3)+
  ggtitle("Consumo de alimentos en México ENSANUT 2018")

#Para evaluar el consumo por regiones y estrato se va a normalizar POR CONSUMO 
#GENERAL DE ALIMENTOS (no por grupo de alimentos) y se va a generar dos tibble
# un tibble es un dataframe optimizado

                  ##### R E G I O N E S #####
#Tibble para regiones
R<- data_filtrado %>% 
  group_by(alimentos) %>% 
  select(alimentos, zona) %>% 
  count()

summary(R)
str(R)
#Se hace summary para conocer la frecuencia máxima y poder 
#normalizarlo/ponderarlo 

regiones <- as_tibble(R)
regiones <- group_by(as.character((regiones$zona)))
regiones <- regiones %>% group_by(zona)
summary(regiones)

regiones$normalizar <- (regiones$freq-149)/(41142-149)*100
regiones$normalizado <- (regiones$freq)/(41142)*100

#Gráfica para consumo de alimentos por región 
ggplot(regiones, aes(x=zona, y=normalizado, fill=zona))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Consumo de alimentos por regiones")+
  ylab("")+
  facet_grid(.~alimentos, switch = "x")+
  theme(strip.text.x = element_text(size = 13, angle = 90, hjust = 1),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))

              ##### S O C I O D E M O G R A F I C O #####

S <- data_filtrado %>% 
  group_by(alimentos) %>% 
  select(alimentos, estrato) %>% count()

summary(S)

S$normalizar <- (S$freq-289)/(58143-289)*100
S$normalizado <- (S$freq)/(58143)*100

socioeconomico <- as_tibble(S)
socioeconomico <- group_by(as.character((socioeconomico$estrato)))

#Gr?fica para consumo de alimentos por estrato sociodemografico
ggplot(socioeconomico, aes(x=estrato, y=normalizado, fill=estrato))+
  geom_bar(stat = "identity")+
  theme_classic()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(fill = "Estrato sociodemográfico")+
  ggtitle("Consumo de alimentos por estrato sociodemográfico")+
  ylab("")+
  scale_fill_manual(values = wes_palette(name = "GrandBudapest1"))+
  facet_grid(.~alimentos, switch = "x")+
  theme(strip.text.x = element_text(size = 13, angle = 90, hjust = 1),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))
 
                            #### S E X O ####

SX <- data_filtrado %>% 
  group_by(alimentos) %>% 
  select(alimentos, sexo) %>% count()
summary(SX)

SX$normalizar <- (SX$freq-1181)/(57218-1181)*100
SX$normalizado <- (SX$freq)/(57218)*100

sexo_p <- as_tibble(SX)
sexo_p <- group_by(as.character((SX$sexo)))

#Gr?fica para consumo de alimentos por sexo

ggplot(sexo_p, aes(x=sexo, y=normalizar, fill=sexo))+
 geom_bar(stat = "identity")+
 theme_classic()+
 theme(axis.title.x=element_blank(),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank())+
 ggtitle("Consumo de alimentos en M?xico por sexo")+
 ylab("")+
 scale_fill_manual(values = wes_palette(name = "Darjeeling2"))+
 facet_grid(.~alimentos, switch = "x")+
 theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
       panel.border=element_blank(),
       strip.background=element_rect(colour="white", fill="white"))

                        ##### POR EDADES #####
edad <- data_filtrado %>% 
  group_by(alimentos) %>% 
  select(alimentos, edades) %>% count()
summary(edad)

edad$normalizar <- (edad$freq-469)/(63078-469)*100
edad$normalizado <- (edad$freq)/(63078)*100

sexo_p <- as_tibble(SX)

ggplot(edad, aes(x=edades, y=normalizar, fill=edades))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Consumo de alimentos en México por edades")+
  ylab("")+
  scale_fill_manual(values = wes_palette(name = "Darjeeling1"))+
  facet_grid(.~alimentos, switch = "x")+
  theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))
 
                        ##### ESTADOS ####
##Norte

Norte <- data_filtrado %>% 
  group_by(entidades) %>% 
  filter(zona == "Norte") %>% 
  select(alimentos, entidades, zona) %>% count()

summary(Norte)

Norte$normalizar <- (Norte$freq-53)/(4547-53)*100

Norte <- as_tibble(Norte)

ggplot(Norte, aes(x=alimentos, y=freq, fill=entidades))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Consumo de alimentos en M?xico por entidad federativa en el norte")+
  ylab("")+
  xlab("Frecuencia")+
  facet_wrap(.~entidades)+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))
#Ya se automatizó en estados por region

ggplot(frutas, aes(x=subgrupo, y=fcsemana, fill=subgrupo))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Frecuencua de consumo de frutas en México")+
  theme(legend.position = "none")

#Otra prueba
ggplot(raw_data, aes(x=grupo, y=fcsemana, fill=estrato))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggtitle("Frecuencua de consumo de alimentos en México")+
  theme(legend.position = "none")+
  facet_grid(.~alimentos, switch = "x")+
  theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))
        
data_filtrado2