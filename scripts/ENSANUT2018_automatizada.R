library(tidyverse)


            ##### C O D I G O   D E   F U N C I O N E S  P O R  G R U P O #####


                              #### P O R   R E G I O N ####

                          #####  Estados por región  #####
#Variables: región (norte, centro, cdmx y sur)
region <- function(data, region, titulo){
  data %>%
    group_by(entidades) %>%
    select(alimentos, entidades, zona) %>%
    filter(zona == region) %>% count() %>%
    ggplot(aes(x=alimentos, y=freq, fill=entidades))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle(titulo)+
    ylab("")+
    xlab("Frecuencia")+
    facet_wrap(.~entidades)+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}



                     #### Estrato sociodemográfico y regiones ####
#Variables: región y estrato sociodemográfico
estrato_region <- function (data, region, titulo){
  data %>%
    group_by(estrato) %>%
    select(alimentos, estrato, zona) %>%
    filter(zona == region) %>% count() %>%
    ggplot(aes(x=estrato, y=freq, fill=estrato))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("dodgerblue3", "cadetblue3", "hotpink3", "orange3"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Estrato sociodemográfico")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}


                              #### Sexo y regiones ####
sexo_region <- function (data, region, titulo){
  data %>%
    group_by(sexo) %>%
    select(alimentos, sexo, zona) %>%
    filter(zona == region) %>% count() %>%
    ggplot(aes(x=sexo, y=freq, fill=sexo))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("lightblue4", "salmon2"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Sexo")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}
                           ### Edad y regiones ####
edad_region <- function (data, region, titulo){
  data %>%
    group_by(edades) %>%
    select(alimentos, edades, zona) %>%
    filter(zona == region) %>% count() %>%
    ggplot(aes(x=edades, y=freq, fill=edades))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("turquoise3", "yellow4", "orchid4"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Edades")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

                               ####Dominio y regiones ####
dominio_region <- function (data, region, titulo){
  data %>%
    group_by(dominio) %>%
    select(alimentos, dominio, zona) %>%
    filter(zona == region) %>% count() %>%
    ggplot(aes(x=dominio, y=freq, fill=dominio))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("lightskyblue", "turquoise4"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Dominio")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

                             #### P O R   E N T I D A D E S ####
#### Estado total ####
estado_grupo <- function(data, estado, titulo){
  data %>%
    group_by(entidades) %>%
    filter(estado==entidades) %>%
    select(alimentos, entidades) %>% count() %>%
    ggplot(aes(x=alimentos, y=freq, fill=alimentos))+
    geom_bar(stat="identity")+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle(titulo)+
    ylab("")+
    xlab("Frecuencia")+
    facet_grid(.~entidades)+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))
}



                            ### Estrato y Estado ####
estrato_estado <- function (data, estado, titulo){
  data %>%
    group_by(estrato) %>%
    filter(entidades == estado) %>%
    select(alimentos, estrato, region) %>% count() %>%
    ggplot(aes(x=estrato, y=freq, fill=estrato))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("yellow3", "turquoise", "indianred1","palevioletred4"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Estrato sociodemográfico")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

                          ### Sexo y estado ####
sexo_estado <- function (data, estado, titulo){
  data %>%
    group_by(sexo) %>%
    filter(entidades == estado) %>%
    select(alimentos, sexo, region) %>% count() %>%
    ggplot(aes(x=sexo, y=freq, fill=sexo))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("lightgoldenrod3", "steelblue1"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Sexo")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}
                           #### Edades y estado ####
edad_estado <- function (data, estado, titulo){
  data %>%
    group_by(edades) %>%
    filter(entidades == estado) %>%
    select(alimentos, edades, region) %>% count() %>%
    ggplot(aes(x=edades, y=freq, fill=edades))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("indianred2",  "peachpuff3", "skyblue2"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Grupo de edad")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

                            #### Dominio y estado ####
dominio_estado <- function (data, estado, titulo){
  data %>%
    group_by(dominio) %>%
    filter(entidades == estado) %>%
    select(alimentos, dominio, region) %>% count() %>%
    ggplot(aes(x=dominio, y=freq, fill=dominio))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("palevioletred3",  "slategrey", "skyblue2"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Dominio")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

                             #### Sexo, estrato, edad, estado ####
especifico <- function(data, genero, socio, estado, dominios, gpo_edad, titulo){
  data %>%
    select(sexo, region, estrato, entidades, alimentos, dominio, edades) %>%
    filter(sexo==genero,
           estrato==socio,
           entidades==estado,
           edades==gpo_edad,
           dominio==dominios) %>%
    ggplot(aes(x=alimentos, fill=alimentos))+
    geom_bar(stat = "count")+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle(titulo)+
    ylab("Frecuencia de consumo")+
    xlab("")+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))+
    geom_text(aes(label=..count..), stat="count", vjust= -0.1, colour= "black",
              size=3)

}


                            ########  A U T O M A T I Z A C I Ó N   ########
   #Variables --> Escribir tal cual
#Región: Norte, Centro, CdMx y Sur
#Estado: Entidades federativas
#Sexo: Hombre, Mujer
#Estrato sociodemográfico: Bajo, Medio bajo, Medio alto y Alto
#Edad: Adolescentes, Adultos, Adultos mayores
#Dominio: Rural y Urbano



                            ##### P O R   R E G I O N #####

#Region total
region(data=data_filtrado, region = "Sur",
       titulo = "Consumo de alimentos por entidad federativa en el Sur")

#Estrato y region
estrato_region(data=data_filtrado, region = "Sur",
               titulo = "Consumo de alimentos en el centro por estrato sociodemográfico")

#Sexo y región

sexo_region(data=data_filtrado, region = "Sur",
            titulo = "Frecuencia de consumo en el sur por sexo")

#Edad y region
edad_region(data=data_filtrado, region = "Centro",
            titulo = "Frecuencia de consumo en el centro por edad")

#Rural y region
dominio_region(data = data_filtrado, region = "Sur",
               titulo = "Consumo de alimentos en el sur por dominio")




                      ##### P O R    E N T I D A D   F E D E R A T I V A #####
###Estado
estado_grupo(data=data_filtrado, estado = "Jalisco",
             titulo = "Consumo de alimentos por entidad federativa en el Sur")

###Estrato y estado
estrato_estado(data=data_filtrado, estado = "Sonora",
               titulo = "Frecuencia de consumo en el Sonora por estrato sociodemográfico")

#Sexo y estado
sexo_estado(data=data_filtrado, estado = "Chiapas",
            titulo = "Frecuencia de consumo en el Chiapas por sexo")

#Edad y estado
edad_estado(data=data_filtrado, estado = "Morelos",
            titulo = "Frecuencia de consumo en Morelos por grupo de edad")

#Dominio y estado
dominio_estado(data = data_filtrado, estado = "Zacatecas",
               titulo = "Consumo de alimentos en Zacatecas por dominio")

#Específico: sexo, edad, estrato y estado
especifico(data = data_filtrado, genero="Hombre", socio="Bajo",
           estado="Sonora", dominios= "Rural", gpo_edad="Adolescentes",
           titulo = "Consumo de alimentos en adolescentes hombres de Sonora en un estrato
           sociodemográfico bajo en una zona rural")



           #### C O D I G O  D E  F U N C I O N E S  S U B G R U P O S####



                                     #### NACIONAL ####

nacional_subgrupo <- function(data, titulo){
  data %>%
    group_by(subgrupo)  %>%
    select(subgrupo) %>% count() %>%
    ggplot(aes(x=subgrupo, y=freq, fill=subgrupo))+
    geom_bar(stat="identity")+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle(titulo)+
    ylab("")+
    xlab("Frecuencia")+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10))

}

                            #### R E G I O N E S ####
catalogo_region <- function(data, region, titulo){
  data %>%
    group_by(entidades) %>%
    select(subgrupo, entidades, zona) %>%
    filter(zona == region) %>% count() %>%
    ggplot(aes(x=subgrupo, y=freq, fill=subgrupo))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle(titulo)+
    ylab("")+
    xlab("Frecuencia")+
    facet_wrap(.~entidades)+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

                          #### ESTADO ####

estado_subgrupo <- function(data, estado, titulo){
  data %>%
    group_by(subgrupo)  %>%
    filter(entidades == estado) %>%
    select(subgrupo) %>% count() %>%
    ggplot(aes(x=subgrupo, y=freq, fill=subgrupo))+
    geom_bar(stat="identity")+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle(titulo)+
    ylab("")+
    xlab("Frecuencia")+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10))

}
#Sexo y estado

catalogo_ef_sexo <- function (data, estado, titulo){
  data %>%
    group_by(sexo) %>%
    filter(entidades == estado) %>%
    select(subgrupo, sexo, entidades) %>% count() %>%
    ggplot(aes(x=sexo, y=freq, fill=sexo))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("lightgoldenrod3", "steelblue1"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Dominio")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~subgrupo, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))
}


#Dominio y estado
catalogo_ef_dominio <- function (data, estado, titulo){
  data %>%
    group_by(dominio) %>%
    filter(entidades == estado) %>%
    select(subgrupo, dominio, entidades) %>% count() %>%
    ggplot(aes(x=dominio, y=freq, fill=dominio))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("palevioletred3",  "olivedrab3", "skyblue2"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Sexo")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~subgrupo, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

#Edades y estado
catalogo_ef_edad <- function (data, estado, titulo){
  data %>%
    group_by(edades) %>%
    filter(entidades == estado) %>%
    select(subgrupo, edades, region) %>% count() %>%
    ggplot(aes(x=edades, y=freq, fill=edades))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("indianred2",  "peachpuff3", "skyblue2"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Grupo de edad")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~subgrupo, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

#Estrato y estado
catalogo_ef_estrato <- function (data, estado, titulo){
  data %>%
    group_by(estrato) %>%
    filter(entidades == estado) %>%
    select(subgrupo, estrato, region) %>% count() %>%
    ggplot(aes(x=estrato, y=freq, fill=estrato))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    scale_fill_manual(values = c("yellow3", "turquoise", "indianred1","palevioletred4"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ggtitle(titulo)+
    labs(fill = "Estrato sociodemográfico")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~subgrupo, switch = "x")+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}
              ###### A U T O M A T I Z A C I O N  P O R  S U B G R U P O ####

#El catlogo de alimentos contiene a los alimentos que están dentro de cada grupo
#Se deben escribir tal cual

#lacteos            #comida_rapida    #botanas
#frutas             #carnes           #bebidas
#verduras           #mariscos         #leguminosas
#cereales           #maiz             #sopas

                      #### N A C I O N A L ####
nacional_subgrupo(data = carnes, titulo = "Consumo de carnes en México")


                     #### P O R  R E G I O N E S ###
#Region total
catalogo_region(data=bebidas, region = "Norte",
       titulo = "Consumo de alimentos por entidad federativa en el Centro")



                     #### P O R  E N T I D A D  F E D E R A T I V A #####
#Total
estado_subgrupo(data = bebidas, estado = "Sonora",
                titulo = "Consumo de bebidas en Quintana Roo")

#Dominio y estado
catalogo_ef_dominio(data = frutas, estado = "Veracruz",
               titulo = "Consumo de frutas en Morelos por dominio")

#Sexo y estado
catalogo_ef_sexo(data = bebidas, estado = "Morelos",
                    titulo = "Consumo de frutas en Morelos por sexo")

#Edad y estado
catalogo_ef_edad(data = frutas, estado = "Tlaxcala",
                 titulo = "Consumo de frutas en Morelos por grupo de edad")

#Estrato y estado
catalogo_ef_estrato(data = botanas, estado = "Hidalgo",
                 titulo = "gggfgfj")
