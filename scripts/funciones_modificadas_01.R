# nuevas funciones

ensanut_filtrado <- read.csv("clean_data/ensanut_filtrado.csv")

# Inicio de funciones

# Grafico por regiones (Norte, Centro, Sur, cdmx)

region_plot <- function(region, title, title_alig = 0.5){
  ensanut_filtrado %>%
    group_by(entidades) %>%
    select(alimentos, entidades, zona) %>%
    filter(zona == region) %>% count() %>%
    ggplot(aes(x=alimentos, y=freq, fill=entidades))+
    geom_bar(stat = "identity")+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle(title)+
    ylab("")+
    xlab("Frecuencia")+
    facet_wrap(.~entidades)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

# clasificacion por region y estrato socioeconomico

# se deben normalizar estos datos
# hacer grafico que solo obtenga datos por estrato

socioeconomic_plot <- function (region, title, title_alig = 0.5){
  ensanut_filtrado %>%
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
    ggtitle(title)+
    labs(fill = "Estrato sociodemográfico")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

# clasificacion por sexo

sex_region_plot <- function (region, title, title_alig = 0.5){
  ensanut_filtrado %>%
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
    ggtitle(title)+
    labs(fill = "Sexo")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}


# clasificacion por edad

age_plot <- function (region, title, title_alig = 0.5){
  ensanut_filtrado %>%
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
    ggtitle(title)+
    labs(fill = "Edades")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}

# clasificacion por dominio

settlement_plot <- function (region, title, title_alig = 0.5){
  ensanut_filtrado %>%
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
    ggtitle(title)+
    labs(fill = "Dominio")+
    ylab("Frecuencia de consumo")+
    xlab("")+
    facet_grid(.~alimentos, switch = "x")+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(strip.text.x = element_text(size = 10, angle = 90, hjust = 1),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))

}
