# nuevas funciones

ensanut_filtrado <- read.csv("clean_data/ensanut_filtrado.csv")

# Inicio de funciones

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

