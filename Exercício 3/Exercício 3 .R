library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
agrist <- read_html("https://scholar.google.com.br/citations?user=LmhqcScAAAAJ&hl=pt-BR&oi=sra")

#número de citações
citacoes <- agrist %>%
  html_nodes("#gsc_a_b .gsc_a_c") %>%
  html_text() %>%
  as.numeric()

data_citacoes <- as.data.frame(citacoes) #dataframe de numero de citacoes

## títulos dos papers

titulos <- read_html("https://scholar.google.com.br/citations?user=LmhqcScAAAAJ&hl=pt-BR&oi=sra") %>%
  html_nodes("#gsc_a_t .gsc_a_at") %>%
  html_text()

ano <-  agrist %>%
  html_nodes("#gsc_a_b .gsc_a_y") %>%
  html_text() %>%
  as.numeric()
anos <- as.data.frame(ano)

#merging both
agrist2 <- cbind(data_citacoes, data_titlos)
agrist2 <- cbind(agrist2, anos)

### Gráficos
lista <- c("Mostly harmless econometrics",
           "Identification of causal effects using instrumental variables", 
           "Does compulsory school attendance affect schooling and earnings?")
principais <- agrist2 %>%
  filter(titulos %in% lista )

grafico_barras <-principais%>%
  ggplot(aes(x=titulos, y=citacoes)) +
  geom_bar(stat="identity", fill = "palegreen4") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Citações de Joshua Angrist") +
  xlab("Títulos")+
  ylab("Citacões")

grafico_linha <- agrist2 %>% 
  ggplot(aes(x=ano, y=citacoes,)) +
  geom_line()+
  geom_point()+ 
  hrbrthemes::theme_ipsum() +
  ggtitle("Ano de publicação e número de citações corrente do trabalho dos 20 trabalhos mais citados") +
  xlab("Anos de Publicações")+
  ylab("Citações")+
  theme(plot.title = element_text(size=11))+
  scale_x_continuous(labels=as.character(agrist2$ano),breaks=agrist2$ano)

