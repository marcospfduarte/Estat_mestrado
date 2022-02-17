library("basedosdados")
library("dplyr")
library("tidyr")
library("ggplot2")
library(gridExtra)
library(RColorBrewer)
#### Fazendo o download via a Base dos Dados (BD)
## Selecionando projeto no Google Clound
set_billing_id("ra-amazon")
#Produzindo a query que buscará os dados
query <- bdplyr("br_inep_ideb.regiao")
# definindo o dataframe a partir da query no repositório online da BD
df <- bd_collect(query)
# Criando um subconjunto de informações apenas com os anos de 2019
ideb_2019 <- df %>%
  filter(
    ano == 2019,
    rede == "estadual",
    ensino == "medio"
  )
  
### gráfico com a avaliação do INEP para a educação estadual 
grafico_barras_ideb <-ideb_2019%>%
  ggplot(aes(x=regiao, y=ideb)) +
  geom_bar(stat="identity", fill = "palegreen4") +
  theme_ipsum() +
  coord_cartesian(ylim = c(0, 6)) +
  theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
  ggtitle("IDEB 2019 de escolas estaduais por Região") +
  xlab("Região")+
  ylab("Nota do IDEB 2019")

#Subconjunto 

ideb_sudeste <- df %>%
  filter(
    regiao == "Sudeste",
    rede == "estadual",
    ensino == "medio"
  )

grafico_linha <- ideb_sudeste %>% 
  ggplot(aes(x=ano, y=ideb,)) +
  geom_line()+
  geom_point()+
  theme_ipsum() +
  coord_cartesian(xlim = c(2005,2019), ylim = c(0,6))+
  ggtitle("IDEB 2019 de escolas estaduais no Sudeste") +
  xlab("Ano")+
  ylab("Nota do IDEB")+
  theme(plot.title = element_text(size=11))+
  scale_x_continuous(labels=as.character(ideb_sudeste$ano),breaks=ideb_sudeste$ano)
  

gridExtra::grid.arrange(grafico_barras_ideb,grafico_linha,ncol=2)
