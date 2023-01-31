# Instalação e carregamento dos pacotes utilizados

pacotes <- c(
              "tidyverse" #carregar outros pacotes do R
              ,"dplyr"
              ,"readxl" # importar arquivo Excel
              ,"knitr", "kableExtra" #formatação de tabelas
              ,"plotly" #plataforma gráfica
              ,"reshape2" #função 'melt'
              ,"cluster" #função 'agnes' para elaboração de clusters hierárquicos
              ,"factoextra" #função 'fviz_dend' para construção de dendrogramas
              ,"ade4"
              ,"wordcloud"
              ,"plot3D"
              ,"scales"
              ,"ggplot2"
              ) 

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregar DB
vr_db <- read_excel("../2022.10.12 - DB.xls")

#Anonimizar cabeçalhos
vr_db <- vr_db %>% rename("F1" = 1,
                          "F2" = 2,
                          "BRAND" = 3,
                          "MODEL" = 4,
                          "VERSION" = 5,
                          "DOOR" = 6,
                          "F7" = 7,
                          "F8" = 8,
                          "F9" = 9,
                          "New" = 10,
                          "year_0" = 11,
                          "year_1" = 12,
                          "year_2" = 13,
                          "year_3" = 14,
                          "year_4" = 15)

#Remover colunas 
vr_db <- vr_db[,1:15] #após a 15 - Espaço temporal fora do escopo do estudo - > 4 anos de uso

vr_db <- vr_db %>%  select(-F1, -F2, -F7, -F8, -F9) #Colunas de dados sensíveis

head(vr_db)

summary(vr_db)

#Remover Veículos que não possuem versão 0K, ou não possuem preço nos anos de estudo
nrow(vr_db)
vr_db <- vr_db %>% filter(New != 0 & 
                            year_1 != 0 &
                            year_2 != 0 &
                            year_3 != 0 &
                            year_4 != 0)
nrow(vr_db)

#Avaliar número de portas
vr_db %>% count(DOOR) %>% 
kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# 0 = Moto
# 1 = Ônibus
# 3 = Vans
# 5 = Vans
# 6 = Vans

vr_db <- vr_db %>% filter(DOOR %in% c("2P", "3P", "4P", "5P", "6P"))

nrow(vr_db)

#Marcas não disponíveis no mercado brasileiro
vr_db %>% count(BRAND)
vr_db <- vr_db %>% filter(!BRAND %in% c("AGRALE",
                                        "BITREM",
                                        "EFFA",
                                        "FOTON",
                                        "LAMBORGHINI",
                                        "LEXUS",
                                        "MCLAREN",
                                        "REBOQUE",
                                        "RODOTREM",
                                        "SCANIA",
                                        "SEMI-REBOQUE"))
nrow(vr_db)

brands_volume <- vr_db %>% select(BRAND, MODEL) %>% distinct() %>% count(BRAND)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

brands_volume %>% with(wordcloud(BRAND, n, random.order = F, max.words = 50, colors=pal, scale=c(2, .5)))



################################################################################

#Cálculo Residual por ano

summary(vr_db)
write.csv(summary(vr_db),"../price_summary.csv")

vr_db <- vr_db %>% mutate(residual_year_1 = year_1/New,
                          residual_year_2 = year_2/New,
                          residual_year_3 = year_3/New,
                          residual_year_4 = year_4/New,)

summary(vr_db[,11:14])
write.csv(summary(vr_db[,11:14]),"../price_residual_summary.csv")


#Fazer - Identificado Outlier que baixou de preço, remover os casos
nrow(vr_db)
vr_db <- vr_db %>% filter(residual_year_1 < 1 &
                          residual_year_2 < 1 &
                          residual_year_3 < 1 &
                          residual_year_4 < 1
                            )
nrow(vr_db)
#Outlier superior a 1 - justificado por inflação, padrão não esperado
summary(vr_db[,11:14])

################################################################################

#Gráfico não apresentou uma visualização de fácil compreensão
#scatter3D(x=vr_db$residual_year_2,
#          y=vr_db$residual_year_3,
#          z=vr_db$residual_year_4,
#          phi = 0, bty = "g", pch = 20, cex = 1,
#          xlab = "Residual 2º ano",
#          ylab = "Residual 3º ano",
#          zlab = "Residual 4º ano",
#          main = "Residual",
#          clab = "Residual")



# Boxplots por variável

boxplot_data <- vr_db[,c(1:3,11:14)] %>%
  mutate(VEHICLE = paste(paste(BRAND, MODEL, sep = " "), VERSION, sep = " ")) %>% 
  select(VEHICLE, residual_year_1, residual_year_2, residual_year_3, residual_year_4) %>% 
  rename("1º Ano" = 2, "2º Ano" = 3, "3º Ano" = 4, "4º Ano" = 5)
  

ggplotly(
    boxplot_data %>% 
    melt() %>%
    ggplot(aes(label = VEHICLE)) +
    geom_boxplot(aes(x = variable, y = value, fill = variable)) +
    geom_point(aes(x = variable, y = value), alpha = 0.5) +
    labs(x = "Ano",
         y = "% Residual") +
    scale_fill_manual("Legenda:",
                      values = c("dodgerblue", "green", "gold", "red")) +
      #stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
    theme_bw()
)


# Matriz de dissimilaridades
matriz_D <- vr_db %>% 
  select(residual_year_1, residual_year_2, residual_year_3, residual_year_4) %>% 
  dist(method = "euclidean")


# Visualizando a matriz de dissimilaridades - Não exibir
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)


# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = matriz_D, method = "complete") #single 

# Definição do esquema hierárquico de aglomeração

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes


# Tabela com o esquema de aglomeração. Interpretação do output:

## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio

esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
#esquema %>%
#  kable(row.names = T) %>%
#  kable_styling(bootstrap_options = "striped", 
#                full_width = FALSE, 
#                font_size = 20)


# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier)

# Dendrograma com visualização dos clusters (definição de 3 clusters)
fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "deeppink"),
          color_labels_by_k = F,
          rect = T,
          #rect_fill = T,
          lwd = 1,
          main = "Dendrograma com 3 Agrupamentos",
          xlab = "Observações",
          ylab = "Altura",
          show_labels = F,
          ggtheme = theme_bw()) +
  guides(scale = c(0, 0.65))
      scale_y_continuous(limits = c(0,0.65))




#---------- Esquema de aglomeração não hierárquico K-MEANS ---------------------

# Método de Elbow para identificação do número ótimo de clusters
## Apresenta a variação total dentro dos clusters para várias nº de clusters
## Em geral, quando há a dobra é um indício do número ótimo de clusters
fviz_nbclust(vr_db[,11:14],  
             kmeans, 
             method = "wss", 
             k.max = 6) + 
        labs(title= "K-means - Método de Elbow") + 
        xlab("Clusters") +
        ylab("Variação Total")

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(vr_db[,11:14],
                         centers = 3)

# Criando variável categórica para indicação do cluster no banco de dados
vr_db$cluster_K <- factor(cluster_kmeans$cluster)


# Visualização da base de dados
head(vr_db) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

head(vr_db)

summary(vr_db$cluster_K)

# ANOVA da variável
summary(anova_ry1 <- aov(formula = residual_year_1 ~ cluster_K,
                                data = vr_db))


#Interpretação do Resultado
cluster_1 <- vr_db %>% filter(cluster_K == 1) %>% 
            group_by(BRAND, MODEL) %>% 
            summarise(qtt = n())

cluster_2 <- vr_db %>% filter(cluster_K == 2) %>% 
            group_by(BRAND, MODEL) %>% 
            summarise(qtt = n())

cluster_3 <- vr_db %>% filter(cluster_K == 3) %>% 
            group_by(BRAND, MODEL) %>% 
            summarise(qtt = n())

#categorização dos resultados
vr_db <- vr_db %>% mutate(cluster_name = case_when(cluster_K == 1 ~ "Básico",
                                                   cluster_K == 2 ~ "Executivo",
                                                   cluster_K == 3 ~ "Importados"))

resiudal_ranges <- vr_db %>% select(cluster_name, 
                         residual_year_1, 
                         residual_year_2, 
                         residual_year_3, 
                         residual_year_4) %>% 
        group_by(cluster_name) %>% 
        summarise(mean_year_1 = round(mean(residual_year_1), 4),
                  mean_year_2 = round(mean(residual_year_2), 4), 
                  mean_year_3 = round(mean(residual_year_3), 4), 
                  mean_year_4 = round(mean(residual_year_4), 4)) %>% 
        rename("Grupo" = 1, "1º Ano" = 2, "2º Ano" = 3, "3º Ano" = 4, "4º Ano" = 5)

resiudal_ranges %>% 
  kable(row.names = T) %>%
    kable_styling(bootstrap_options = "striped", 
                  full_width = FALSE, 
                  font_size = 20)

#ggplotly(
  resiudal_ranges %>% 
    melt() %>%
    ggplot(aes(x=Grupo, y=percent(), group=Grupo)) +
    geom_line(aes(x = variable, y = value , color = Grupo)) +
    geom_point(aes(x = variable, y = value, color = Grupo), alpha = 0.5) +
    geom_text(aes(x = variable, y = value, label = percent(round(value, 2))),  
              nudge_x = 0.075,
              #color="white",
              size = 5,
              fontface="bold") +
    labs(title = "Residual por Grupo",
          x = "Ano",
         y = "% Residual") +
    #scale_y_continuous(labels = percent_format(), limits=c(0.5,0.98)) +
    
    scale_y_continuous(labels = percent_format(),
                       breaks = pretty_breaks(n = 8))+
    theme_bw()
#)