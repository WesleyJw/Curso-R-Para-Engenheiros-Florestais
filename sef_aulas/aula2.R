#Graficos com ggplot2

library(ggplot2)
library(gridExtra)

names(df3)

#Scatterplot - dispersao

graph <- ggplot(data = df3, aes(y = volume, x = dap))

graph.1 <- graph +
    geom_point(size = 3, shape = 21, colour = "blue", fill = "green")

graph.1 + geom_smooth(method = "lm")


graph.2 <- ggplot(data = df3, aes(y = volume, x = dap, 
                                  color = espacamento)) +
    geom_point(size = 3)

ggsave("Figuras/grap_2_ggsave.pdf", width = 8, 
       height = 6, units = "cm", dpi = 900)

#Histograma
graph.3 <- ggplot(data = df3, aes(x = dap))

graph.4 <- graph.3 + 
    geom_histogram(bins = 15, colour = "darkblue", 
                   fill = "darkgreen")
graph.5 <- graph.3 +
    geom_density(colour = "darkblue",
                 fill = "lightblue", alpha = .5)

grid.arrange(graph.1, graph.2, graph.4, graph.5, ncol = 1)


vol.e <- tapply(df3$volume, df3$espacamento, mean)
arv.h <- unique(df3$arvoresh)

df.vol <- data.frame(espacamento = names(vol.e),
                     vol_ha = vol.e * arv.h)

graph.6 <- ggplot(data = df.vol, aes(x = espacamento,
                                     y = vol_ha)) +
    geom_bar(stat = "identity", 
             fill = c("#BC8F8F", "#CD853F", "#D2691E",
                      "#F4A460"), width = .8) +
    coord_flip()

#box plot
graph.7 <- ggplot(data = df3, aes(y = volume, 
                                  x = espacamento, 
                                  fill = espacamento)) +
    geom_boxplot()


graph.p1 <- graph.1 +
    scale_y_continuous(limits = c(0, 0.2), 
                       breaks = seq(0, 0.2, 0.025),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(6, 16), 
                       breaks = seq(6, 16, 1),
                       expand = c(0, 0)) +
    xlab("Diâmetro a Altura do Peito -DAP (cm)") + 
    ylab("Volume por árvore (m³)") +
    ggtitle("Gráfico de Dispersão") +
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          text = element_text(family = "Times New Roman", size = 16))


