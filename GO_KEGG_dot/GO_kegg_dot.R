rm(list = ls())
setwd('D:/R/Task/task13')

library(readxl)
#install.packages('ggh4x')
#devtools::install_github("teunbrand/ggh4x")
#install.packages('D:/R/library/ggh4x_0.2.8.tar.gz', repos = NULL, type = 'source')
library(ggh4x)
#install.packages('ggstar')
#install.packages('D:/R/library/ggstar_1.0.4.tar.gz', repos = NULL, type = 'source')
library(ggstar)
library(ggnewscale)
library(ggfun)

df <- read_xlsx(path = "test_data.xlsx", col_names = T) %>%
  dplyr::mutate(ONTOLOGY = factor(ONTOLOGY, levels = c("BP","CC","MF","KEGG"), ordered = T))%>%
  dplyr::mutate(cellclass = factor(ONTOLOGY, levels = c("BP","CC","MF","KEGG"), ordered = T))


ratio <- matrix(as.numeric(unlist(strsplit(as.character(df$GeneRatio),"/"))),ncol=2,byrow=TRUE)
df$GeneRatio <- ratio[,1]/ratio[,2]



ggplot(data = df, aes(x = compare, y = Description)) + 
  geom_point(data = df %>% dplyr::filter(compare == "compare1"),
             aes(x = compare, y = Description, size = GeneRatio, color = -log10(pvalue))) + 
  scale_color_gradient(low = "#d4b9da", high = "#ce1256")+
  new_scale_color() +
  geom_point(data = df %>% dplyr::filter(compare == "compare2"),
             aes(x = compare, y = Description, size = GeneRatio, color = -log10(pvalue)),
             shape = 18) +
  scale_color_gradient(low = "#c7e9c0", high = "#006d2c") + 
  scale_y_discrete(position = "right") + 
  labs(x = "", y = "") +
  scale_size(range = c(4, 8)) + 
  facet_grid2(ONTOLOGY ~ compare+cellclass,
              scales = "free", 
              switch = "y",
              space = "free",
              strip = strip_nested(
                background_y = elem_list_rect(fill = c("#bcbddc", "#fcbba1", "#a6bddb", "#addd8e")),
                background_x = elem_list_rect(fill = c("#bcbddc", "#fcbba1")),
                by_layer_x = T
              ))+
  theme(
    panel.background = element_rect(fill = "white",colour = NA),
    panel.grid.minor = element_line(linewidth = rel(0.5)),
    panel.grid = element_line(colour = "grey92"),
    panel.spacing.x = unit(c(0,0,0,0.3,0,0,0), "cm"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
    legend.background = element_roundrect(color = "#636363", linetype = 1)
  )

pdf(file=paste(prefix,'.pdf',sep=''))
p
dev.off()

svg(filename=paste(prefix,'.svg',sep=''))
p
dev.off()
ggsave(filename=paste(prefix,'.png',sep=''),type='cairo-png',plot=p)