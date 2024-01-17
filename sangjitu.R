# 富集结果桑基图

suppressMessages({
library(tidyverse)
library(ggsankey)
library(cols4all)
#BiocManager::install("dittoSeq")
library(dittoSeq)
library(argparser)})


argv <- arg_parser('')
argv <- add_argument(argv,"--stat", help="the stat file")
argv <- add_argument(argv,"--type", help="the plot type")
argv <- add_argument(argv,"--prefix", help="the prefix of outfile")
argv <- parse_args(argv)

stat <- argv$stat
type <- argv$type
prefix <- argv$prefix

stat_frame <- read.delim(stat,head=TRUE,sep='\t',quote='')
if (type=='GO') {
  data <- dplyr::select(stat_frame, geneName, Description)%>%
    separate_rows(geneName, sep = '/')
  colnames(data) <- c('gene', 'pathway')
}

if (type=='KEGG') {
  data <- dplyr::select(stat_frame, geneName, Description)%>%
    separate_rows(geneName, sep = '/')
  colnames(data) <- c('gene', 'pathway')
}




data2 <- make_long(data, gene, pathway )


# ggplot(data2, aes(x = x, next_x = next_x,
#                node = node, next_node = next_node,
#                fill = factor(x),
#                label = node,)) +
#   geom_sankey(color = "grey", # node.color=, flow.color=
#               #fill = "brown", # node.fill=, flow.fill=
#               flow.alpha = 0.6,
#               # space = 1,
#               width = 0.1) +
#   geom_sankey_text(size = 3, vjust = 0.5, hjust = 1.5,
#                    color = "black", fontface = "bold") +
#   ggsci::scale_fill_aaas() +
#   theme_sankey(base_size = 18) +
#   labs(x = NULL) +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust = .5)) +
#   ggtitle("Car features")


###


p <- ggplot(data2, aes(x = x,
                      next_x= next_x,
                      node= node,
                      next_node= next_node,
                      fill= node,
                      label= node)) +
  geom_sankey(flow.fill="#DFDFDF",
              flow.color="grey60",
              node.fill=dittoColors()[1:length(unique(data2$node))],
              width=0.15) + 
  geom_sankey_text(size = 4,
                   color= 'black',
                   hjust=1) + 
  theme_void()


pdf(file=paste(prefix,'.pdf',sep=''))
print(p)
dev.off()

svg(filename=paste(prefix,'.svg',sep=''))
print(p)
dev.off()
ggsave(file=paste(prefix,'.png',sep=''),type='cairo-png',plot=p)



