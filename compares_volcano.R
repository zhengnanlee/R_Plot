#!/TJPROJ2/GB/PUBLIC/software/GB_TR/mRNA/miniconda3/envs/r_3.5.0/bin/Rscript

suppressMessages({
  library(stringr)
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
  #install.packages("ggnewscale")
  library(ggnewscale)
  library(argparser)})

argv <- arg_parser('绘制多组差异火山图')
argv <- add_argument(argv,"--diffdir", help="the differential analysis results dir")
argv <- add_argument(argv,"--compares", help="the compare names")
argv <- add_argument(argv,"--fc", help="the foldchange vlaue")
argv <- add_argument(argv,"--pvalue", help="the p value")
argv <- add_argument(argv,"--padj", help="the p adjust value")
argv <- add_argument(argv,"--outdir", help="the output dir")
argv <- parse_args(argv)

diffdir <- argv$diffdir
compares <- argv$compares
fc <- argv$fc
p <- argv$pvalue
q <- argv$padj
outdir <- argv$outdir

if (!is.na(fc)){fc <-as.numeric(fc)}
if (!is.na(p)){p <-as.numeric(p)}
if (!is.na(q)){q <-as.numeric(q)}

compare_names <- unlist(str_split(compares, pattern = ','))
compare_names <- unlist(str_split(compares, pattern = ','))

volcano_data <- data.frame()
y_max <- c()
for (compare in compare_names){
  deg_file <- paste(diffdir,"/",compare,"/",compare,"_deg.xls", sep = '')
  deg <- read.table(deg_file, sep = '\t', header = T, quote = '',fill = T)
  tmp_volcano_data <- select(deg, gene_id, log2FoldChange, pvalue, padj)
  tmp_volcano_data$compare <- compare
  volcano_data <- rbind(volcano_data, tmp_volcano_data)
  y_max <- c(y_max, max(abs(deg$log2FoldChange)))
}

volcano_data$diff <- 'no_diff'
if (is.na(p)) {
  volcano_data$diff[abs(volcano_data$log2FoldChange) >=log2(fc) & volcano_data$padj <=0.05] <- 'diff'
}
if (is.na(q)){
  volcano_data$diff[abs(volcano_data$log2FoldChange) >=log2(fc) & volcano_data$pvalue <=0.05] <- 'diff'
}

data2 <- data.frame(x = compare_names,
                    y=0,
                    label = compare_names)

datbar<-data.frame(x=compare_names,
                   y=y_max)
if (length(compare_names) <= 12){
  color = brewer.pal(length(compare_names), "Set3")
} else {
  color = rainbow(length(compare_names))
}

P <- ggplot()+
  geom_col(data=datbar,aes(x=x,y=y),fill="grey",alpha=0.5)+
  geom_col(data=datbar,aes(x=x,y=-y),fill="grey",alpha=0.5)+
  geom_jitter(data=volcano_data,
              aes(x=compare,y=log2FoldChange,
                  color=diff))+
  scale_color_manual(name=NULL,
                     values = c("red","darkgrey"))+
  ggnewscale::new_scale_fill()+
  theme_minimal()+
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.justification = c(1,0),
        legend.direction = "vertical",
        axis.text.x = element_blank())+
  labs(x="Compare",y="log2FoldChange")+
  geom_tile(data=data2,
            aes(x=x,y=y,fill=x),
            height=3,color="black",
            alpha=0.9,
            show.legend = F)+
  scale_fill_manual(values = color, "Paired")+
  geom_text(data=data2,aes(x=x,y=y,label=label))
  
pdf(file=paste(outdir,'compares_volcano.pdf', sep = '/'))
P
dev.off()
svg(filename=paste(outdir,'compares_volcano.svg', sep = '/'))
P
dev.off()
ggsave(file=paste(outdir,'compares_volcano.pdf', sep = '/'),type='cairo-png',plot=P,
       device ='png', width =7, height = 7,units = 'in',dpi = 300,
       bg='white')
