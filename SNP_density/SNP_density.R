#!/TJPROJ6/RNA_SH/personal_dir/lizhengnan/mamba/envs/R_4.2/bin/Rscript
# https://github.com/YinLiLin/CMplot

#install.packages("CMplot")
library(CMplot)
library(tibble)
library(dplyr)
library(argparser)

argv <- arg_parser('绘制SNP在染色体上的密度图')
argv <- add_argument(argv,"--stat", help="the SNP analysis result, example:SNP/1.snpsite/*_SNP.xls")
argv <- add_argument(argv,"--prefix", help="the prefix of outfile")
argv <- parse_args(argv)

stat <- argv$stat
prefix <- argv$prefix


data <- read.table(stat, sep = '\t', header = T)%>%
  rownames_to_column(var = 'SNP')

CMplot(data,plot.type="d",bin.size=1e6,
       #chr.den.col=c("darkgreen", "yellow", "red"),
       chr.den.col=rev(rainbow(4)),
       file="jpg",file.name=prefix,dpi=300,
       chr.labels = unique(data$CHROM),chr.pos.max=T,
       main="SNP density",file.output=TRUE,verbose=TRUE,width=9,height=6)

CMplot(data,plot.type="d",bin.size=1e6,
       #chr.den.col=c("darkgreen", "yellow", "red"),
       chr.den.col=rev(rainbow(4)),
       file="pdf",file.name=prefix,dpi=300,
       chr.labels = unique(data$CHROM),chr.pos.max=T,
       main="SNP density",file.output=TRUE,verbose=TRUE,width=9,height=6)
