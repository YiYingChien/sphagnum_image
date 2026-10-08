#at server /home/yee/sphagnum_area
###top
top_negative_data<-read.table("Top_negative_analysis.csv",sep=',')
top_data<-read.table("Top_analysis.csv",sep=',')
colnames(top_negative_data)<-c("day1name","day1value","day1log","day1serial","day56name","day56value","day56log","day56serial","ratio","log_ratio")
colnames(top_data)<-c("day1name","day1value","day1log","day1serial","day56name","day56value","day56log","day56serial","ratio","log_ratio")

top_negative_data["type"]<- unlist(lapply(strsplit(top_negative_data[,1],split="_"),`[`,3))
top_data["type"]<- unlist(lapply(strsplit(top_data[,1],split="_"),`[`,3))
 
area_plot_table<-rbind(top_negative_data,top_data)
area_plot_table[grep("^n-",area_plot_table[,"day1serial"]),"type"]<-"control_mod"
area_plot_table[grep("^nn-",area_plot_table[,"day1serial"]),"type"]<-"control"
 
area_plot_table["TukeyHSD"]<-lapply(area_plot_table["type"],function(x) gsub("GC2405-9","GC2405_9",x))
 
area_plot_table<-area_plot_table[-c(setdiff(which(area_plot_table[,"day56value"]==0),grep("162",area_plot_table[,"day56serial"])),grep("^n-",area_plot_table[,"day56serial"])),]
 
area_plot_table[which(area_plot_table["log_ratio"]=="-Inf"),"log_ratio"]<-log(0.00001)/log(10)
area_plot_table[which(area_plot_table["day56log"]=="-Inf"),"day56log"]<-0.00001
 
##group
library(multcompView)
area_group<-multcompLetters4(aov(ratio~TukeyHSD,data=area_plot_table),TukeyHSD(aov(ratio~TukeyHSD,data=area_plot_table)))$TukeyHSD$Letters
area_log_group<-multcompLetters4(aov(log_ratio~TukeyHSD,data=area_plot_table),TukeyHSD(aov(log_ratio~TukeyHSD,data=area_plot_table)))$TukeyHSD$Letters

##label location 
for(g in 1:length(area_group)){
    area_plot_table[which(area_plot_table[,"TukeyHSD"]==names(area_group)[g]),"area_group"]<- area_group[g]
    area_plot_table[which(area_plot_table[,"TukeyHSD"]==names(area_group)[g]),"text_y"]<- quantile(area_plot_table[which(area_plot_table[,"TukeyHSD"]==names(area_group)[g]),"ratio"],probs = 0.75)
}
 
area_plot_table$type<-factor(area_plot_table$type,levels=c("control","BC342","BC215","BC130","BC099","GC2405-9","BC237","BC360","BC312","BC166","BC162"))
 
library(ggplot2)
ggplot(area_plot_table,aes(x=type,y=ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_area_ratio.pdf",width=12,height=12)

##label location 
for(g in 1:length(area_log_group)){
    area_plot_table[which(area_plot_table[,"TukeyHSD"]==names(area_group)[g]),"area_log_group"]<- area_log_group[g]
    area_plot_table[which(area_plot_table[,"TukeyHSD"]==names(area_log_group)[g]),"log_text_y"]<- quantile(area_plot_table[which(area_plot_table[,"TukeyHSD"]==names(area_log_group)[g]),"log_ratio"],probs = 0.75)
}
 
library(ggbreak)
ggplot(area_plot_table,aes(x=type,y=log_ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = area_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0,-4.5))
ggsave("sphagnum_area_log_ratio.pdf",width=12,height=12)
###

###side
##four side
side_negative_data<-read.table("side_negative_analysis.csv",sep=',')
side_data<-read.table("side_analysis.csv",sep=',')
colnames(side_negative_data)<-c("day1name","day1value","day1log","day56name","day56value","day56log","type","side","ratio","log_ratio")
colnames(side_data)<- c("day1name","day1value","day1log","day56name","day56value","day56log","type","side","ratio","log_ratio")
 
side_area_plot_table<-rbind(side_negative_data,side_data)
side_area_plot_table[which(side_area_plot_table[,"type"]=="negative_control_new"),"type"]<-"control"
side_area_plot_table["TukeyHSD"]<-lapply(side_area_plot_table["type"],function(x) gsub("GC2405-9","GC2405_9",x))
 
side_area_plot_table<-side_area_plot_table[-c(setdiff(which(side_area_plot_table[,"day56value"]==0),grep("162",side_area_plot_table[,"day1name"])), which(side_area_plot_table[,"type"]=="negative_control_mod")),]
side_area_plot_table[which(side_area_plot_table["log_ratio"]=="-Inf"),"log_ratio"]<-log(0.00001)/log(10)
side_area_plot_table[which(side_area_plot_table["day56log"]== "-Inf"),"day56log"]<-0.00001

##group 
side_area_group<-multcompLetters4(aov(ratio~TukeyHSD,data=side_area_plot_table),TukeyHSD(aov(ratio~TukeyHSD,data=side_area_plot_table)))$TukeyHSD$Letters
side_area_log_group<-multcompLetters4(aov(log_ratio~TukeyHSD,data=side_area_plot_table),TukeyHSD(aov(log_ratio~TukeyHSD,data=side_area_plot_table)))$TukeyHSD$Letters
 
##label location 
for(g in 1:length(side_area_group)){
side_area_plot_table[which(side_area_plot_table[,"TukeyHSD"]==names(side_area_group)[g]),"side_area_group"]<- side_area_group[g]
    side_area_plot_table[which(side_area_plot_table[,"TukeyHSD"]==names(side_area_group)[g]),"text_y"]<- quantile(side_area_plot_table[which(side_area_plot_table[,"TukeyHSD"]==names(side_area_group)[g]),"ratio"],probs = 0.75)
}
side_area_plot_table$type<-factor(side_area_plot_table$type,levels=c("control","BC342","BC215","BC130","BC099","GC2405-9","BC237","BC360","BC312","BC166","BC162"))
 
 
ggplot(side_area_plot_table,aes(x=type,y=ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = side_area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_4side_area_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side1"),],aes(x=type,y=ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = side_area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_side1_area_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side2"),],aes(x=type,y=ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = side_area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_side2_area_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side3"),],aes(x=type,y=ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = side_area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_side3_area_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side4"),],aes(x=type,y=ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = side_area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_side4_area_ratio.pdf",width=12,height=12)

##label location
for(g in 1:length(side_area_log_group)){
side_area_plot_table[which(side_area_plot_table[,"TukeyHSD"]==names(side_area_log_group)[g]),"side_area_log_group"]<- side_area_log_group[g]
side_area_plot_table[which(side_area_plot_table[,"TukeyHSD"]==names(side_area_log_group)[g]),"log_text_y"]<- quantile(side_area_plot_table[which(side_area_plot_table[,"TukeyHSD"]==names(side_area_log_group)[g]),"log_ratio"],probs = 0.75)
}
 
ggplot(side_area_plot_table,aes(x=type,y=log_ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = side_area_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0.9,-4.9),scale=10)
ggsave("sphagnum_4side_area_log_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side1"),],aes(x=type,y=log_ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = side_area_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0.9,-4.9),scale=10)
ggsave("sphagnum_side1_area_log_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side2"),],aes(x=type,y=log_ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = side_area_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0.9,-4.9),scale=10)
ggsave("sphagnum_side2_area_log_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side3"),],aes(x=type,y=log_ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = side_area_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0.9,-4.9),scale=10)
ggsave("sphagnum_side3_area_log_ratio.pdf",width=12,height=12)
 
ggplot(side_area_plot_table[which(side_area_plot_table[,"side"]=="side4"),],aes(x=type,y=log_ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = side_area_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0.9,-4.9),scale=10)
ggsave("sphagnum_side4_area_log_ratio.pdf",width=12,height=12)
##four side
 
##ratio then average (day56/day1)/4
side_average_negative_data<-read.table("side_average_negative_analysis.csv",sep=',')
side_average_data<-read.table("side_average_analysis.csv",sep=',')
colnames(side_average_negative_data)<-c("name","type","side1","side2","side3","side4","log_side1","log_side2","log_side3","log_side4","ratio_average","log_ratio_average")
colnames(side_average_data)<- c("name","type","side1","side2","side3","side4","log_side1","log_side2","log_side3","log_side4","ratio_average","log_ratio_average")
 
side_average_area_plot_table<-rbind(side_average_negative_data,side_average_data)
side_average_area_plot_table[which(side_average_area_plot_table[,"type"]=="negative_control_new"),"type"]<-"control"
side_average_area_plot_table["TukeyHSD"]<-lapply(side_average_area_plot_table["type"],function(x) gsub("GC2405-9","GC2405_9",x))
 
side_average_area_plot_table<-side_average_area_plot_table[-c(setdiff(which(side_average_area_plot_table[,"side1"]==0),grep("162",side_average_area_plot_table[,"type"])), which(side_average_area_plot_table[,"type"]=="negative_control_mod")),]
side_average_area_plot_table[which(side_average_area_plot_table["log_ratio_average"]== "-Inf"),"log_ratio_average"]<- log(0.00001)/log(10)side_average_area_plot_table[which(side_average_area_plot_table["log_side1"]== "-Inf"),"log_ratio_average"]<-log(0.00001)/log(10)
side_average_area_plot_table[which(side_average_area_plot_table["log_side2"]== "-Inf"),"log_ratio_average"]<- log(0.00001)/log(10)
side_average_area_plot_table[which(side_average_area_plot_table["log_side3"]== "-Inf"),"log_ratio_average"]<- log(0.00001)/log(10)
side_average_area_plot_table[which(side_average_area_plot_table["log_side4"]== "-Inf"),"log_ratio_average"]<- log(0.00001)/log(10)

##group 
side_average_area_group<-multcompLetters4(aov(ratio_average~TukeyHSD,data=side_average_area_plot_table),TukeyHSD(aov(ratio_average~TukeyHSD,data=side_average_area_plot_table)))$TukeyHSD$Letters
side_average_area_log_group<-multcompLetters4(aov(log_ratio_average~TukeyHSD,data=side_average_area_plot_table),TukeyHSD(aov(log_ratio_average~TukeyHSD,data=side_average_area_plot_table)))$TukeyHSD$Letters 
 
##label location
for(g in 1:length(side_average_area_group)){
side_average_area_plot_table[which(side_average_area_plot_table[,"TukeyHSD"]==names(side_average_area_group)[g]),"side_average_area_group"]<- side_average_area_group[g]
side_average_area_plot_table[which(side_average_area_plot_table[,"TukeyHSD"]==names(side_average_area_group)[g]),"text_y"]<- quantile(side_average_area_plot_table[which(side_average_area_plot_table[,"TukeyHSD"]==names(side_average_area_group)[g]),"ratio_average"],probs = 0.75)
}
side_average_area_plot_table$type<-factor(side_average_area_plot_table$type,levels=c("control","BC342","BC215","BC130","BC099","GC2405-9","BC237","BC360","BC312","BC166","BC162"))
 
 
ggplot(side_average_area_plot_table,aes(x=type,y=ratio_average))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = side_average_area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_side_area_ratio_average.pdf",width=12,height=12)

##label location 
for(g in 1:length(side_average_area_log_group)){
side_average_area_plot_table[which(side_average_area_plot_table[,"TukeyHSD"]==names(side_average_area_log_group)[g]),"side_average_area_log_group"]<- side_average_area_log_group[g]
side_average_area_plot_table[which(side_average_area_plot_table[,"TukeyHSD"]==names(side_average_area_log_group)[g]),"log_text_y"]<- quantile(side_average_area_plot_table[which(side_average_area_plot_table[,"TukeyHSD"]==names(side_average_area_log_group)[g]),"log_ratio_average"],probs = 0.75)
}
 
ggplot(side_average_area_plot_table,aes(x=type,y=log_ratio_average))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = side_average_area_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0.9,-4.9),scale=10)
 
ggsave("sphagnum_side_area_log_ratio_average.pdf",width=12,height=12)
 
##sum then average sum(day56)/sum(day1)
side_ratio_negative_data<-read.table("side_ratio_negative_analysis.csv",sep=',')
side_ratio_data<-read.table("side_ratio_analysis.csv",sep=',')
colnames(side_ratio_negative_data)<-c("name","type","day1average","day56average","ratio","day1log","day56log","log_ratio")
colnames(side_ratio_data)<- c("name","type","day1average","day56average","ratio","day1log","day56log","log_ratio")
 
side_area_ratio_plot_table<-rbind(side_ratio_negative_data,side_ratio_data)
side_area_ratio_plot_table[which(side_area_ratio_plot_table[,"type"]=="negative_control_new"),"type"]<-"control"
side_area_ratio_plot_table["TukeyHSD"]<-lapply(side_area_ratio_plot_table["type"],function(x) gsub("GC2405-9","GC2405_9",x))
 
side_area_ratio_plot_table<-side_area_ratio_plot_table[-c(setdiff(which(side_area_ratio_plot_table[,"day56average"]==0),grep("162",side_area_ratio_plot_table[,"type"])), which(side_area_ratio_plot_table[,"type"]=="negative_control_mod")),]
side_area_ratio_plot_table[which(side_area_ratio_plot_table["log_ratio"]== "-Inf"),"log_ratio"]<-log(0.00001)/log(10)
 
##group
side_area_ratio_group<-multcompLetters4(aov(ratio~TukeyHSD,data=side_area_ratio_plot_table),TukeyHSD(aov(ratio~TukeyHSD,data=side_area_ratio_plot_table)))$TukeyHSD$Letters
side_area_ratio_log_group<-multcompLetters4(aov(log_ratio~TukeyHSD,data=side_area_ratio_plot_table),TukeyHSD(aov(log_ratio~TukeyHSD,data=side_area_ratio_plot_table)))$TukeyHSD$Letters

#label location 
for(g in 1:length(side_area_ratio_group)){
side_area_ratio_plot_table[which(side_area_ratio_plot_table[,"TukeyHSD"]==names(side_area_ratio_group)[g]),"side_area_group"]<- side_area_ratio_group[g]
side_area_ratio_plot_table[which(side_area_ratio_plot_table[,"TukeyHSD"]==names(side_area_ratio_group)[g]),"text_y"]<- quantile(side_area_plot_table[which(side_area_plot_table[,"TukeyHSD"]==names(side_area_ratio_group)[g]),"ratio"],probs = 0.75)
}
side_area_ratio_plot_table$type<-factor(side_area_ratio_plot_table$type,levels=c("control","BC342","BC215","BC130","BC099","GC2405-9","BC237","BC360","BC312","BC166","BC162"))
 
ggplot(side_area_ratio_plot_table,aes(x=type,y=ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio")+labs(fill="type")+ geom_text(aes(x = type, y = text_y, label = side_area_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_continuous(breaks = seq(0, 6, 1))
ggsave("sphagnum_side_area_ratio.pdf",width=12,height=12)
 
##label location
for(g in 1:length(side_area_ratio_log_group)){
side_area_ratio_plot_table[which(side_area_ratio_plot_table[,"TukeyHSD"]==names(side_area_ratio_log_group)[g]),"side_area_ratio_log_group"]<- side_area_ratio_log_group[g]
side_area_ratio_plot_table[which(side_area_ratio_plot_table[,"TukeyHSD"]==names(side_area_ratio_log_group)[g]),"log_text_y"]<- quantile(side_area_ratio_plot_table[which(side_area_ratio_plot_table[,"TukeyHSD"]==names(side_area_ratio_log_group)[g]),"log_ratio"],probs = 0.75)
}
 
ggplot(side_area_ratio_plot_table,aes(x=type,y=log_ratio))+geom_boxplot(aes(fill=type))+ scale_fill_brewer(palette = "Paired")+theme_light()+theme_bw(base_size=15)+theme(axis.text.x=element_text(size=20,angle = 90),axis.text.y=element_text(size=20),axis.title = element_text(size = 40),legend.text = element_text(size=25),legend.title = element_text(size=30),legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),strip.text = element_text(size = 35,face="italic"))+ xlab(label=NULL)+ylab(label="area ratio (log10 value)")+labs(fill="type")+ geom_text(aes(x = type, y = log_text_y, label = side_area_ratio_log_group), size = 8, vjust=-0.1, hjust =-0.1,show.legend = FALSE)+geom_hline(yintercept=1)+scale_y_break(c(0.9,-4.9))
ggsave("sphagnum_side_area_log_ratio.pdf",width=12,height=12)
###
