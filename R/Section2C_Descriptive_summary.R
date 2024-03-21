
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(sf)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(reshape2) 



# Load the data with missing counts
load("../Data/IHD_data.Rdata")


# Counties with NA counts in all the period of study
x <- rowSums(is.na(counts))
Tab <- data.frame(GEOID=counts$GEOID, X=x)
ID.NAs <- Tab$GEOID[Tab$X==23]

rm(counts, pop, carto)


# Load the data after imputing the missing counts
load("../Imputed_data/IHD_imputed_data.Rdata")


T <- ncol(counts)-1
t.from <- 1999
t.to <- 2021



###########################
# Figure 2: SMR for space #
###########################
O.total <- rowSums(counts[, -1], na.rm=TRUE)
Exp.total <- rowSums(exp[, -1], na.rm=TRUE)


SMR.spat <- data.frame(GEOID=counts$GEOID, SMR=O.total/Exp.total)
SMR.spat$SMR[SMR.spat$GEOID %in% ID.NAs] <- NA


carto.SMR <- merge(carto, SMR.spat, by="GEOID")
summary(carto.SMR$SMR)


# Cartography for the states
carto.states <- aggregate(carto[,"geometry"], by=list(ID.group=carto$STATEFP), head)


# Cartography of the four regions
carto.reg <- aggregate(carto[,"geometry"], by=list(ID.group=carto$Region), head)



paleta <- brewer.pal(9,"YlOrRd")
values <- c(0,0.5,0.75,0.9,1,1.1,1.25,1.5,2,3.38)


plot.SMR.spat <- tm_shape(carto.SMR) +
  tm_polygons(col=c("SMR"), palette=paleta, border.alpha=1, title="",
              leyend.show=T, legend.reverse=T, style="fixed", breaks=values, interval.closure="left",
              border.col = "black",
              colorNA = "#bdbdbd", textNA = "Insufficient data") +
  tm_layout(main.title="", main.title.position="center", legend.text.size=0.9,
            panel.labels="SMR", panel.label.bg.color="lightskyblue",
            legend.position = c("right", "bottom"), legend.frame=F,
            panel.label.size=2, panel.label.height=1, outer.margins=0) +
  tm_facets(ncol=1, nrow=1) + 
  tm_shape(carto.states) + tm_borders(col="white", lwd=1) + 
  tm_shape(carto.reg) + tm_borders(col="#c2a5cf", lwd=3)
print(plot.SMR.spat)
tmap_save(plot.SMR.spat, width=9, height=7, file="Figure2a.pdf")



##########################
# Figure 2: SMR for time #
##########################
O.total <- colSums(counts[, -1], na.rm=TRUE)
Exp.total <- colSums(exp[, -1], na.rm=TRUE)


SMR.temp <- data.frame(year=seq(1999,2021,1), SMR=O.total/Exp.total, 
                       standard.error=sqrt(O.total)/Exp.total)

SMR.temp$CIlow <- SMR.temp$SMR-qnorm(0.975)*SMR.temp$standard.error
SMR.temp$CIup <- SMR.temp$SMR+qnorm(0.975)*SMR.temp$standard.error



pdf("Figure2b.pdf", width=8, height=7)
ggplot(SMR.temp, aes(x=year, y=SMR)) + 
  geom_line(size=0.2) +
  geom_ribbon(aes(ymin=CIlow, ymax=CIup),
              alpha=0.2) +
  scale_color_manual(values="grey") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines")) + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(0.8,1.4,0.1), limits=c(0.8,1.45)) +
  xlab("Year") + ylab("SMR") + labs(colour = NULL)
dev.off()



#########################################
# Compute SMRs for Table 1 and Figure 3 #
#########################################
SMR <- data.frame(GEOID=counts$GEOID)

for(i in 2:(T+1)){
  SMR <- cbind(SMR, counts[,i]/exp[,i])
  colnames(SMR)[i] <- 1999+i-2
}

rownames(SMR) <- SMR$GEOID



###########
# Table 1 #
###########
descriptive <- function(x, t){
  O <- counts[x, t]
  E <- exp[x, t]
  data.frame(Year=t, O=O, E=E, SMR=O/E,
             var.SMR=O/E^2,
             CV.SMR=sqrt(O/E^2)/(O/E))
}


# 1999
counts$GEOID[counts$`1999`==max(counts$`1999`)]  # GEOID="06037"
counts$GEOID[counts$`1999`==1]                   # GEOID="16025"


# 2010
counts$GEOID[counts$`2010`==max(counts$`2010`)]  # GEOID="06037"
counts$GEOID[counts$`2010`==1]                   # GEOID="16025"


# 2021
counts$GEOID[counts$`2021`==max(counts$`2021`)]  # GEOID="06037"
counts$GEOID[counts$`2021`==1]                   # GEOID="16025"


Table1.1999 <- rbind(descriptive(x="16025", t="1999"),
                     descriptive(x="06037", t="1999"))
rownames(Table1.1999) <- c("Min", "Max")


Table1.2010 <- rbind(descriptive(x="16025", t="2010"),
                     descriptive(x="06037", t="2010"))
rownames(Table1.2010) <- c("Min", "Max")


Table1.2021 <- rbind(descriptive(x="16025", t="2021"),
                     descriptive(x="06037", t="2021"))
rownames(Table1.2021) <- c("Min", "Max")



Table1 <- rbind(Table1.1999, Table1.2010, Table1.2021)
Table1[, 2:6]<- round(Table1[, 2:6], 4)
print(Table1)



############
# Figure 3 #
############
SMR.new <- melt(SMR, id.var="GEOID", value.name="SMR")


pdf(file="Figure3.pdf", width=14, height=8)
p <- ggplot(SMR.new, aes(x=variable , y=SMR)) + 
  geom_boxplot(position = position_dodge(0.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines")) + 
  xlab("") + ylab('SMR')  +
  scale_y_continuous(limits=c(0,7), breaks=c(0,1,2,4,6))
p + geom_boxplot(fill='#74c476', color="black")
dev.off()



