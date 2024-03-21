
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(INLA)
library(data.table)
library(gridExtra)
library(zoo)
library(ggplot2)
library(latex2exp)


# Load the results
load("ST_models_ICAR_partitioned.Rdata")
rm(TypeI.ICAR, TypeIII.ICAR, TypeIV.ICAR)

model <- TypeII.ICAR


# Load the data and cartography
load("../Data/IHD_data.Rdata")


S <- 3105
T <- 23

t.from <- 1999
t.to <- 2021



###########################################################
# Relative risks estimates: medians, 95% CI and P(rr>1|Y) #
###########################################################
RR.median <- as.data.frame(matrix(model$summary.fitted.values$`0.5quant`, S, T, byrow=F))
rownames(RR.median) <- model$.args$data$Area[1:S]
colnames(RR.median) <- seq(1999, 2021, 1)
RR.median$GEOID <- rownames(RR.median)
RR.median <- RR.median[, c(24, 1:23)]


RR.CIlow <- as.data.frame(matrix(model$summary.fitted.values$`0.025quant`, S, T, byrow=F))
rownames(RR.CIlow) <- model$.args$data$Area[1:S]
colnames(RR.CIlow) <- paste0("CIlow", seq(1999, 2021, 1))
RR.CIlow$GEOID <- rownames(RR.CIlow)
RR.CIlow <- RR.CIlow[, c(24, 1:23)]


RR.CIup <- as.data.frame(matrix(model$summary.fitted.values$`0.975quant`, S, T, byrow=F))
rownames(RR.CIup) <- model$.args$data$Area[1:S]
colnames(RR.CIup) <- paste0("CIup", seq(1999, 2021, 1))
RR.CIup$GEOID <- rownames(RR.CIup)
RR.CIup <- RR.CIup[, c(24, 1:23)]


probs <- as.data.frame(matrix(1-model$summary.linear.predictor$`0 cdf`, nrow=S, ncol=T, byrow=F))
rownames(probs) <- model$.args$data$Area[1:S]
colnames(probs) <- paste0("Probs", seq(1999, 2021, 1))
probs$GEOID <- rownames(probs)
probs <- probs[, c(24, 1:23)]



###############################################################################
# Select 2 most populated counties and 1 low populated county without missing # 
# counts in each region in the year 2021                                      #
###############################################################################
pop$num.NAs <- rowSums(is.na(counts))
pop$Region <- carto$Region



########
# West #
########
pop.west <- pop[pop$Region==1, c("GEOID", "2021", "num.NAs")]
pop.west <- pop.west[order(pop.west$`2021`, decreasing=TRUE), ]

pop.west.noNAs <- pop.west[pop.west$num.NAs==0, ]

print(pop.west.noNAs[c(1:2, nrow(pop.west.noNAs)), ])

# 06037="Los Angeles County - Los angeles", 04013="Maricopa County - Phoenix"
# 30105="Valley County - Montana"



###########
# Midwest #
###########
pop.midwest <- pop[pop$Region==2, c("GEOID", "2021", "num.NAs")]
pop.midwest <- pop.midwest[order(pop.midwest$`2021`, decreasing=TRUE), ]

pop.midwest.noNAs <- pop.midwest[pop.midwest$num.NAs==0, ]

print(pop.midwest.noNAs[c(1:2, nrow(pop.midwest.noNAs)), ])

# 17031="Cook county - Illinois", 26163="Wayne county - Michigan"
# 20141="Osborne County - Kansas"



#########
# South #
#########
pop.south <- pop[pop$Region==3, c("GEOID", "2021", "num.NAs")]
pop.south <- pop.south[order(pop.south$`2021`, decreasing=TRUE), ]

pop.south.noNAs <- pop.south[pop.south$num.NAs==0, ]

print(pop.south.noNAs[c(1:2, nrow(pop.south.noNAs)), ])

# 48201="Harris County - Huston", 12086="Miami-Dade County - Miami", 
# 21057="Cumberland County - Kentucky"



#############
# Northeast #
#############
pop.northeast <- pop[pop$Region==4, c("GEOID", "2021", "num.NAs")]
pop.northeast <- pop.northeast[order(pop.northeast$`2021`, decreasing=TRUE), ]

pop.northeast.noNAs <- pop.northeast[pop.northeast$num.NAs==0, ]

print(pop.northeast.noNAs[c(1:2, nrow(pop.northeast.noNAs)), ])

# 36047="Kings County - New York (Brooklin)", 36081="Queens County - New York (Queens)"
# 42057="Fulton County - Pennsylvania"



###########################################################################
# Figure 5: temporal trends of the relative risks in each selected county # 
###########################################################################
IDs <- c("06037", "04013", "30105", 
         "17031", "26163", "20141",
         "48201", "12086", "21057",
         "36047", "36081", "42057")

names <- c("Los Angeles county (California)", "Maricopa county (Arizona)", "Valley county (Montana)",
           "Cook county (Illinois)", "Wayne county (Michigan)", "Osborne county (Kansas)",
           "Harris county (Texas)", "Miami-Dade county (Florida)", "Cumberland county (Kentucky)",
           "Kings county (New York)", "Queens county (New York)", "Fulton county (Pennsylvania)")



color <- c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027")


minimum <- min(RR.CIlow[IDs, -1])
maximum <- max(RR.CIup[IDs, -1])



for (i in 1:length(IDs)){
  id <- IDs[i]
  print(id)
  
  fitted.val <- data.frame(County=rep(id, T),
                           County.name=rep(names[i], T),
                           r_it=unlist(RR.median[id, -1]),
                           Year=seq(t.from, t.to, 1),
                           q1=unlist(RR.CIlow[id, -1]),
                           q2=unlist(RR.CIup[id, -1]),
                           PRP_it=Hmisc::cut2(unlist(probs[id, -1]), c(0,0.1,0.2,0.8,0.9,1)))
  
  fitted.val$group <- rleid(fitted.val$PRP_it)  
  fitted.val$PRP_it2 <- as.character(fitted.val$PRP_it)
  
  fitted.val.plot <- head(do.call(rbind, by(fitted.val, fitted.val$group, rbind, NA)), -1)
  fitted.val.plot[,c("group","PRP_it2")] <- lapply(fitted.val.plot[,c("group","PRP_it2")], na.locf)
  fitted.val.plot[] <- lapply(fitted.val.plot, na.locf, fromLast = TRUE)
  
  assign(paste0("fitted.val.plot.", i), fitted.val.plot)
  fitted.val.plot <- NULL
}


prob.levels <- Hmisc::cut2(unlist(c(0.05,0.15,0.5,0.85,0.95)), c(0,0.1,0.2,0.8,0.9,1))


p1 <- ggplot(fitted.val.plot.1, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p2 <- ggplot(fitted.val.plot.2, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p3 <- ggplot(fitted.val.plot.3, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p4 <- ggplot(fitted.val.plot.4, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p5 <- ggplot(fitted.val.plot.5, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p6 <- ggplot(fitted.val.plot.6, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p7 <- ggplot(fitted.val.plot.7, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p8 <- ggplot(fitted.val.plot.8, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p9 <- ggplot(fitted.val.plot.9, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p10 <- ggplot(fitted.val.plot.10, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p11 <- ggplot(fitted.val.plot.11, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)

p12 <- ggplot(fitted.val.plot.12, aes(x=Year, y=r_it)) + 
  facet_wrap(. ~ County.name, ncol=3, scales="free") +
  geom_line(size=1, color="black") +
  geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
  scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                    breaks=prob.levels) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue"),
        legend.position = "none") + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(1,5,1), limits=c(0.4,5.5)) +
  xlab("Year") + 
  ylab(expression(r[it])) + labs(colour = NULL)


pdf("Figure5.pdf", height=18, width=15)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, nrow=4, ncol=3) 
dev.off()


