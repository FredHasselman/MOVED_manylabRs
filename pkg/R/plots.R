require(beeswarm)
library(ggplot2)
library(plyr)
library(dplyr)
require(reshape2)
require(lme4)
require(lmerTest)
require(rio)
require(lattice)

dir.in <- "~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/"
outlist1 <- rio::import(paste0(dir.in,"Data_Figures_Primary.rds"))
outlist2 <- rio::import(paste0(dir.in,"Data_Figures_Secondary.rds"))
outlistG <- rio::import(paste0(dir.in,"Data_Figures_Global.rds"))


srcDir <- "~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/"
setwd(srcDir)


require(devtools)
#srcDir <- "https://raw.githubusercontent.com/FredHasselman/ManyLabRs/master/manylabRs/R/"
srcDir <- "~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/manyLabRs/R/"

source(paste0(srcDir,"C-3PR_ASCII.R"))
source(paste0(srcDir,'getData.R'))
source(paste0(srcDir,'inIT.R'))
source(paste0(srcDir,'ML2_variable_functions.R'))
source(paste0(srcDir,'fRedsRutils.R'))

ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df

nmissing <- function(x) sum(is.na(x))

setwd("~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS")
outlist.tmp <- readRDS("ML2_results_primary.rds")
outlist.tmp <- outlist.tmp$aggregated
outlist1 <- ldply(outlist.tmp, function(d) if(!is.null(d)) cbind.data.frame(study.source   = d$study.source,
                                                                            test.statistic = d$test.statistic,
                                                                            test.p.value   = d$test.p.value,
                                                                            dplyr::select(d,starts_with("ESCI"))
                                                                            ))

idOk     <- complete.cases(outlist1$ESCI.N.total,outlist1$ESCI.r)
outlist1 <- outlist1[idOk,]
outlist1 <- outlist1[outlist1$.id%in%ML2.key$study.analysis[ML2.key$study.figure2.include==1],]


outlist.tmp <- readRDS("ML2_results_secondary.rds")
outlist.tmp <- outlist.tmp$aggregated
outlist2    <-  ldply(outlist.tmp, function(d) if(!is.null(d)) cbind.data.frame(study.source   = d$study.source,
                                                                                test.statistic = d$test.statistic,
                                                                                test.p.value   = d$test.p.value,
                                                                                dplyr::select(d,starts_with("ESCI"))
                                                                                ))
# id          <- which(colwise(nmissing)(outlist2) > nrow(outlist2)/2)
# outlist2    <- dplyr::select(outlist2,-id)

idOk     <- complete.cases(outlist2$ESCI.N.total,outlist2$ESCI.r)
outlist2 <- outlist2[idOk,]
#outlist2 <- outlist1[outlist2$.id%in%ML2.key$study.analysis[ML2.key$study.figure2.include==1],]

#outlist2 <- outlist2[outlist2$.id%in%ML2.key$study.analysis[ML2.key$study.figure2.include==1],]

outlist.tmp <- readRDS("ML2_results_global.rds")
outlist.tmp <- outlist.tmp$aggregated
outlistG <- ldply(outlist.tmp, function(d) if(!is.null(d)) cbind.data.frame(study.source   = d$study.source,
                                                                            test.statistic = d$test.statistic,
                                                                            test.p.value   = d$test.p.value,
                                                                            dplyr::select(d,starts_with("ESCI"))))
idOk     <- complete.cases(outlistG$ESCI.N.total,outlistG$ESCI.r)
outlistG <- outlistG[idOk,]

 # id          <- which(colwise(nmissing)(outlistG) > nrow(outlistG)/2)
 # outlistG    <- dplyr::select(outlistG,-id)

#sl=c(1,2)
#df <- rbind.data.frame(outlist1,outlist2) #dfo[(dfo$study.slate%in%sl)&!(is.na(dfo$test.r)),]

wd = 8
hg = 10

outlist1$GlobalES <- NA

l_ply(seq_along(outlist1$.id), function(l){
    outlist1$GlobalES[l]   <<- outlistG$ESCI.r[outlistG$.id==outlist1$.id[l]]
    outlist1$GlobalESlo[l] <<- outlistG$ESCI.l.r[outlistG$.id==outlist1$.id[l]]
    outlist1$GlobalEShi[l] <<- outlistG$ESCI.u.r[outlistG$.id==outlist1$.id[l]]
    })

# idZ <- which(outlist1$.id=="Zaval.3")
# idS <- which(outlist1$ESCI.r< -.5)
# idChange <- intersect(idZ,idS)
# outlist1$ESCI.r[idChange] <- -1*outlist1$ESCI.r[idChange]


setwd("~/Dropbox/Manylabs2/Figures")
#df <-outlist1[!(is.na(outlist1$ESCI.r)|is.na(outlist1$ESCI.r)),]
g1 <- get.SwarmPlot(outlist1, addLabel = FALSE, addOriES = TRUE)
g2 <- get.SwarmPlot(outlist2)
g3 <- get.SwarmPlot(outlistG)

ggsave(filename = paste0("ML2_SwarmPlot_primary_selection_130417.pdf"),
       plot = g1,
       scale = 3,
       width = wd,
       height = hg,
       units = "cm"
)

ggsave(filename = paste0("ML2_SwarmPlot_secondary.pdf"),
       plot = g2,
       scale = 3,
       width = wd,
       height = hg,
       units = "cm"
)

ggsave(filename = paste0("ML2_SwarmPlot_global.pdf"),
       plot = g3,
       scale = 3,
       width = wd,
       height = hg,
       units = "cm"
)



    #     df %>% group_by(.id) %>% select(ESCI.N.total)
    #
    # og <- outlistG #[outlistG$study.slate==sl,]
    #
    # bs2 <- ldply(unique(bs$labels),
    #              function(l) data.frame(x = bs$xn[bs$labels==l][1],
    #                                     meanES = bs$meanES[bs$labels==l][1],
    #                                     SEmean = bs$seES[bs$labels==l][1],
    #                                     globalES = og[which(paste(og$analysis.name)==l),'ESCI.ncp'],
    #                                     SEglobal = sd(bs$y[bs$labels==l]),
    #                                     labels = bs$labels[bs$labels==l][1],
    #                                     stringsAsFactors = FALSE))
    # bs2 <- bs2[order(bs2$meanES, decreasing = F), ]
    #
    # id <- laply(bs2$labels, function(x) grep(x, interceptES$study.source))
    #
    # bs2$globalES  <- laply(bs2$labels, function(l) og[which(paste(og$analysis.name)==l),'ESCI.ncp'])
    # bs2$modelES <- interceptES$modelES[id]
    # bs2$SEmodel <- interceptES$`Std. Error`[id]
    #
    # bs3     <- data.frame(labels = c(bs2$labels, og$labels),
    #                       x = c(bs2$x,bs2$x),
    #                       aggregateES=c(bs2$meanES, bs2$globalES),
    #                       SEmean = c(bs$seES, bs2$SEglobal))
    #
    # bs3$shp  <- c(rep(24,nrow(bs2)),rep(25,nrow(bs2)))
    # bs3$fill <- c(rep("ivory3",nrow(bs2)),rep("ivory2",nrow(bs2)))
    # #bs2$id       <- outlistG[outlistG$study.slate==sl,'.id'][id]

    #ggplot(bs2, aes(x=x, y=meanES, labels=labels)) + geom_point() + coord_flip()

    library(metafor)
    library(gplots)
    pdf("FunnelPerSlate.pdf", paper="a4r")


    dfo    <- outlist1
    dfo.s1 <- dfo[dfo$analysis.type%in%"Primary",]
    meta1 <- rma.mv(yi=ESCI.r, V=ESCI.var.r, struct = "CS", mods = ~ .id, random = ~ 1 | .id, slab = .id, data=dfo.s1, method="REML")

    textplot(capture.output(summary(meta1)))
    title(paste("Mixed model meta-analysis\nES (r) analysis as moderator and study source as random effect:\n",dfo.s1$study.slate[1]))
    funnel(meta1, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),refline=0)

    dfo.s2 <- outlist2[outlist2$analysis.type%in%"Secondary",]
    #dfo.sec               <- subset(dfo, study.slate == 2)
    dfo.s2$analysis.name <- relevel(drop(dfo.s2$analysis.name), ref = "zeroEffect")
    dfo.s2$study.source <- relevel(drop(dfo.s2$study.source), ref = "re-burp2")

    meta2 <- rma.mv(yi=test.r, V=test.sigma2, struct = "CS",
                    mods = ~ analysis.name, random = ~ 1 | study.source, slab = analysis.name, data=dfo.s2, method="REML")
    textplot(capture.output(summary(meta2)))

    title(paste("Mixed model meta-analysis\nES (r) analysis as moderator and study source as random effect:\n",dfo.s2$study.slate[1]))
    funnel(meta2, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),refline=0, main=paste0("Slate ",dfo.s2$study.slate[1]))

    xyplot(sqrt(test.sigma2) ~ test.r | analysis.name,
           groups = study.source,
           drop.unused.levels = TRUE,
           ylim = c(max(sqrt(dfo.s1$test.sigma2))+.1,-.05),
           key = list(text=list(lab = paste0(labels(unique(drop(dfo.s1$study.source))),".",unique(drop(dfo.s1$study.source)))),
                      columns=5, cex=.6),
           ylab = "Estimated SE of r",
           xlab = "Residual",
           main = "Slate 1 - funnel data per source",
           xlim = c(-1.1,1.1),
           data = dfo.s1,
           panel=function(x, y, groups, ...){
               panel.abline(v = 0, lty = 2)
               panel.xyplot(x, y, col = "grey",pch = 16, cex = .5)
               ltext(x=x, y=y, pos=1, offset=sign(rnorm(n=length(x))), cex=.4)
           })


    xyplot(sqrt(test.sigma2) ~ test.r | analysis.name,
           groups = study.source,
           drop.unused.levels = TRUE,
           ylim = c(max(sqrt(dfo.s1$test.sigma2))+.1,-.05),
           key = list(text=list(lab = paste0(labels(unique(drop(dfo.s2$study.source))),".",unique(drop(dfo.s2$study.source)))),
                      columns=5, cex=.6),
           ylab = "Estimated SE of r",
           xlab = "Residual",
           main = "Slate 2 - funnel data per source",
           xlim = c(-1.1,1.1),
           data = dfo.s2,
           panel=function(x, y, groups, ...){
               panel.abline(v = 0, lty = 2)
               panel.xyplot(x, y, col = "grey",pch = 16, cex = .5)
               ltext(x=x, y=y, pos=1, offset=sign(rnorm(n=length(x))), cex=.4)
           })


    dev.off()

