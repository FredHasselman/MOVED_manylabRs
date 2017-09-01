require(plyr)
require(dplyr)
require(knitr)
require(rmarkdown)

#setwd("~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS")
outlist.tmp <- readRDS("~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/ML2_results_primary.rds")
outlist.tmp <- outlist.tmp$aggregated
outlist1 <- ldply(outlist.tmp, function(d) if(!is.null(d)) cbind.data.frame(study.source   = d$study.source,
                                                                            test.statistic = d$test.statistic,
                                                                            test.p.value   = d$test.p.value,
                                                                            dplyr::select(d,starts_with("ESCI"))
))

idOk     <- complete.cases(outlist1$ESCI.N.total,outlist1$ESCI.r)
outlist1 <- outlist1[idOk,]
outlist1 <- group_by(outlist1,.id)
outlist1 <- mutate(outlist1, rMed = median(ESCI.r, na.rm = TRUE))
outlist1 <- arrange(outlist1,desc(rMed))
saveRDS(outlist1,"sparklist1.rds")
#
#
# x <- outlist1$test.p.value
# x.c <- cut(x, breaks=c(1e-07, 5e-07, 1e-06, 5e-06, 1e-05, 5e-05, 1e-04, 5e-04, 1e-03, 5e-03, 1e-02, 5e-02, 1e-01, 1e+00))
# barplot(table(x.c), cex.names = .6)
#
# plot(density(outlist1$test.p.value,from=0,to=.1))
# require(lattice)
# plot(outlist1$ESCI.l.r,log(outlist1$test.p.value),pch=".",ylim=c(-100,0))
# axis(2, at = log(c(1e-07, 5e-07, 1e-06, 5e-06, 1e-05, 5e-05, 1e-04, 5e-04, 1e-03, 5e-03, 1e-02, 5e-02, 1e-01, 1e+00)), labels = c(1e-07, 5e-07, 1e-06, 5e-06, 1e-05, 5e-05, 1e-04, 5e-04, 1e-03, 5e-03, 1e-02, 5e-02, 1e-01, 1e+00)))
# abline(h=log(0.05))


outlist.tmp <- readRDS("~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/ML2_results_secondary.rds")
outlist.tmp <- outlist.tmp$aggregated
outlist2    <-  ldply(outlist.tmp, function(d) if(!is.null(d)) cbind.data.frame(study.source   = d$study.source,
                                                                                test.statistic = d$test.statistic,
                                                                                test.p.value   = d$test.p.value,
                                                                                dplyr::select(d,starts_with("ESCI"))
))

idOk     <- complete.cases(outlist2$ESCI.N.total,outlist2$ESCI.r)
outlist2 <- outlist2[idOk,]
outlist2 <- group_by(outlist2,.id)
outlist2 <- mutate(outlist2, rMed = median(ESCI.r, na.rm = TRUE))
outlist2 <- arrange(outlist2,desc(rMed))
saveRDS(outlist2,"sparklist2.rds")


outlist.tmp <- readRDS("~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/ML2_results_global.rds")
outlist.tmp <- outlist.tmp$aggregated
outlistG <- ldply(outlist.tmp, function(d) if(!is.null(d)) cbind.data.frame(study.source   = d$study.source,
                                                                            test.statistic = d$test.statistic,
                                                                            test.p.value   = d$test.p.value,
                                                                            dplyr::select(d,starts_with("ESCI"))))
idOk     <- complete.cases(outlistG$ESCI.N.total,outlistG$ESCI.r)
outlistG <- outlistG[idOk,]
#xx<-density(outlist1$test.p.value$x

hdr0 <- list()
yamlSep <- "---"
hdr0[1] <- yamlSep
hdr0[2] <- "output:"
hdr0[3] <- "  html_document:"
hdr0[4] <- "    theme: spacelab"
hdr0[5] <- yamlSep

hdr1 <- "| Source | Distribution of Effect Size r | Global Effect Size r | Density of log(p-value) |"
hdr2 <- "|-------:|:-----------------------------:|:--------------------:|:------------------------:|"

txt  <- laply(unique(outlist1$.id), function(s){
  paste0("|",s,"| `r sparkline(sort(outlist1$ESCI.r[outlist1$.id=='",s,"']), type = 'box', minValue=-1,maxValue=1,width='300px', chartRangeMin=-1,chartRangeMax=1)`|",
         round(outlistG$ESCI.r[outlistG$.id==s],digits=2)," [",round(outlistG$ESCI.l.r[outlistG$.id==s],digits=2),",",round(outlistG$ESCI.u.r[outlistG$.id==s],digits=2),
         "] | `r sparkline(sort(log(outlist1$test.p.value[outlist1$.id=='",s,"'])), normalRangeMin = log(0.05), normalRangeMax = log(1), normalRangeColor = '#ff5656', drawNormalOnTop = true, chartRangeMin=log1p(0),chartRangeMax=log(1), type = 'line')` |")})
setup <- "`r library(htmlwidgets);library(sparkline);outlist1<-readRDS('sparklist1.rds')`"

#`r sparkline(density(outlist1$test.p.value[outlist1$.id=='",s,"'])$y
outFile <- file("spark1.Rmd","w")
l_ply(hdr0,writeLines,con=outFile)
writeLines(setup,outFile)
writeLines(hdr1,outFile)
writeLines(hdr2,outFile)
writeLines(txt,outFile)
close(outFile)

rmarkdown::render(input = "spark1.Rmd",
                  output_format = "html_document",
                  output_file = "ML2_primary.html")

rm(txt,outFile,setup)


outlist2 <- filter(outlist2,.id!='Savani.2')
setup <- "`r library(htmlwidgets);library(sparkline);outlist2<-readRDS('sparklist2.rds')`"

txt  <- laply(unique(outlist2$.id), function(s){
  paste0("|",s,"| `r sparkline(sort(outlist2$ESCI.r[outlist2$.id=='",s,"']), type = 'box', minValue=-1,maxValue=1,width='300px', chartRangeMin=-1,chartRangeMax=1)`|",
         round(outlistG$ESCI.r[outlistG$.id==s],digits=2)," [",round(outlistG$ESCI.l.r[outlistG$.id==s],digits=2),",",round(outlistG$ESCI.u.r[outlistG$.id==s],digits=2),
         "] | `r sparkline(density(log(outlist2$test.p.value[outlist2$.id=='",s,"']))$y, type = 'line')` |")})


#`r sparkline(density(outlist1$test.p.value[outlist1$.id=='",s,"'])$y
outFile <- file("spark2.Rmd","w")
l_ply(hdr0,writeLines,con=outFile)
writeLines(setup,outFile)
writeLines(hdr1,outFile)
writeLines(hdr2,outFile)
writeLines(txt,outFile)
close(outFile)

rmarkdown::render(input = "spark2.Rmd",
                  output_format = "html_document",
                  output_file = "ML2_secondary.html")


