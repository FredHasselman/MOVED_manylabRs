# Code to bulk generate analyses, saves consoe output to a log file.

devtools::source_url('https://raw.githubusercontent.com/FredHasselman/manylabRs/master/pkg/R/C-3PR_ASCII.R')
init()

# Set variables ---------------------------------------------------------------------------------------------------
tp <- 1
subset = "all"

ML2.key <- ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
ML2.key <- ML2.key[!is.na(ML2.key$unique.id),]

analysis  <- c('study.global.include', 'study.primary.include', 'study.secondary.include','study.global.include')
if(!is.null(tp)){
    ID.global <- which(ML2.key[ , analysis[tp]]==1)
}

skip         <- NULL #c(14,15,38)
studies      <- ML2.key$unique.id[ID.global][!ML2.key$unique.id[ID.global]%in%skip]

saveRDSfile  <- TRUE
saveCSVfile  <- TRUE

# Set console output to file ---------------------------------------------------------------------------------------
startLog <- function(tp){
  if(length(studies)==length(ML2.key$unique.id[ID.global][!ML2.key$unique.id[ID.global]%in%skip])){
    con <- file(c(paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Global_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Primary_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Secondary_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_StudyOrder_",subset,".txt"))[tp])
  } else {
    con <- file(c(paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Global_",paste0(studies,collapse = "_"),"_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Primary_",paste0(studies,collapse = "_"),"_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_Secondary_",paste0(studies,collapse = "_"),"_",subset,".txt"),
                  paste0("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_StudyOrder_",paste0(studies,collapse = "_"),"_",subset,".txt"))[tp])
  }
  # Set the sink to 'con'
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  return(con)
}

restore <- function(con){
    # Restore output to console
    sink()
    sink(type="message")

    close(con)
    closeAllConnections()
}

con <- startLog(tp)

# This will echo all input and not truncate 150+ character lines...
tryCatch(testScript(studies = studies,
                    tp = tp,
                    subset = subset,
                    saveCSVfile = saveCSVfile,
                    saveRDSfile = saveRDSfile),
         finally = restore(con))




# #tb<-import("/Users/Fred/Dropbox/Manylabs2/TestOutput/ORI.EFFECTS/GatiTversk_oriTable.csv")
#
# library(ggplot2)
# library(devtools)
# source_url('https://raw.githubusercontent.com/FredHasselman/manylabRs/master/R/C-3PR_ASCII.R')
# ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
#
# # Load the the R objects used for this analysis (datasets filters, results, etc.)
# load("~/Dropbox/Manylabs2/TestOutput/ROBJECTS/Gati.1a_Global.RData")
# load("~/Dropbox/Manylabs2/TestOutput/ROBJECTS/Gati.4a_Global.RData")
#
# # Get the data used in the analysis, for convenience, the more complex model based analyses have a data.frame attached in addition to seperate data vectors.
# # df <- dataSource[[1]]$stat.data.analysed$df
# df <- as.data.frame(dataSource[[1]]$stat.data.analysed)
#
#
# # Check the analysis code, we do not need the summary, but the original test object.
# ML2.key$stat.test[ML2.key$study.analysis%in%dataSource[[1]]$analysis.name]
#
# # Fit the model
# #sav.1a <- lme4::glmer(Response ~ Importance * Condition + (1|uID/trialID), family = binomial('logit'), data = df)
# #df$Condition.z <- as.numeric(df$Condition)-1
# gati.1a <- lmer(DV ~ Condition + (Condition|uID) + (Condition|itemID), data = df)
# gati.4a <- lmer(DV ~ Condition + (Condition|uID) + (Condition|itemID), data = df)
#
# summary(gati.1a)
# summary(gati.4a)
#
# anova(gati.4a)
#
# rndata <- ranef(gati.1a)
# rndata <- ranef(gati.4a)
#
# library(ggplot2)
#
# dfItem <- rndata$itemID
#
# dfItem$Items <- as.numeric(rownames(dfItem))-1
#
# ilabs <- c("USA - Mexico",
# "Russia - Poland",
# "Albania - China",
# "USA - Israel",
# "Philippines - Japan",
# "Canada - USA",
# "Israel - Russia",
# "Ireland - England",
# "Austria - Germany",
# "Russia - France",
# "Belgium - Luxembourg",
# "Russia - USA",
# "China - N. Korea",
# "Sri Lanka - India",
# "USA - France",
# "Russia - Cuba",
# "England - Jordan",
# "France - Israel",
# "Germany - USA",
# "Syria - Russia",
# "France - Algeria")
#
# dfItem$Items <- factor(dfItem$Items, levels=1:21, labels = ilabs)
# dfItem$Items <- reorder(dfItem$Items,dfItem$`(Intercept)`)
#
# dfI <- gather(dfItem, value = Residual, key = RandomEffects, -Items)
# dfI$RandomEffects[grepl("Condition",dfI$RandomEffects)] <- "Condition (Slope)"
# ggplot(dfI, aes(y=Items, x=Residual)) +
#     geom_point() +
#     facet_wrap(~RandomEffects) +
#     scale_x_continuous(limits = c(-5,5)) +
#     labs(y = "Country Pair",
#          x = "Residuals (Similarity Judgement)",
#          title="Random Effects of ML2 analysis: gati.1ab",
#          subtitle = "Note: Means of country pair as residuals around grand mean & slope.") +
#     theme_bw()
#


# Create some data we'll use to predict the model
# predframe      <- expand.grid(Condition=levels(df$Condition), Importance=c(seq(-4,4,length=51),0,range(df$Importance)))
# predframe$pred <- predict(sav.1a, newdata = predframe, REform=NA,type="response")

# # Plot the interaction
# minmaxvals <- c(0,range(df$Importance))
# ggplot(predframe, aes(Importance,pred,linetype=Condition)) +
#     geom_line() +
#     geom_point(data=subset(predframe,Importance %in% minmaxvals), aes(shape=Condition)) +
#     labs(y = "Predicted Proportions [Response scale]", x = "Importance (centered)", title="Interaction Effect of ML2 analysis: savani.3ab", subtitle = "Note: Markers at mean (0) and observed range of Importance.") +
#     theme_bw()

# Plot the interaction
# minmaxvals <- c(0,range(df$DV))
# ggplot(predframe, aes(Condition,pred,linetype=Condition)) +
#     geom_line(stat = "identity") +
#     geom_point(aes(shape=Condition)) +
#     labs(y = "Predicted Proportions [Response scale]", x = "Importance (centered)", title="Interaction Effect of ML2 analysis: gati.1a", subtitle = "Note: Markers at mean (0) and observed range of Importance.") +
#     theme_bw()


#chisq.test(c(Pb = ML2.var[[g]]$ParentB,Pa = sum(ML2.var[[g]]$N)-ML2.var[[g]]$ParentB), p=c(1,0))
#system("R CMD Rd2pdf ManyLabRs")

# #testScript(studies,tp,saveCSVfile,saveRDSfile)
# #
  # studies = c(1)
  #    dfout1 <- get.analyses(studies,
  #                           tp=1,
  #                           rootdir   = "~/Dropbox/Manylabs2/TestOutput",
  #                           indir     = list(RAW.DATA = "RAW.DATA.PRIVATE",MASTERKEY = "",SOURCEINFO = ""),
  #                           outdir    = list(ROBJECTS    = "ROBJECTS",RESULTS.RDS = "RESULTS.RDS"))
#
#
# fdout<-get.analyses(studies      = 83,
#              analysis.type = 2,
#              Nmin.raw  = 1,
#              Nmin.cond = 1,
#              rootdir   = "/Users/Fred/Dropbox/Manylabs2/Gati trials/",
#              indir     = list(RAW.DATA = "gati_test_output",MASTERKEY = "",SOURCEINFO = ""),
#              outdir    = list(ROBJECTS    = "ROBJECTS",RESULTS.RDS = "RESULTS.RDS"))
#
# fdout$raw.case$Gati.2
#
# tmp <- ML2.sr[[g]]
#
# ML2.var[[g]]$Asymmetry
#
# all.equal(tmp$RawDataFilter[[1]],tmp$RawDataFilter[[2]])
#
# export(tmp$RawDataFilter[[1]],"/Users/Fred/Dropbox/Manylabs2/Gati trials/gati_test_analysis_CounterbalanceA.csv")
# export(tmp$RawDataFilter[[2]],"/Users/Fred/Dropbox/Manylabs2/Gati trials/gati_test_analysis_CounterbalanceB.csv")
# # DO NOT MELT -----------------------------------------------------------------------------------------------------
#
#  studies=26
# #
#  dfout <- get.analyses(studies,analysis.type=1)
# #
# tmpp <- dfout$aggregated$Inbar.1a
#
#  eee<-dfout$raw.case$Gati.1a
#
#  df.clean <-eee[eee$case.include,]
#
#
# m0 <- with(tmp,lmerTest::lmer(DV ~ Condition + (Condition|uID) + (Condition|itemID)))
#
#
# library(effects)
# DV<-tmp$DV
# Condition <- tmp$Condition
# ef <- effect("Condition", m0)
# summary(ef)
#
# rand(m0)
#
# sjp.lmer(m0,type="re.qq")
#
# sjp.lmer(m0,
#          type = "rs.ri",
#          show.legend = F)
#
# eff<-allEffects(m0)
#
# plot(eff)
# x <- as.data.frame(ef)
#
# summary(m0)
# library(sjPlot)
# sjp.lmer(m0,
#          facet.grid = FALSE,
#          sort.est = "sort.all",
#          y.offset = .4)
#
#  idA <- df.clean %>% select(starts_with("gati1s")) %>% complete.cases()
#  idB <- df.clean %>% select(starts_with("gati2s")) %>% complete.cases()
#
#  df.clean$cbA      <- "A"
#  df.clean$cbA[idB] <- "B"
#
#  dfA <- df.clean %>% filter(idA) %>% select(starts_with("gati1s"))
#  dfB <- df.clean %>% filter(idB) %>% select(starts_with("gati2s"))
#
#  prefix1 <- "gati1s."
#  prefix2 <- "gati2s."
#
#  CounterBalanceA <- list(P1st = paste0(prefix1, c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22)),
#                          P2nd = paste0(prefix1, c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21)))
#
#  CounterBalanceB <- list(P1st = paste0(prefix2, c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21)),
#                          P2nd = paste0(prefix2, c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22)))
#
#  dfA$CounterBalance <- 1
#  dfB$CounterBalance <- 2
#
#  itemID1st        <- which(colnames(dfA)%in%CounterBalanceA$P1st)
#  itemID2nd        <- which(colnames(dfA)%in%CounterBalanceA$P2nd)
#  dfA$Prominent1st <- dfA$Prominent2nd <- NA
#
#  dfA$Prominent1st <- rowMeans(dfA[,itemID1st], na.rm=TRUE)
#  dfA$Prominent2nd <- rowMeans(dfA[,itemID2nd], na.rm=TRUE)
#
#  itemID1st        <- which(colnames(dfB)%in%CounterBalanceB$P1st)
#  itemID2nd        <- which(colnames(dfB)%in%CounterBalanceB$P2nd)
#  dfB$Prominent1st <- dfB$Prominent2nd <- NA
#
#  dfB$Prominent1st <- rowMeans(dfB[,itemID1st], na.rm=TRUE)
#  dfB$Prominent2nd <- rowMeans(dfB[,itemID2nd], na.rm=TRUE)
#
#  dfA$Assymetry <- dfA$Prominent1st-dfA$Prominent2nd
#  dfB$Assymetry <- dfB$Prominent1st-dfB$Prominent2nd
#
#  df <- as.data.frame(rbind(dfA[,22:25],dfB[,22:25]))
#
#  df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))
#
#  df$study.order    <- NA
#  df$Country        <- NA
#  df$Language      <- NA
#
#  for(i in seq_along(df.clean$uID)){
#    idDF <- df$uID%in%df.clean$uID[i]
#    if(sum(idDF)>0){
#      df$study.order[idDF] <- df.clean$study.order[i]
#      df$Country[idDF] <- df.clean$Country[i]
#      df$Language[idDF] <- df.clean$Language[i]
#    }
#  }
#
#
#  df$Asymmetry  = df$Prominent1st-df$Prominent2nd
#
#  WEIRD <- df$Country%in%c("Australia","Austria","Canada","France","Germany","Hungary","Italy","New Zealand","Poland","Portugal","Serbia","Spain","Sweden", "The Netherlands","UK","USA")
#
#  nonWEIRD                       <- !WEIRD
#  df$source.WEIRD        <- 2
#  df$source.WEIRD[WEIRD] <- 1
#
#  df$source.WEIRD.f <- factor(df$source.WEIRD,
#                                        levels = c(1,2),
#                                        labels = c("WEIRD","non-WEIRD"))
#
#
#  df$USA <- "non-USA"
#  df$USA[df$Country%in%"USA"] <- "USA"
#
#  export(df,"Gati_bySubject_withOrder_similarity_rowmeans.xlsx")
#
# #df <- import("Gati_bySubject_withOrder_similarity_rowmeans.xlsx")
#
# df$prop = NA
# df$prop[df$Asymmetry<=0] <- 0
# df$prop[df$Asymmetry>0] <- 1
#
#  df$study.order.f <- factor(df$study.order, levels = 1:15, labels = paste("Order",1:15))
#
#  df<-df %>% group_by(Language) %>% mutate(Nlanguage = n())
#
#  df$Language.f <- paste0(df$Language," (N=",df$Nlanguage,")")
#
#  ggplot(df, aes(y=prop, x=CounterBalance)) +
#    stat_summary(fun.data = "mean_cl_boot") +
#    facet_wrap(~Language.f) + ylab("Proportion P1st>P2nd")
#
#
#
#
#
#    ggtitle("Do not create long data")
#
#
#  ggplot(df, aes(y=Asymmetry, x=CounterBalance)) +
#    geom_boxplot() +
#    ggtitle("Do not create long data")
#
#
#
# # varfun ----------------------------------------------------------------------------------------------------------
#
#  studies=83
#  dfout <- get.analyses(studies,analyis.type=2)
#
#  eee<-dfout$raw.case$Gati.2
#
#  df.clean <-eee[eee$case.include,]
#  df.clean$uID <- 1:nrow(df.clean)
#
#  idA <- df.clean %>% select(starts_with("gati1s")) %>% complete.cases()
#  idB <- df.clean %>% select(starts_with("gati2s")) %>% complete.cases()
#
#  df.clean$cbA      <- "A"
#  df.clean$cbA[idB] <- "B"
#
#  CounterBalanceA <- list(P1st = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22),
#                          P2nd = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21))
#
#  CounterBalanceB <- list(P1st =  c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21),
#                          P2nd = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22))
#
#  get.gatiData<-function(df.clean,CounterBalanceA,CounterBalanceB){
#
#    idA <- df.clean %>% select(starts_with("gati1s")) %>% complete.cases()
#    idB <- df.clean %>% select(starts_with("gati2s")) %>% complete.cases()
#
#    dfA <- df.clean %>% filter(idA) %>% select(starts_with("gati1s"),uID)
#    dfB <- df.clean %>% filter(idB) %>% select(starts_with("gati2s"),uID)
#
#    dfA$CounterBalance <- 1
#    dfB$CounterBalance <- 2
#
#    dfA <- reshape2::melt(dfA,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)
#
#    dfA$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfA$itemID))
#    dfA$Condition <- NA
#    dfA$Condition[dfA$itemID%in%CounterBalanceA$P1st] <- 1
#    dfA$Condition[dfA$itemID%in%CounterBalanceA$P2nd] <- 2
#
#    dfB <- reshape2::melt(dfB,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)
#
#    dfB$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfB$itemID))
#    dfB$Condition <- NA
#    dfB$Condition[dfB$itemID%in%CounterBalanceB$P1st] <- 1
#    dfB$Condition[dfB$itemID%in%CounterBalanceB$P2nd] <- 2
#
#    df <- as.data.frame(rbind(dfA,dfB))
#
#    df$Condition       <- factor(df$Condition,levels=c(1,2),labels=c('Prominent1st','Prominent2nd'))
#    df$Condition <- relevel(df$Condition, ref='Prominent1st')
#    df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))
#
#    #df <- df[order(df$uID,df$itemID), ]
#
#    df$study.order    <- NA
#    df$Country        <- NA
#    df$Language       <- NA
#    for(i in seq_along(df.clean$uID)){
#      idDF <- df$uID%in%df.clean$uID[i]
#      if(sum(idDF)>0){
#        df$study.order[idDF] <- df.clean$study.order[i]
#        df$Country[idDF] <- df.clean$Country[i]
#        df$Language[idDF] <- df.clean$Language[i]
#      }
#    }
#
#    df.subj <- summarize(group_by(df, uID, Condition, CounterBalance),
#                         stimDVm = mean(DV,na.rm = TRUE),
#                         # study.order = paste0(unique(study.order), collapse = "|"),
#                         Language = paste0(unique(Language), collapse = "|"),
#                         Country = paste0(unique(Country), collapse = "|")
#    )
#
#    df.subj<-df.subj %>% group_by(Language) %>% mutate(Nlanguage = n())
#    df.subj$Language.f <- paste0(df.subj$Language," (N=",df.subj$Nlanguage,")")
#
#    df.subj.wide  <- tidyr::spread(df.subj,
#                                   key   = Condition,
#                                   value = stimDVm)
#
#    df.subj.wide$Asymmetry  = df.subj.wide$Prominent1st-df.subj.wide$Prominent2nd
#
#    return(list(df = df,
#                df.subj = df.subj,
#                df.sibj.wide= df.subj.wide))
#  }
#
#
# gati.bad <- get.gatiData(df.clean,CounterBalanceB,CounterBalanceA)
# gati.fix <- get.gatiData(df.clean,CounterBalanceA,CounterBalanceB)
#
#
# gati.bad$df
# gati.fix$df.sibj.wide %>% group_by(CounterBalance) %>% summarise(mean(Asymmetry))
#
#  ggplot(gati.bad, aes(y=Asymmetry, x=CounterBalance)) +
#    geom_boxplot() +
#    facet_wrap(~Language) +
#    ggtitle("Broken?")
#
#
#  ggplot(gati.fix, aes(y=Asymmetry, x=CounterBalance)) +
#    geom_boxplot() +
#    facet_wrap(~Language) +
#    ggtitle("Fixed?")
#
#
#
# # By Country DIFFERENCE ----
#
#  df$Country1st <- df$Country2nd <- NA
#
#  IDcbA <- df$CounterBalance%in%"CBA"
#  IDcbB <- df$CounterBalance%in%"CBB"
#
#  df$Country1st[IDcbA&df$itemID==2] <- "Mexico"
#  df$Country2nd[IDcbA&df$itemID==2] <- "USA"
#  df$Country1st[IDcbA&df$itemID==3] <- "Poland"
#  df$Country2nd[IDcbA&df$itemID==3] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==4] <- "China"
#  df$Country2nd[IDcbA&df$itemID==4] <- "Albania"
#  df$Country1st[IDcbA&df$itemID==5] <- "Israel"
#  df$Country2nd[IDcbA&df$itemID==5] <- "USA"
#  df$Country1st[IDcbA&df$itemID==6] <- "Japan"
#  df$Country2nd[IDcbA&df$itemID==6] <- "Philippines"
#  df$Country1st[IDcbA&df$itemID==7] <- "USA"
#  df$Country2nd[IDcbA&df$itemID==7] <- "Canada"
#  df$Country1st[IDcbA&df$itemID==8] <- "Russia"
#  df$Country2nd[IDcbA&df$itemID==8] <- "Israel"
#  df$Country1st[IDcbA&df$itemID==9] <- "England"
#  df$Country2nd[IDcbA&df$itemID==9] <- "Ireland"
#  df$Country1st[IDcbA&df$itemID==10] <- "Germany"
#  df$Country2nd[IDcbA&df$itemID==10] <- "Austria"
#  df$Country1st[IDcbA&df$itemID==11] <- "France"
#  df$Country2nd[IDcbA&df$itemID==11] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==12] <- "Luxembourg"
#  df$Country2nd[IDcbA&df$itemID==12] <- "Belgium"
#  df$Country1st[IDcbA&df$itemID==13] <- "USA"
#  df$Country2nd[IDcbA&df$itemID==13] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==14] <- "North Korea"
#  df$Country2nd[IDcbA&df$itemID==14] <- "China"
#  df$Country1st[IDcbA&df$itemID==15] <- "India"
#  df$Country2nd[IDcbA&df$itemID==15] <- "Sri Lanka"
#  df$Country1st[IDcbA&df$itemID==16] <- "France"
#  df$Country2nd[IDcbA&df$itemID==16] <- "USA"
#  df$Country1st[IDcbA&df$itemID==17] <- "Cuba"
#  df$Country2nd[IDcbA&df$itemID==17] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==18] <- "Jordan"
#  df$Country2nd[IDcbA&df$itemID==18] <- "England"
#  df$Country1st[IDcbA&df$itemID==19] <- "Israel"
#  df$Country2nd[IDcbA&df$itemID==19] <- "France"
#  df$Country1st[IDcbA&df$itemID==20] <- "USA"
#  df$Country2nd[IDcbA&df$itemID==20] <- "Germany"
#  df$Country1st[IDcbA&df$itemID==21] <- "Russia"
#  df$Country2nd[IDcbA&df$itemID==21] <- "Syria"
#  df$Country1st[IDcbA&df$itemID==22] <- "Algeria"
#  df$Country2nd[IDcbA&df$itemID==22] <- "France"
#
#  df$Country2nd[IDcbB&df$itemID==2] <- "Mexico"
#  df$Country1st[IDcbB&df$itemID==2] <- "USA"
#  df$Country2nd[IDcbB&df$itemID==3] <- "Poland"
#  df$Country1st[IDcbB&df$itemID==3] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==4] <- "China"
#  df$Country1st[IDcbB&df$itemID==4] <- "Albania"
#  df$Country2nd[IDcbB&df$itemID==5] <- "Israel"
#  df$Country1st[IDcbB&df$itemID==5] <- "USA"
#  df$Country2nd[IDcbB&df$itemID==6] <- "Japan"
#  df$Country1st[IDcbB&df$itemID==6] <- "Philippines"
#  df$Country2nd[IDcbB&df$itemID==7] <- "USA"
#  df$Country1st[IDcbB&df$itemID==7] <- "Canada"
#  df$Country2nd[IDcbB&df$itemID==8] <- "Russia"
#  df$Country1st[IDcbB&df$itemID==8] <- "Israel"
#  df$Country2nd[IDcbB&df$itemID==9] <- "England"
#  df$Country1st[IDcbB&df$itemID==9] <- "Ireland"
#  df$Country2nd[IDcbB&df$itemID==10] <- "Germany"
#  df$Country1st[IDcbB&df$itemID==10] <- "Austria"
#  df$Country2nd[IDcbB&df$itemID==11] <- "France"
#  df$Country1st[IDcbB&df$itemID==11] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==12] <- "Luxembourg"
#  df$Country1st[IDcbB&df$itemID==12] <- "Belgium"
#  df$Country2nd[IDcbB&df$itemID==13] <- "USA"
#  df$Country1st[IDcbB&df$itemID==13] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==14] <- "North Korea"
#  df$Country1st[IDcbB&df$itemID==14] <- "China"
#  df$Country2nd[IDcbB&df$itemID==15] <- "India"
#  df$Country1st[IDcbB&df$itemID==15] <- "Sri Lanka"
#  df$Country2nd[IDcbB&df$itemID==16] <- "France"
#  df$Country1st[IDcbB&df$itemID==16] <- "USA"
#  df$Country2nd[IDcbB&df$itemID==17] <- "Cuba"
#  df$Country1st[IDcbB&df$itemID==17] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==18] <- "Jordan"
#  df$Country1st[IDcbB&df$itemID==18] <- "England"
#  df$Country2nd[IDcbB&df$itemID==19] <- "Israel"
#  df$Country1st[IDcbB&df$itemID==19] <- "France"
#  df$Country2nd[IDcbB&df$itemID==20] <- "USA"
#  df$Country1st[IDcbB&df$itemID==20] <- "Germany"
#  df$Country2nd[IDcbB&df$itemID==21] <- "Russia"
#  df$Country1st[IDcbB&df$itemID==21] <- "Syria"
#  df$Country2nd[IDcbB&df$itemID==22] <- "Algeria"
#  df$Country1st[IDcbB&df$itemID==22] <- "France"
#
#
#
#  df$Country1st[df$itemID==2] <- "Mexico"
#  df$Country2nd[df$itemID==2] <- "USA"
#  df$Country1st[df$itemID==3] <- "Poland"
#  df$Country2nd[df$itemID==3] <- "Russia"
#  df$Country1st[df$itemID==4] <- "China"
#  df$Country2nd[df$itemID==4] <- "Albania"
#  df$Country1st[df$itemID==5] <- "Israel"
#  df$Country2nd[df$itemID==5] <- "USA"
#  df$Country1st[df$itemID==6] <- "Japan"
#  df$Country2nd[df$itemID==6] <- "Philippines"
#  df$Country1st[df$itemID==7] <- "USA"
#  df$Country2nd[df$itemID==7] <- "Canada"
#  df$Country1st[df$itemID==8] <- "Russia"
#  df$Country2nd[df$itemID==8] <- "Israel"
#  df$Country1st[df$itemID==9] <- "England"
#  df$Country2nd[df$itemID==9] <- "Ireland"
#  df$Country1st[df$itemID==10] <- "Germany"
#  df$Country2nd[df$itemID==10] <- "Austria"
#  df$Country1st[df$itemID==11] <- "France"
#  df$Country2nd[df$itemID==11] <- "Russia"
#  df$Country1st[df$itemID==12] <- "Luxembourg"
#  df$Country2nd[df$itemID==12] <- "Belgium"
#  df$Country1st[df$itemID==13] <- "USA"
#  df$Country2nd[df$itemID==13] <- "Russia"
#  df$Country1st[df$itemID==14] <- "North Korea"
#  df$Country2nd[df$itemID==14] <- "China"
#  df$Country1st[df$itemID==15] <- "India"
#  df$Country2nd[df$itemID==15] <- "Sri Lanka"
#  df$Country1st[df$itemID==16] <- "France"
#  df$Country2nd[df$itemID==16] <- "USA"
#  df$Country1st[df$itemID==17] <- "Cuba"
#  df$Country2nd[df$itemID==17] <- "Russia"
#  df$Country1st[df$itemID==18] <- "Jordan"
#  df$Country2nd[df$itemID==18] <- "England"
#  df$Country1st[df$itemID==19] <- "Israel"
#  df$Country2nd[df$itemID==19] <- "France"
#  df$Country1st[df$itemID==20] <- "USA"
#  df$Country2nd[df$itemID==20] <- "Germany"
#  df$Country1st[df$itemID==21] <- "Russia"
#  df$Country2nd[df$itemID==21] <- "Syria"
#  df$Country1st[df$itemID==22] <- "Algeria"
#  df$Country2nd[df$itemID==22] <- "France"
#
#  ID1 <- df$Country%in%df$Country1st
#  ID2 <- df$Country1st%in%df$Country
#
#  df.cc <- df[ID1&ID2,]
#
#  df.cc$Country.f    <- factor(paste0("lab:",df.cc$Country))
#  df.cc$Country1st.f <- factor(df.cc$Country1st)
#  df.cc$Country2nd.f <- factor(df.cc$Country2nd)
#  df.cc$Condition.f  <- factor(df.cc$Condition)
#
#  df.cc$CB.Order <- interaction(df.cc$Condition,df.cc$CounterBalance)
#
#
#  df.cca <- summarise(group_by(df.cc, Country.f,Country1st.f,Country2nd.f, CB.Order),
#                      Similarity = mean(DV, na.rm=TRUE),
#                      SimilaritySD = sd(DV, na.rm=TRUE),
#                      N             = n())
#  df.cca$se <- df.cca$SimilaritySD/sqrt(df.cca$N)
#  df.cca$CIlo <- df.cca$Similarity-(1.96*df.cca$se)
#  df.cca$CIhi <- df.cca$Similarity+(1.96*df.cca$se)
#
# # plot similarity  ----
#
# ggplot(df.cca, aes(x = Country2nd.f, y = Similarity)) +
#   geom_point(aes(colour = CB.Order,
#                  size = N),
#              position = position_dodge(.9)) +
#    scale_color_discrete("Asymmetry: Similarity") +
#    scale_shape_discrete("Asymmetry: Similarity") +
#   facet_grid(Country.f~Country1st.f,scales = "free_x") +
#   scale_x_discrete("Country pair") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45,
#                                                 vjust = .85,
#                                                 hjust = .85)) +
#    coord_equal()
#
#
# #tmp      <-reshape2::melt(df.cca[,1:10], id = 1:3, measured = 4:10)
# tmp <- spread(df.cca[,1:5], key=CB.Order, value=Similarity)
#
# tmp$Asymmetry  <-NA
# tmp$difference <-NA
#
# tmp$Asymmetry <- tmp$Prominent1st.CBA-tmp$Prominent2nd.CBB
# tmp$difference[!is.na(tmp$Asymmetry)] <- "P1st.CBA - P2nd.CBB"
# tmp$difference[is.na(tmp$Asymmetry)]  <- "P1st.CBB - P2nd.CBA"
# tmp$Asymmetry[is.na(tmp$Asymmetry)]   <- na.exclude(tmp$Prominent1st.CBB-tmp$Prominent2nd.CBA)
#
#
# ggplot(tmp, aes(x = Country2nd.f,
#                 y = Asymmetry)) +
#   geom_point(aes(colour = difference, shape = difference),
#              position = position_dodge(.9), size=3) +
#   scale_color_discrete("Asymmetry: Similarity") +
#   scale_shape_discrete("Asymmetry: Similarity") +
#   facet_grid(Country.f~Country1st.f,scales = "free_x") +
#   scale_x_discrete("Country pair") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45,
#                                                 vjust = .85,
#                                                 hjust = .85)) +
#   coord_equal()
#
#
# # item based dataset ---
# df.item <- summarize(group_by(df, itemID, Condition, CounterBalance),
#                      stimDVm = mean(DV,na.rm = TRUE),
#                      study.order = paste0(unique(study.order), collapse = "|"),
#                      Country = paste0(unique(Country), collapse = "|"))
#
# df.item$CB.order <- NA
# df.item$CB.order <- interaction(df.item$Condition,df.item$CounterBalance)
#
# tmp <- spread(df.item[,c(1,4,7)], key = CB.order, value = stimDVm)
#
# tmp$Asymmetry  <-NA
# tmp$difference <-NA
#
# tmp$Asymmetry <- tmp$Prominent1st.CBA-tmp$Prominent2nd.CBB
# tmp$difference[!is.na(tmp$Asymmetry)] <- "P1st.CBA - P2nd.CBB"
# tmp$difference[is.na(tmp$Asymmetry)]  <- "P1st.CBB - P2nd.CBA"
# tmp$Asymmetry[is.na(tmp$Asymmetry)]   <- na.exclude(tmp$Prominent1st.CBB-tmp$Prominent2nd.CBA)
#
# ggplot(tmp, aes(x = difference, y = Asymmetry)) +
#   geom_boxplot() +
#   scale_x_discrete("Similarity")
#
#
#  # Suibject based dataset
#  df.subj <- summarize(group_by(df, uID, Condition, CounterBalance),
#                       stimDVm = mean(DV,na.rm = TRUE),
#                       study.order = paste0(unique(study.order), collapse = "|"),
#                       Country = paste0(unique(Country), collapse = "|")
#                       )
#
#
#
#
#
#  # df.subj1 <- summarize(group_by(df, uID, Condition, CounterBalance), stimDVsd = sd(DV, na.rm = TRUE))
#  # df.subj2 <- summarize(group_by(df, uID, Condition, CounterBalance), Ncases = n())
#
#  df.subj.wide  <- tidyr::spread(df.subj, key = Condition, value = stimDVm)
#  # df.subj.wide1 <- tidyr::spread(df.subj1, key = Condition, value = stimDVsd)
#  # df.subj.wide2 <- tidyr::spread(df.subj2, key = Condition, value = Ncases)
#  #
#  # df.subj.wide$Prominent1stSD     <- df.subj.wide1$Prominent1st
#  # df.subj.wide$Prominent2ndSD     <- df.subj.wide1$Prominent2nd
#  # df.subj.wide$Prominent1stNcases <- df.subj.wide2$Prominent1st
#  # df.subj.wide$Prominent2ndNcases <- df.subj.wide2$Prominent2nd
#  #
#
#   df.subj.wide$Asymmetry  = df.subj.wide$Prominent1st-df.subj.wide$Prominent2nd
#
# # df <- df[!is.na(df$study.order), ]
#
#  table(df.clean$study.order,df.clean$cbA)
#
#  WEIRD <- df.subj.wide$Country%in%c("Australia","Austria","Canada","France","Germany","Hungary","Italy","New Zealand","Poland","Portugal","Serbia","Spain","Sweden", "The Netherlands","UK","USA")
#
#  nonWEIRD                       <- !WEIRD
#  df.subj.wide$source.WEIRD        <- 2
#  df.subj.wide$source.WEIRD[WEIRD] <- 1
#
#  df.subj.wide$source.WEIRD.f <- factor(df.subj.wide$source.WEIRD,
#                                        levels = c(1,2),
#                                        labels = c("WEIRD","non-WEIRD"))
#
#
#  df.subj.wide$USA <- "non-USA"
#  df.subj.wide$USA[df.subj.wide$Country%in%"USA"] <- "USA"
#
#  export(df.subj.wide,"Gati_bySubject_withOrder_similarity.xlsx")
#
#
#
# df.subj.wide$study.order.f <- factor(df.subj.wide$study.order, levels = 1:15, labels = paste("Order",1:15))
#  ggplot(df.subj.wide, aes(y=Asymmetry, x=CounterBalance)) +
#    geom_boxplot() +
#    facet_wrap(~source.WEIRD.f,ncol = 7)
#
#
#  ggplot(df.subj.wide, aes(y=Asymmetry, x=CounterBalance)) +
#    geom_boxplot() +
#    ggtitle("First create long data")
#
#
#  # DIFFERENCE ----
#
#  studies=87
#  dfout2 <- get.analyses(studies,analyis.type=3)
#
#  aaa<-dfout2$raw.case$Gati.5
#
#  df.clean <-aaa[aaa$case.include,]
#
#  idA <- complete.cases(df.clean[,3:23])
#  idB <- complete.cases(df.clean[,24:44])
#
#  df.clean$cbA      <- "A"
#  df.clean$cbA[idB] <- "B"
#
#
#  dfA <- df.clean[idA,c( 3:23,58)]
#  dfB <- df.clean[idB,c(24:44,58)]
#
#  CounterBalanceB <- list(P1st = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22),
#                          P2nd = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21))
#  CounterBalanceA <- list(P1st = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21),
#                          P2nd = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22))
#
#  dfA$CounterBalance <- 1
#  dfB$CounterBalance <- 2
#
#  dfA <- reshape2::melt(dfA,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)
#
#  dfA$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfA$itemID))
#  dfA$Condition <- NA
#  dfA$Condition[dfA$itemID%in%CounterBalanceA$P1st] <- 1
#  dfA$Condition[dfA$itemID%in%CounterBalanceA$P2nd] <- 2
#
#  dfB <- reshape2::melt(dfB,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)
#
#  dfB$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfB$itemID))
#  dfB$Condition <- NA
#  dfB$Condition[dfB$itemID%in%CounterBalanceB$P1st] <- 1
#  dfB$Condition[dfB$itemID%in%CounterBalanceB$P2nd] <- 2
#
#  df <- as.data.frame(rbind(dfA,dfB))
#
#  df$Condition       <- factor(df$Condition,levels=c(1,2),labels=c('Prominent1st','Prominent2nd'))
#  df$Condition       <- relevel(df$Condition, ref='Prominent1st')
#  df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))
#
#  df <- df[order(df$uID,df$itemID), ]
#
#  df$study.order    <- NA
#  df$Country        <- NA
#  for(i in seq_along(df.clean$uID)){
#    idDF <- df$uID%in%df.clean$uID[i]
#    if(sum(idDF)>0){
#      df$study.order[idDF] <- df.clean$study.order[i]
#      df$Country[idDF] <- df.clean$Country[i]
#    }
#  }
#
#
#  # By Country ----
#
#  df$Country1st <- df$Country2nd <- NA
#
#  IDcbA <- df$CounterBalance%in%"CBA"
#  IDcbB <- df$CounterBalance%in%"CBB"
#
#  df$Country1st[IDcbA&df$itemID==2] <- "Mexico"
#  df$Country2nd[IDcbA&df$itemID==2] <- "USA"
#  df$Country1st[IDcbA&df$itemID==3] <- "Poland"
#  df$Country2nd[IDcbA&df$itemID==3] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==4] <- "China"
#  df$Country2nd[IDcbA&df$itemID==4] <- "Albania"
#  df$Country1st[IDcbA&df$itemID==5] <- "Israel"
#  df$Country2nd[IDcbA&df$itemID==5] <- "USA"
#  df$Country1st[IDcbA&df$itemID==6] <- "Japan"
#  df$Country2nd[IDcbA&df$itemID==6] <- "Philippines"
#  df$Country1st[IDcbA&df$itemID==7] <- "USA"
#  df$Country2nd[IDcbA&df$itemID==7] <- "Canada"
#  df$Country1st[IDcbA&df$itemID==8] <- "Russia"
#  df$Country2nd[IDcbA&df$itemID==8] <- "Israel"
#  df$Country1st[IDcbA&df$itemID==9] <- "England"
#  df$Country2nd[IDcbA&df$itemID==9] <- "Ireland"
#  df$Country1st[IDcbA&df$itemID==10] <- "Germany"
#  df$Country2nd[IDcbA&df$itemID==10] <- "Austria"
#  df$Country1st[IDcbA&df$itemID==11] <- "France"
#  df$Country2nd[IDcbA&df$itemID==11] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==12] <- "Luxembourg"
#  df$Country2nd[IDcbA&df$itemID==12] <- "Belgium"
#  df$Country1st[IDcbA&df$itemID==13] <- "USA"
#  df$Country2nd[IDcbA&df$itemID==13] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==14] <- "North Korea"
#  df$Country2nd[IDcbA&df$itemID==14] <- "China"
#  df$Country1st[IDcbA&df$itemID==15] <- "India"
#  df$Country2nd[IDcbA&df$itemID==15] <- "Sri Lanka"
#  df$Country1st[IDcbA&df$itemID==16] <- "France"
#  df$Country2nd[IDcbA&df$itemID==16] <- "USA"
#  df$Country1st[IDcbA&df$itemID==17] <- "Cuba"
#  df$Country2nd[IDcbA&df$itemID==17] <- "Russia"
#  df$Country1st[IDcbA&df$itemID==18] <- "Jordan"
#  df$Country2nd[IDcbA&df$itemID==18] <- "England"
#  df$Country1st[IDcbA&df$itemID==19] <- "Israel"
#  df$Country2nd[IDcbA&df$itemID==19] <- "France"
#  df$Country1st[IDcbA&df$itemID==20] <- "USA"
#  df$Country2nd[IDcbA&df$itemID==20] <- "Germany"
#  df$Country1st[IDcbA&df$itemID==21] <- "Russia"
#  df$Country2nd[IDcbA&df$itemID==21] <- "Syria"
#  df$Country1st[IDcbA&df$itemID==22] <- "Algeria"
#  df$Country2nd[IDcbA&df$itemID==22] <- "France"
#
#  df$Country2nd[IDcbB&df$itemID==2] <- "Mexico"
#  df$Country1st[IDcbB&df$itemID==2] <- "USA"
#  df$Country2nd[IDcbB&df$itemID==3] <- "Poland"
#  df$Country1st[IDcbB&df$itemID==3] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==4] <- "China"
#  df$Country1st[IDcbB&df$itemID==4] <- "Albania"
#  df$Country2nd[IDcbB&df$itemID==5] <- "Israel"
#  df$Country1st[IDcbB&df$itemID==5] <- "USA"
#  df$Country2nd[IDcbB&df$itemID==6] <- "Japan"
#  df$Country1st[IDcbB&df$itemID==6] <- "Philippines"
#  df$Country2nd[IDcbB&df$itemID==7] <- "USA"
#  df$Country1st[IDcbB&df$itemID==7] <- "Canada"
#  df$Country2nd[IDcbB&df$itemID==8] <- "Russia"
#  df$Country1st[IDcbB&df$itemID==8] <- "Israel"
#  df$Country2nd[IDcbB&df$itemID==9] <- "England"
#  df$Country1st[IDcbB&df$itemID==9] <- "Ireland"
#  df$Country2nd[IDcbB&df$itemID==10] <- "Germany"
#  df$Country1st[IDcbB&df$itemID==10] <- "Austria"
#  df$Country2nd[IDcbB&df$itemID==11] <- "France"
#  df$Country1st[IDcbB&df$itemID==11] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==12] <- "Luxembourg"
#  df$Country1st[IDcbB&df$itemID==12] <- "Belgium"
#  df$Country2nd[IDcbB&df$itemID==13] <- "USA"
#  df$Country1st[IDcbB&df$itemID==13] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==14] <- "North Korea"
#  df$Country1st[IDcbB&df$itemID==14] <- "China"
#  df$Country2nd[IDcbB&df$itemID==15] <- "India"
#  df$Country1st[IDcbB&df$itemID==15] <- "Sri Lanka"
#  df$Country2nd[IDcbB&df$itemID==16] <- "France"
#  df$Country1st[IDcbB&df$itemID==16] <- "USA"
#  df$Country2nd[IDcbB&df$itemID==17] <- "Cuba"
#  df$Country1st[IDcbB&df$itemID==17] <- "Russia"
#  df$Country2nd[IDcbB&df$itemID==18] <- "Jordan"
#  df$Country1st[IDcbB&df$itemID==18] <- "England"
#  df$Country2nd[IDcbB&df$itemID==19] <- "Israel"
#  df$Country1st[IDcbB&df$itemID==19] <- "France"
#  df$Country2nd[IDcbB&df$itemID==20] <- "USA"
#  df$Country1st[IDcbB&df$itemID==20] <- "Germany"
#  df$Country2nd[IDcbB&df$itemID==21] <- "Russia"
#  df$Country1st[IDcbB&df$itemID==21] <- "Syria"
#  df$Country2nd[IDcbB&df$itemID==22] <- "Algeria"
#  df$Country1st[IDcbB&df$itemID==22] <- "France"
#
#
#
#  df$Country1st[df$itemID==2] <- "Mexico"
#  df$Country2nd[df$itemID==2] <- "USA"
#  df$Country1st[df$itemID==3] <- "Poland"
#  df$Country2nd[df$itemID==3] <- "Russia"
#  df$Country1st[df$itemID==4] <- "China"
#  df$Country2nd[df$itemID==4] <- "Albania"
#  df$Country1st[df$itemID==5] <- "Israel"
#  df$Country2nd[df$itemID==5] <- "USA"
#  df$Country1st[df$itemID==6] <- "Japan"
#  df$Country2nd[df$itemID==6] <- "Philippines"
#  df$Country1st[df$itemID==7] <- "USA"
#  df$Country2nd[df$itemID==7] <- "Canada"
#  df$Country1st[df$itemID==8] <- "Russia"
#  df$Country2nd[df$itemID==8] <- "Israel"
#  df$Country1st[df$itemID==9] <- "England"
#  df$Country2nd[df$itemID==9] <- "Ireland"
#  df$Country1st[df$itemID==10] <- "Germany"
#  df$Country2nd[df$itemID==10] <- "Austria"
#  df$Country1st[df$itemID==11] <- "France"
#  df$Country2nd[df$itemID==11] <- "Russia"
#  df$Country1st[df$itemID==12] <- "Luxembourg"
#  df$Country2nd[df$itemID==12] <- "Belgium"
#  df$Country1st[df$itemID==13] <- "USA"
#  df$Country2nd[df$itemID==13] <- "Russia"
#  df$Country1st[df$itemID==14] <- "North Korea"
#  df$Country2nd[df$itemID==14] <- "China"
#  df$Country1st[df$itemID==15] <- "India"
#  df$Country2nd[df$itemID==15] <- "Sri Lanka"
#  df$Country1st[df$itemID==16] <- "France"
#  df$Country2nd[df$itemID==16] <- "USA"
#  df$Country1st[df$itemID==17] <- "Cuba"
#  df$Country2nd[df$itemID==17] <- "Russia"
#  df$Country1st[df$itemID==18] <- "Jordan"
#  df$Country2nd[df$itemID==18] <- "England"
#  df$Country1st[df$itemID==19] <- "Israel"
#  df$Country2nd[df$itemID==19] <- "France"
#  df$Country1st[df$itemID==20] <- "USA"
#  df$Country2nd[df$itemID==20] <- "Germany"
#  df$Country1st[df$itemID==21] <- "Russia"
#  df$Country2nd[df$itemID==21] <- "Syria"
#  df$Country1st[df$itemID==22] <- "Algeria"
#  df$Country2nd[df$itemID==22] <- "France"
#
#  ID1 <- df$Country%in%df$Country1st
#  ID2 <- df$Country1st%in%df$Country
#
#  df.cc <- df[ID1&ID2,]
#
#  df.cc$Country.f    <- factor(paste0("lab:",df.cc$Country))
#  df.cc$Country1st.f <- factor(df.cc$Country1st)
#  df.cc$Country2nd.f <- factor(df.cc$Country2nd)
#  df.cc$Condition.f <- factor(df.cc$Condition)
#
#  df.cc$CB.Order <- interaction(df.cc$Condition,df.cc$CounterBalance)
#
#
#  df.cca <- summarise(group_by(df.cc, Country.f,Country1st.f,Country2nd.f, CB.Order),
#                      Difference = mean(DV, na.rm=TRUE),
#                      DifferenceSD = sd(DV, na.rm=TRUE),
#                      N             = n())
#  df.cca$se <- df.cca$DifferenceSD/sqrt(df.cca$N)
#  df.cca$CIlo <- df.cca$Difference-(1.96*df.cca$se)
#  df.cca$CIhi <- df.cca$Difference+(1.96*df.cca$se)
#
#  ggplot(df.cca, aes(x = Country2nd.f, y = Difference)) +
#    geom_point(aes(colour = CB.Order,size = N), position = position_dodge(.9)) +
#    facet_grid(Country.f~Country1st.f,scales = "free_x") +
#    scale_x_discrete("Country pair") +
#    theme_bw() + theme(axis.text.x = element_text(angle = 45,
#                                                  vjust = .85,
#                                                  hjust = .85)) +
#    coord_equal()
#
#
#  #tmp      <-reshape2::melt(df.cca[,1:10], id = 1:4, measured = 5:10)
#  #df.wide <- spread(tmp, key=variable, value=value)
#
#  tmp <- spread(df.cca[,1:5], key=CB.Order, value=Difference)
#
#  tmp$Asymmetry  <-NA
#  tmp$difference <-NA
#
#  tmp$Asymmetry <- tmp$Prominent1st.CBA-tmp$Prominent2nd.CBB
#  tmp$difference[!is.na(tmp$Asymmetry)] <- "P1st.CBA - P2nd.CBB"
#  tmp$difference[is.na(tmp$Asymmetry)]  <- "P1st.CBB - P2nd.CBA"
#  tmp$Asymmetry[is.na(tmp$Asymmetry)]   <- na.exclude(tmp$Prominent1st.CBB-tmp$Prominent2nd.CBA)
#
# # plot difference ----
#
#  ggplot(tmp, aes(x = Country2nd.f,
#                  y = Asymmetry)) +
#    geom_point(aes(colour = difference, shape = difference),
#               position = position_dodge(.9), size=3) +
#    scale_color_discrete("Asymmetry: Difference") +
#    scale_shape_discrete("Asymmetry: Difference") +
#    facet_grid(Country.f~Country1st.f,scales = "free_x") +
#    scale_x_discrete("Country pair") +
#    theme_bw() + theme(axis.text.x = element_text(angle = 45,
#                                                  vjust = .85,
#                                                  hjust = .85)) +
#    coord_equal()
#
#
#
#
#  # item based dataset ---
#  df.item <- summarize(group_by(df, itemID, Condition, CounterBalance),
#                       stimDVm = mean(DV,na.rm = TRUE),
#                       study.order = paste0(unique(study.order), collapse = "|"),
#                       Country = paste0(unique(Country), collapse = "|"))
#
#  df.item$CB.order <- NA
#  df.item$CB.order <- interaction(df.item$Condition,df.item$CounterBalance)
#
#  tmp <- spread(df.item[,c(1,4,7)], key = CB.order, value = stimDVm)
#
#  tmp$Asymmetry  <-NA
#  tmp$difference <-NA
#
#  tmp$Asymmetry <- tmp$Prominent1st.CBA-tmp$Prominent2nd.CBB
#  tmp$difference[!is.na(tmp$Asymmetry)] <- "P1st.CBA - P2nd.CBB"
#  tmp$difference[is.na(tmp$Asymmetry)]  <- "P1st.CBB - P2nd.CBA"
#  tmp$Asymmetry[is.na(tmp$Asymmetry)]   <- na.exclude(tmp$Prominent1st.CBB-tmp$Prominent2nd.CBA)
#
#  ggplot(tmp, aes(x = difference, y = Asymmetry)) +
#    geom_boxplot() +
#    scale_x_discrete("Difference")
#
# df$Condition
# # Subject based dataset ---
#  df.subj <- summarize(group_by(df, uID, Condition, CounterBalance),
#                       stimDVm = mean(DV,na.rm = TRUE),
#                       study.order = paste0(unique(study.order), collapse = "|"),
#                       Country = paste0(unique(Country), collapse = "|"))
#
#  # df.subj1 <- summarize(group_by(df, uID, Condition, CounterBalance), stimDVsd = sd(DV, na.rm = TRUE))
#  # df.subj2 <- summarize(group_by(df, uID, Condition, CounterBalance), Ncases = n())
#
#  df.subj.wide  <- tidyr::spread(df.subj, key = Condition, value = stimDVm)
#  # df.subj.wide1 <- tidyr::spread(df.subj1, key = Condition, value = stimDVsd)
#  # df.subj.wide2 <- tidyr::spread(df.subj2, key = Condition, value = Ncases)
#  #
#  # df.subj.wide$Prominent1stSD     <- df.subj.wide1$Prominent1st
#  # df.subj.wide$Prominent2ndSD     <- df.subj.wide1$Prominent2nd
#  # df.subj.wide$Prominent1stNcases <- df.subj.wide2$Prominent1st
#  # df.subj.wide$Prominent2ndNcases <- df.subj.wide2$Prominent2nd
#  #
#
#  df.subj.wide$Asymmetry  = df.subj.wide$Prominent1st-df.subj.wide$Prominent2nd
#
#  ggplot(df.subj.wide, aes(x=CounterBalance, y=Asymmetry)) +
#    geom_boxplot() +
#    facet_wrap(~Country)
#
#
#  # df <- df[!is.na(df$study.order), ]
#
#  table(df.clean$study.order,df.clean$cbA)
#
#
#  WEIRD <- df.subj.wide$Country%in%c("Australia","Austria","Canada","France","Germany","Hungary","Italy","New Zealand","Poland","Portugal","Serbia","Spain","Sweden", "The Netherlands","UK","USA")
#
#  nonWEIRD                       <- !WEIRD
#  df.subj.wide$source.WEIRD        <- 2
#  df.subj.wide$source.WEIRD[WEIRD] <- 1
#
#  df.subj.wide$source.WEIRD.f <- factor(df.subj.wide$source.WEIRD,
#                                      levels = c(1,2),
#                                      labels = c("WEIRD","non-WEIRD"))
#
#  df.subj.wide$USA <- "non-USA"
#  df.subj.wide$USA[df.subj.wide$Country%in%"USA"] <- "USA"
#
#  export(df.subj.wide,"Gati_bySubject_withOrder_difference.xlsx")
#
#  df.subj.wide$study.order.f <- factor(df.subj.wide$study.order, levels = 1:15, labels = paste("Order",1:15))
#  ggplot(df.subj.wide, aes(y=Asymmetry, x=CounterBalance)) +
#    geom_boxplot() +
#    facet_wrap(~source.WEIRD.f,ncol = 2) +
#
#
#
# #     aaa <- dfout2$aggregated$Graham.1
# #
# # sigCor <- cbind(pearson = aaa$test.p.value<.05, spearman = eee$test.p.value < .05)
# #
# # eee$source.name[sigCor[,2]]
#
#
#  #
#  #      cor.test.fisherZ(r1=.3,r2=.4,n1=100,n2=100)
#  #
#  #     dfout2$aggregated$Rottenstreich.1$ESCI.d
#
# # compute.es::chies(chi.sq=x, level=CL*100, n = N, verbose = TRUE, dig = 5)
