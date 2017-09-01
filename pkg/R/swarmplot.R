get.SwarmPlot <- function(df, anonymous=FALSE, addSize=FALSE, addMedian=TRUE, addOriES=TRUE, addLabel=FALSE, oriES=oriEffects, fillvar=c("USA","sigf")[1]){

    sourceInfo <- get.GoogleSheet(url="https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/export?format=csv")$df

    df$slabel <- "No label"
    if(!anonymous){
        l_ply(seq_along(oriEffects$study.analysis), function(l) df$slabel[tolower(as.character(df$.id))==oriEffects$study.analysis[l]] <<- oriEffects$description[l])
        df$slabel <- factor(df$slabel)
    } else {
        l_ply(seq_along(oriEffects$study.analysis), function(l) df$slabel[tolower(as.character(df$.id))==oriEffects$study.analysis[l]] <<- l)
        df$slabel <- factor(df$slabel)
    }


    df$.id    <- factor(df$.id)
    btype     <- "swarm"
    pdf(tempfile())
    bs    <- beeswarm(ESCI.r ~ slabel, data = df,
                      horizontal = FALSE, corral = "none",
                      corralWidth = 5,
                      pch = 21, spacing = 2, cex = .5,
                      priority = "ascending", method = btype, do.plot = TRUE)[, c(1, 2, 4, 6)]
    dev.off()

    colnames(bs)[4] <- "labels"

    df <- data.frame(df,bs)
    se <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}

    df$meanES <- ldply(unique(df$labels),
                       function(r) cbind(rep(mean(df$y[df$labels==r], na.rm = TRUE),
                                             sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
    df$seES <- ldply(unique(df$labels),
                     function(r) cbind(rep(se(df$y[df$labels==r]),
                                           sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]

    df <- arrange(df, meanES)

    df$xx <- ldply(unique(df$labels),
                   function(r) cbind(scale(df$x[df$labels==r], scale = F)))[,1]
    df$xf <- ldply(seq_along(unique(df$labels)),
                   function(r) cbind(df$xx[df$labels==unique(df$labels)[r]] +
                                         seq(10,10*length(unique(df$labels)), by=10)[r]))[,1]
    df$xn <- ldply(seq_along(unique(df$labels)),
                   function(r) cbind(rep(seq(10,10*length(unique(df$labels)),by=10)[r],
                                         sum(df$labels==unique(df$labels)[r]))))[,1]

    keylabs <- c("Mean of Sample Effect Sizes","Effect Size of Global Mean")

    mxN       <- max(df$ESCI.N.total)
    mypalette <- c("#d73027","#4575b4")

    df$sigf <- NA
    df$sigf[df$test.p.value> .05] <- "Not Significant"
    df$sigf[df$test.p.value<=.05] <- "Significant"
    df$sigf <- factor(df$sigf)

    df$Country <- "No Country"
    l_ply(seq_along(df$.id), function(l) df$Country[l] <<- sourceInfo$Country[sourceInfo$Source.Global==df$study.source[l]])

    df$USA                    <- "Non-USA"
    df$USA[df$Country=="USA"] <- "USA"

    df$USA <- factor(df$USA)

    df <- df[df$ESCI.N.total>=30,]

    dfG <- summarise(group_by(df,.id),
                     y= mean(ESCI.r,na.rm = T),
                     ymin=mean(ESCI.l.r, na.rm = T),
                     ymax=mean(ESCI.u.r, na.rm = T))

    dfG   <- arrange(dfG, y)
    dfG$x <- seq(10,10*nrow(dfG),by=10)

    # if(anonymous){
    #     df$labels <- as.numeric(df$labels)
    # }

    cwhite="#f7f7f7"
    ccream="#ffffbf"

    oriES$mES <- laply(oriES$study.analysis, function(s) dfG$y[tolower(dfG$.id)==s])
    oriES <- dplyr::arrange(oriES, mES)
    oriES$x <- seq(10,10*nrow(oriES),by=10)

    df$fillvar <- df[,fillvar]

    g <-ggplot(df, aes(x=xf, y=y)) +
        geom_vline(xintercept = unique(df$xn), colour = "grey80",alpha = 1) +
        geom_hline(yintercept = 0, colour = "ivory4")

    if(addSize){
        g <-  g +
            geom_point(aes(fill = USA, size = ESCI.N), col=cwhite, pch=21)
        # scale_size_continuous("Sample Size", breaks = c(0.01, 0.1, 0.3, 0.5,0.8,1),
        #                       labels = round(c(0.01, 0.1, 0.3, 0.5,0.8, 1) * mxN),
        #                       guide = guide_legend(override.aes=list(colour="grey30",fill="grey70"), byrow = TRUE)
        # )
    } else {
        g <-  g +
            geom_point(aes(fill = fillvar), size = 2, col=cwhite, pch=21)
    }

    if(addMedian){
        g <- g + geom_point(data=dfG,aes(x=x,y=y),
                            color="black",fill=ccream,alpha=1,size=3,pch=22)
    }

    if(addOriES){
        g <- g +
            geom_point(data=oriES,aes(x=x,y=ESCI.r),
                            color="black",fill="#a1d76a",alpha=1,size=3,pch=23)
    }

    if(addLabel){
        g <- g +
            geom_text(aes(label=study.source,hjust=0,color=USA),
                      size= 1,
                      angle = 45,
                      position=position_dodge(.9))
    }

    g <- g +
        scale_y_continuous("Effect Size r", limits = c(-1,1)) +
        scale_x_continuous("", breaks = unique(df$xn),
                           labels = unique(paste(df$labels)),
                           expand = c(0, 10)) +
        scale_fill_manual("",values = mypalette,
                          guide   = guide_legend(override.aes = list(size = 4),
                                                 byrow = TRUE)) +
        scale_shape_manual(labels=keylabs, values=c(24,25),guide=guide_legend("")) +
        gg.theme() + coord_flip()  +
        theme(legend.position = "top", legend.background=element_rect())

    return(g)

}

