##############PDF Title page#############
PDFCreate <- function(data, 
        FilterVars,
	loopvar, 
	AnalysisBy,
	Trial,
	Treatment,
	Rep, 
	UTC,
	TimeXaxis,
	Response,
        TrtType,
        TrtBlack,
        TrtGreen,
        TrtRed,
        TrtPurple,
        TrtOrange,
	TableSortByTrt,
	OutputURL,
	TitlePageTitle1,
	TitlePageTitle2,
	xLabel,
	yLabel,
	PDFHeader1,
	PDFHeader2,
	PDFHeader3) {
  data[,"Treatment"] <- data[,Treatment]
  pdfnum=1

  calculated_df <- data.frame()
  for (pdfi in 1:pdfnum){
    pdfdatafilter <- data  
    MainTitle <- "Data Summary"
    pdfname=paste0("audpcBy", AnalysisBy,format(Sys.time(), "%a%b%d%H%M%S"),'.pdf')
    fullpdffilename<-paste(OutputURL,pdfname,sep='/')
    pdf(fullpdffilename, onefile = TRUE, width=10, height=8.5,  paper="USr")
    plot.new()
    grid.text(MainTitle,gp=gpar(cex=3,font=1),y=0.7,x=0.5,just='center')
    grid.text("AUDPC",gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
    grid.text(TitlePageTitle1,gp=gpar(cex=1.6,font=1),y=0.4,x=0.5,just='center')
    grid.text(TitlePageTitle2,gp=gpar(cex=1.6,font=1),y=0.35,x=0.5,just='center')

    add_footer() 
    assign("loopvar",loopvar,envir = .GlobalEnv)
    assign("AnalysisBy",AnalysisBy,envir = .GlobalEnv)
    sqlgroupvar <- paste0(paste(loopvar,collapse=','), ',', get(AnalysisBy))
    if (AnalysisBy=="Treatment") 
      contentsummary=sqldf(paste("select ", sqlgroupvar, ' as "Treatment",count(distinct ', Trial, ') as "# of Trials" from pdfdatafilter group by ',sqlgroupvar ))
    else 
      contentsummary=sqldf(paste("select distinct ", sqlgroupvar, " from pdfdatafilter"))
    perpage=20
    totaltrialnum=dim(contentsummary)[1]
    tableno=ceiling(totaltrialnum/perpage)
    for (i in 1:tableno) {
      plot.new()
      grid.table(contentsummary[((i-1)*perpage+1):min(i*perpage,dim(contentsummary)[1]),], rows=NULL)
      tabletitle='Table of Contents'
      if (tableno!=1){
        tabletitle=paste0(tabletitle,'(',i,')')
      }
      grid.text(PDFHeader1,gp=gpar(cex=0.9,font=1),y=0.95,x=0.1,just='left')
      grid.text(PDFHeader2,gp=gpar(cex=0.9,font=1),y=0.93,x=0.1,just='left')
      grid.text(PDFHeader3,gp=gpar(cex=0.9,font=1),y=0.91,x=0.1,just='left')
      grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.9,x=0.5,just='center')
      add_footer() 
    }
    
    tocnum=dim(contentsummary)[1]
    assign("contentsummary",contentsummary,envir = .GlobalEnv)
    assign("loopvar",loopvar,envir = .GlobalEnv)
    for (i in 1:tocnum){
      currentkeys<-as.data.frame(contentsummary[i,])
      currentkeys_utc <-currentkeys[,loopvar]
      assign("UTC",UTC,envir = .GlobalEnv)
      currentkeys_utc[,Treatment]<-UTC
      currentdata=sqldf(paste("select *",'from pdfdatafilter natural inner join currentkeys' ))
      currentdata_utc=sqldf(paste("select *",'from data natural inner join currentkeys_utc' ))
      assign("sqlgroupvar",sqlgroupvar,envir = .GlobalEnv)
      assign("data",data,envir = .GlobalEnv)
      assign("currentkeys",currentkeys,envir = .GlobalEnv)
      assign("currentkeys_utc",currentkeys_utc,envir = .GlobalEnv)
      if (AnalysisBy=="Treatment") 
        sqlgroupvar <- paste(sqlgroupvar, Trial, Treatment, sep=',')
      else
        sqlgroupvar <- paste(sqlgroupvar, Trial, TrtType, Treatment, sep=',')
      sqlgroupvar <- paste(sqlgroupvar,TimeXaxis,sep=',')
      sqlordervar <- paste(sqlgroupvar,Rep,sep=',')
      currentsummary=sqldf(paste('select ', sqlgroupvar, ',', Rep, ', avg(', Response,') as "Response" from currentdata group by ',sqlgroupvar, ' order by ', sqlordervar ))
      currentfinal <- unique(currentsummary[,get(AnalysisBy)])
      assign("sqlordervar",sqlordervar,envir = .GlobalEnv)
      assign("currentsummary",currentsummary,envir = .GlobalEnv)
      assign("currentfinal",currentfinal,envir = .GlobalEnv)
      title1 <- paste(currentfinal,": Calculated AUDPC, RAUDPC, stAUDPC")
      title2 <- paste("Matched by :",paste(c(loopvar,get(AnalysisBy)),collapse='|'))
      title3 <- paste(unique(currentsummary[,c(loopvar,get(AnalysisBy))]),collapse = '|')
      out <- CalculateAUDPC(currentkeys=currentkeys,
                        FilterVars=FilterVars,
                        AnalysisBy=AnalysisBy,
			currentdata=currentdata,
			currentdata_utc=currentdata_utc,
			Trial= Trial,
			Treatment=Treatment,
			Rep=Rep, 
			UTC=UTC,
			TimeXaxis=TimeXaxis,
			Response=Response,
			TableSortByTrt=TableSortByTrt,
			title1=title1,
			title2=title2,
			title3=title3,
			PDFHeader1=PDFHeader1,
			PDFHeader2=PDFHeader2,
			PDFHeader3=PDFHeader3,
			Export=FALSE)	
      out <- CalculateAUDPC(currentkeys=currentkeys,
                        FilterVars=FilterVars,
                        AnalysisBy=AnalysisBy,
			currentdata=currentdata,
			currentdata_utc=currentdata_utc,
			Trial= Trial,
			Treatment=Treatment,
			Rep=Rep, 
			UTC=UTC,
			TimeXaxis=TimeXaxis,
			Response=Response,
			TableSortByTrt=TableSortByTrt,
			title1=title1,
			title2=title2,
			title3=title3,
			PDFHeader1=PDFHeader1,
			PDFHeader2=PDFHeader2,
			PDFHeader3=PDFHeader3,
			Export=TRUE)
      assign("out_df",out$df,envir = .GlobalEnv)
      calculated_df <- rbind(calculated_df,out$df)
      title1 <- paste(currentfinal,": Plot of ",yLabel, " vs ", xLabel)
      singlelineplot(data=currentsummary,
                        loopvar=loopvar,
                        AnalysisBy=AnalysisBy, 
	                Trial,
                	Treatment,
			TimeXaxis=TimeXaxis,
			Response=Response,
			TrtType,
			TrtBlack,
			TrtGreen,
			TrtRed,
			TrtPurple,
		        TrtOrange,
			xLabel=xLabel,
			yLabel=yLabel,
			title1=title1,
			title2=title2,
			title3=title3,
			PDFHeader1=PDFHeader1,
			PDFHeader2=PDFHeader2,
			PDFHeader3=PDFHeader3)
    }
    dev.off()
  }
  return(list(url=fullpdffilename,pdfname=pdfname,calculated_df=calculated_df))
}

CalculateAUDPC <- function(currentkeys,
                        FilterVars,
                        AnalysisBy,
			currentdata,
			currentdata_utc,
			Trial,
			Treatment,
			Rep, 
			UTC,
			TimeXaxis,
			Response,
			TableSortByTrt,
			title1,
			title2,
			title3,
			PDFHeader1,
			PDFHeader2,
			PDFHeader3,
			Export) {	
  if (Export==TRUE) {
    currentaverage=sqldf(paste0('select Treatment,', Trial, ',', TimeXaxis, ',', Rep,  ', avg(', Response, ') as "', Response, '" from currentdata group by Treatment,',Trial,',',TimeXaxis, ',', Rep ))
    currentaverage_utc=sqldf(paste0('select Treatment,', Trial, ',', TimeXaxis, ',', Rep, ', avg(', Response, ') as "', Response, '" from currentdata_utc group by ',Trial,',',TimeXaxis, ',', Rep )) 
  } else {
    currentaverage=sqldf(paste0('select Treatment,', Trial, ',', TimeXaxis,  ', avg(', Response, ') as "', Response, '" from currentdata group by Treatment,',Trial,',',TimeXaxis ))
    currentaverage_utc=sqldf(paste0('select Treatment,', Trial, ',', TimeXaxis,  ', avg(', Response, ') as "', Response, '" from currentdata_utc group by ',Trial,',',TimeXaxis ))
  }
  assign("currentkeys",currentkeys,envir = .GlobalEnv)
  assign("FilterVars",FilterVars,envir = .GlobalEnv)
  assign("Trial",Trial,envir = .GlobalEnv)
  assign("Treatment",Treatment,envir = .GlobalEnv)
  assign("Trial",Trial,envir = .GlobalEnv)
  assign("Rep",Rep,envir = .GlobalEnv)
  assign("UTC",UTC,envir = .GlobalEnv)
  assign("TimeXaxis",TimeXaxis,envir = .GlobalEnv)
  assign("Response",Response,envir = .GlobalEnv)
  assign("currentaverage",currentaverage,envir = .GlobalEnv)
  assign("currentaverage_utc",currentaverage_utc,envir = .GlobalEnv)
  assign("currentdata",currentdata,envir = .GlobalEnv)
  assign("currentdata_utc",currentdata_utc,envir = .GlobalEnv)
  assign("currentaverage",currentaverage,envir = .GlobalEnv)
  assign("currentaverage_utc",currentaverage_utc,envir = .GlobalEnv)
  assign("currentkeys",currentkeys,envir = .GlobalEnv)

  raudpc <-data.frame()
  if (Export==TRUE) {
    for (Replicate in levels(factor(currentaverage[,Rep]))) {
      currentaverage_rep <- subset(currentaverage,get(Rep)==Replicate)
      currentaverage_utc_rep <- subset(currentaverage_utc,get(Rep)==Replicate)
      for (trtmnt in levels(factor(currentaverage_rep[,"Treatment"]))) {
        try({
          evaluation<-currentaverage_rep[currentaverage_rep[,"Treatment"]==trtmnt,Response]
          dates<-currentaverage_rep[currentaverage_rep$Treatment==trtmnt,TimeXaxis]
          evaluation_utc<-currentaverage_utc_rep[currentaverage_utc_rep$Trial==currentkeys$Trial,Response]
          dates_utc<-currentaverage_utc_rep[currentaverage_utc_rep$Trial==currentkeys$Trial,TimeXaxis]
          df <- currentkeys
          df$Treatment <- trtmnt
          df$Rep <- Replicate
          if (length(evaluation)==1) evaluation <-c(0,evaluation)
          if (length(dates)==1) evaluation <-c(0,dates)
          if (length(evaluation_utc)==1) evaluation <-c(0,evaluation_utc)
          if (length(dates_utc)==1) evaluation <-c(0,dates_utc)
          df$totalarea = round(audpc(evaluation, dates, type = "absolute"),2)
          df$u_area = round(audpc(evaluation_utc, dates_utc, type = "absolute"),2)
          df$r_area = round(df$totalarea/df$u_area,6)
          df$std_area = round(df$totalarea/(max(dates)-min(dates)),6)
          raudpc <- rbind(raudpc,df)
        })
      }
    } 
    colnames(raudpc)<-c(colnames(raudpc)[1:(length(colnames(raudpc))-6)],Treatment, Rep, 'AUDPC','Untreated \nAUDPC','AUDPC Relative \nto Untreated','Standardized \nAUDPC')
    assign("currentdata",currentdata,envir = .GlobalEnv)
    assign("raudpc",raudpc,envir = .GlobalEnv)  
    return (list(df=raudpc))
  } else {
    for (trtmnt in levels(factor(currentaverage[,"Treatment"]))) {
      try({
        evaluation<-currentaverage[currentaverage[,"Treatment"]==trtmnt,Response]
        dates<-currentaverage[currentaverage$Treatment==trtmnt,TimeXaxis]
        evaluation_utc<-currentaverage_utc[currentaverage_utc$Trial==currentkeys$Trial,Response]
        dates_utc<-currentaverage_utc[currentaverage_utc$Trial==currentkeys$Trial,TimeXaxis]
        df <- data.frame(A=trtmnt)
        df$B=currentkeys$Trial 
        if (length(evaluation)==1) evaluation <-c(0,evaluation)
        if (length(dates)==1) evaluation <-c(0,dates)
        if (length(evaluation_utc)==1) evaluation <-c(0,evaluation_utc)
        if (length(dates_utc)==1) evaluation <-c(0,dates_utc)
        df$totalarea = round(audpc(evaluation, dates, type = "absolute"),2)
        df$u_area = round(audpc(evaluation_utc, dates_utc, type = "absolute"),2)
        df$r_area = round(df$totalarea/df$u_area,6)
        df$std_area = round(df$totalarea/(max(dates)-min(dates)),6)
        raudpc <- rbind(raudpc,df)
      })
    }
    if (TableSortByTrt) raudpc <- raudpc[order(raudpc$A),] 
    else raudpc <- raudpc[order(raudpc$totalarea),] 
    colnames(raudpc)<-c('Treatment','Trial No','AUDPC','Untreated \nAUDPC','AUDPC Relative \nto Untreated','Standardized \nAUDPC')
    perpage=15
    tableno=ceiling(dim(raudpc)[1]/perpage)
    for (i in 1:tableno) {
      plot.new()
      grid.table(raudpc[((i-1)*perpage+1):min(i*perpage,dim(raudpc)[1]),], rows=NULL)
      tabletitle <- title1
      if (tableno!=1){
        tabletitle=paste0(tabletitle,'(',i,')')
      }
      grid.text(PDFHeader1,gp=gpar(cex=0.9,font=1),y=0.95,x=0.1,just='left')
      grid.text(PDFHeader2,gp=gpar(cex=0.9,font=1),y=0.93,x=0.1,just='left')
      grid.text(PDFHeader3,gp=gpar(cex=0.9,font=1),y=0.91,x=0.1,just='left')
      grid.text(title2,gp=gpar(cex=1.2,font=2),y=0.84,x=0.5,just='center')
      grid.text(title3,gp=gpar(cex=1.2,font=2),y=0.80,x=0.5,just='center')
      grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.88,x=0.5,just='center')
      add_footer() 
    }
  }
}	
	
singlelineplot <- function(data,
			loopvar,
			AnalysisBy, 
                	Trial,
                        Treatment,
			TimeXaxis,
			Response,
			TrtType,
			TrtBlack,
			TrtGreen,
			TrtRed,
			TrtPurple,
		        TrtOrange,
			xLabel,
			yLabel,
			title1,
			title2,
			title3,
			PDFHeader1,
			PDFHeader2,
			PDFHeader3) {
  assign("data",data,envir = .GlobalEnv)
  assign("loopvar",loopvar,envir = .GlobalEnv)
  assign("AnalysisBy",AnalysisBy,envir = .GlobalEnv)
  assign("TimeXaxis",TimeXaxis,envir = .GlobalEnv)
  assign("Response",Response,envir = .GlobalEnv)
  assign("TrtType",TrtType,envir = .GlobalEnv)
  assign("TrtBlack",TrtBlack,envir = .GlobalEnv)
  assign("TrtGreen",TrtGreen,envir = .GlobalEnv)
  assign("TrtRed",TrtRed,envir = .GlobalEnv)
  assign("TrtPurple",TrtPurple,envir = .GlobalEnv)
  assign("TrtOrange",TrtOrange,envir = .GlobalEnv)
  assign("xLabel",xLabel,envir = .GlobalEnv)
  assign("yLabel",yLabel,envir = .GlobalEnv)
  assign("title1",title1,envir = .GlobalEnv)
  assign("title2",title2,envir = .GlobalEnv)
  assign("title3",title3,envir = .GlobalEnv)
  assign("PDFHeader1",PDFHeader1,envir = .GlobalEnv)
  assign("PDFHeader2",PDFHeader2,envir = .GlobalEnv)
  assign("PDFHeader3",PDFHeader3,envir = .GlobalEnv)
  # Create Line Chart
  x <- data[,TimeXaxis]
  y <- data$Response
  h <- (max(y)-min(y))/20
  if (AnalysisBy=="Trial") AnalysisBy1="Treatment"
  if (AnalysisBy=="Treatment") AnalysisBy1="Trial"
  color <- factor(data[,get(AnalysisBy1)])
  titles=paste(title1, '\n',title2, '\n',title3)
  gp <- ggplot(data=data,aes(x = data[,TimeXaxis],y = data$Response))  +
  geom_line(aes(color=Prog_Alias)) +
  geom_point(aes(color=Prog_Alias)) +
  geom_line(aes(linetype=Prog_Alias)) +
  geom_point(aes(shape=Prog_Alias)) +
  geom_text(x=min(x), y=max(y), label=PDFHeader1) +
  geom_text(x=min(x), y=max(y)-h, label=PDFHeader2) +
  geom_text(x=min(x), y=max(y)-2*h, label=PDFHeader3) +
  labs(title=titles,x =xLabel, y = yLabel, color=get(AnalysisBy1)) + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position="none",legend.text = element_text(size=7)) +
  #theme(legend.direction ="vertical",legend.position="bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) 

  if (AnalysisBy=="Trial") {
  color.codes <- vector(mode="character", length=length(levels(color)))
  if (TrtBlack > " "){
    for (i in 1:length(levels(color))) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtBlack)
        color.codes[i] <- "black" 
    }
  }
  if (TrtGreen > " "){
    for (i in 1:length(levels(color))) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtGreen)
        color.codes[i] <- "green"
    }
  }
  if (TrtRed > " "){
    for (i in 1:length(levels(color))) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtRed)
        color.codes[i] <- "red"
    }
  }
  if (TrtPurple > " "){
    for (i in 1:length(levels(color))) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtPurple)
        color.codes[i] <- "purple"
    }
  }  
  if (TrtOrange > " "){
    for (i in 1:length(levels(color))) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtOrange)
        color.codes[i] <- "orange"
    }
  } 
  mycolors <- color.codes[color.codes>" "] 
  random.colors <- setdiff(sample(colors(),length(levels(color))),mycolors)
  color.codes[color.codes==""] <- random.colors[1:length(color.codes[color.codes==""])]
    gp <- gp + scale_color_manual(values=setNames(color.codes,levels(color)))
  }
  gp <-  gp + theme(legend.position="bottom",legend.text = element_text(size=8))
  footertext <- paste0("FMC Confidential     Reveal V1.1      ", Sys.Date())
  gp <- grid.arrange(gp, bottom = textGrob(footertext, 
                         just = "center", vjust=1,
                         gp = gpar(fontface = 3L, fontsize = 7)))
  print(gp)
}
