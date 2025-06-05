
# Load libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(plotly,reshape2,gridExtra,egg,grid,TTR,cowplot,readr)


######################## Figure 1 ###############################


# A helper function for the first figure
gplot_mine<-function(dataset,var="gg.1",nam="G",ma_n=22,base_size=16,margins=c(-0.6,0.25,-0.5,0.35),
                     legend.position="none",l_m=0,legend_text_size=14,hjust=0.05,vjust=-10,
                     title_size=12,scale_y_lim=NULL, var_dict, LogTrans=T,add_xlabs=F){
  
  
  # Add a title
  titl=var_dict[which(var_dict$var==var),"nam"]
  
  # If we want a log transformed time series, append it to the var name to select the correct column
  if (LogTrans) var=paste(var,"Log",sep="_")
  
  # Add moving average
  ma_var=paste(var,"ma",sep=".")
  dataset[,ma_var] = SMA(x=dataset[,var], n=ma_n)
  nam_ma=paste0("MA(",nam,")")
  
  
  #  Scale
  vmin=floor(min(dataset[,var]))
  vmax=ceiling(max(dataset[,var]))
  scale_y_lim=c(vmin,vmax)
  
  gplot_res <- ggplot() +
    {if(var == "VI.H1.Log"){
      geom_line(data=dataset,aes_string(x="Date", y = var, color=shQuote("RV"))) 
    } else {
      geom_line(data=dataset,aes_string(x="Date", y = var, color=shQuote("Att")))
    }
    }+
    geom_line(data=dataset, aes_string(x="Date", y= ma_var, color=shQuote("MA_22")))+
    
    ggplot2::ggtitle(titl)+
    xlab("") + ylab("")+
    theme_bw(base_size = base_size) +
    theme(legend.position=legend.position,
          legend.justification = "right",
          legend.margin = margin(l_m, l_m, l_m, l_m, "cm"),
          legend.text = element_text(size = legend_text_size),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          axis.title.y = element_text(angle = 0, margin = margin(t = 20)),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = hjust, vjust = vjust,size = title_size),
          plot.margin=unit(margins, "cm")
    )+
    {if(var == "VI.H1.Log"){
      scale_color_manual(name = "", values = c(RV="red",MA_22="black"),limits = c("RV", "MA_22"))
    } else {
      scale_color_manual(name = "", values = c(Att="red",MA_22="black"),limits = c("Att", "MA_22"))
    }
    }+
    
    
    {if(!is.null(scale_y_lim)){ 
      scale_y_continuous(limits = scale_y_lim,
                         labels = scales::label_number(accuracy = 0.1))
    }
    } +
    {if(!add_xlabs){
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
      )
    }
    }+
    scale_x_date(date_breaks = "years", date_labels = "%Y",date_minor_breaks = "years")
  
  print(gplot_res)
  return(gplot_res)
}


figure1_function <- function(stock_dataset,VRS=c("gg.1","wg.1","tg.1","pg.1","VI.H1.Log"),
                    legend_text_size=14,base_size=18,title_size=18,
                    l_m=0.1,hjust=0.5,vjust=-10,  # title up + title down -
                    margins=c(-0.75, 0.05,-0.75,0.05),legend.position="none",ma_n=22,
                    filename="figure1.jpeg",results_wd="./results",parent_wd, figures_wd="./figures"){
  
  # Prepare data
  # Transform variables
  stock_dataset = transform(stock_dataset,totrans=c("gg.1","wg.1","tg.1","pg.1",
                                                    "tg.3","tg.4","tg.6","tg.7","tg.9","tg.10",
                                                    "pg.3","pg.4","pg.6","pg.7","pg.9","pg.10"),
                            formats=c('Log'),K=5,addto=T)
  stock_dataset = stock_dataset$DR
  stock_dataset$wg.1.ma22 = SMA(x=stock_dataset[,"wg.1"], n=22)
  stock_dataset$wg.1_Log_NA<-stock_dataset$wg.1_Log
  stock_dataset[stock_dataset$wg.1_Log_NA==0,"wg.1_Log_NA"]<-NA
  
  # Var dictionary
  var_dict<-data.frame("var"=c("gg.1","wg.1","tg.1","tg.9","tg.10","pg.1", "pg.9", "pg.10","VI.H1.Log"),
                       "nam"=c("Google attention","Wikipedia attention","Twitter attention",
                               "Twitter positive sentiment","Twitter negative sentiment",
                               "Newspaper attention","Newspaper positive sentiment","Newspaper negative sentiment",
                               "RV"))
  # Empty list for graphs
  graphs<-list()
  
  # Loop over selected variables in VRS
  for (i in 1:length(VRS)){
    
    # Select variable
    var=VRS[i]
    
    # Do we want to select variables that have been log transformed above by the transform function?
    LogTrans=T
    # RV is already log transformed,, so set it to FALSE
    if (var == "VI.H1.Log") LogTrans=F
    
    # Add x axis labs?
    add_xlabs=F
    if (i == length(VRS)){
      add_xlabs=T
      margins=c(-0.75, 0.05,0,0.05)
    } 
    
    # Make a plot for one variable
    p<-gplot_mine(stock_dataset,var=var,nam="G",ma_n=ma_n,base_size=base_size,
                  margins=margins,legend.position="bottom",l_m=l_m,
                  legend_text_size=legend_text_size,hjust=hjust,vjust=vjust,
                  title_size=title_size,scale_y_lim=NULL,var_dict=var_dict,
                  LogTrans=LogTrans,add_xlabs=add_xlabs)
    
    # Store the outcome into a list
    graphs[[paste("p",var,sep=".")]]<-p
  }
  
  
  # Remove legends
  graphs<-lapply(graphs, remove_legend)
  g <- plot_grid(plotlist = graphs[c(1,2,3,4,5,5)], nrow=3, align="v",rel_heights =c(1,1,1.3))
  
  setwd(results_wd)
  jpeg(filename, units="in",width=20, height=10,res=300) #specifics of the image you want to save
  g
  dev.off()
  setwd(parent_wd)
  setwd(figures_wd)
  
  return(g)
  
}



######################## Figure 2 ###############################
figure2_function= function(results, ext.rem = 12,perc=1,quan=c("top"),selection,model_dict,
                 nms,losses=c("MSE","QLIKE","MAE","MAPE"),filename="figure2.jpeg",
                 results_wd="./results",parent_wd, figures_wd="./figures"){
  
  # Start by running function get_store.tbl to get losses for these results
  store.tbl = get_store.tbl(out=results,ext.rem=ext.rem,perc=perc,quan=quan,selection=selection,
                            model_dict=model_dict,nms=nms,losses=losses)
  
  # Loop over models
  for (m in 2:11){
    mg<-improvement_graph(store.tbl,m,hist_out="black", hist_fill="white",
                          mean_col="red",mean_type="dashed",bw=2,bbs=50)
    assign(x=paste0("m",m),mg)
  }
  
  p <- plot_grid(m5,m8,m9,m10, ncol=2,nrow=2, align = "h")
  
  setwd(results_wd)
  jpeg(filename, units="in",width=14, height=7,res=300) #specifics of the image you want to save
  print(p)
  dev.off()
  setwd(parent_wd)
  setwd(figures_wd)
  
  
  
  return(p)
}


improvement_graph<-function(store.tbl,m,hist_out="darkblue", hist_fill="lightblue",
                            mean_col="red",mean_type="solid",bw=1,bbs=40,base_size=18){
  mod_dict<-data.frame("store.tbl"=dimnames(store.tbl)[[3]],
                       "latex"=c("HAR","CSR","HAR-M","HAR-A","CSR-A","ALA-A","RF-A","HAR-S","CSR-S","ALA-S","RF-S"))
  mn=mod_dict[mod_dict$store.tbl==dimnames(store.tbl)[[3]][m],2]
  df= data.frame("improvement"=100*(store.tbl[1,1,1,]-store.tbl[1,1,m,])/store.tbl[1,1,1,]) 
  mg=ggplot(df, aes(x=improvement)) +
    geom_histogram(bins=bbs,
                   color=hist_out, fill=hist_fill)+
    geom_vline(aes(xintercept=0),color=mean_col,  size=0.8, linetype=mean_type)+
    xlab(paste0("Averge % forecast improvement [",mn,"]")) + 
    ylab("")+
    theme_minimal(base_size = base_size)+
    theme(panel.grid.minor = element_blank())
  return(mg)
}



######################## Timing Figure ###############################

timing_figure <- function(dataset,nams, parent_wd, figures_wd, bloomberg_partial_wd="./bloomberg/partial results", 
                         timing_wd="./timing", vers="att",filename="figureA1.png",results_wd="./results"){
  
  
  #### Load or prepare dummies ####
  # Try to load true_dummies.RData from timing_wd folder, if it exists
  file_name = paste0(timing_wd, "/true_dummies.RData")
  if (file.exists(file_name)) {
    load(file_name)
    print("true_dummies loaded")
  } else {
    # If it doesn't exist, load the econ file from ./bloomberg and create it
    setwd(parent_wd)
    econ_file_name = paste0(bloomberg_partial_wd, "/econ.RData")
    econ <- load(econ_file_name)
    # Run function make_econ_dummy_new to prepare dummies without shifting them for prediction
    true_dummies<-make_econ_dummy_new(econ=econ, calendar='UnitedStates/NYSE',
                                      from1="2005-01-01",to1="2021-02-24", 
                                      from2="2005-12-30", to2="2021-02-24",
                                      opn="09:30:00", cls="16:30:00",
                                      opt="dummy",move_hours=F)
    
    # Save it to a RData file for future use
    setwd(figures_wd)
    save(true_dummies,file=file_name)
    print("true_dummies save to an RData file")
  }

  
  #### Merge datasets ####
  DT<-merge(true_dummies[,1:10],dataset, by="row.names", all = F)
  rownames(DT)<-DT$Row.names; DT<-DT[,-1]
  # Assign variables names to new dummies
  colnames(DT)[1:10]<-paste("d",1:10,sep=".")
  # Save
  save(DT,file=paste0(timing_wd, "/DT.RData"))
  
  
  ############## ALL GRAPHS ############## 
  
  event_dict<-data.frame(
    "event"=c("NFP","IJC","FOMC","GDP","CPI","ISM","SENT","CBCC","RSA","GDO"),
    "idx"=1:10)  
  
  var_dict<-data.frame("var"=c("ge.1","we.1","te.1","te.81","te.91","pe.1", "pe.81", "pe.91"),
                       "nam"=c("Google attention","Wikipedia attention","Twitter attention",
                               "Twitter positive sentiment","Twitter negative sentiment",
                               "Newspaper attention","Newspaper positive sentiment","Newspaper negative sentiment"))
  

  # Prepare empty lists
  all_graphs<-list()
  all_grids<-list()
  
  
  # Loop over events
  for (i in 1:10){
    print(i)
    
    add_title=F
    add_xtitle=F
    
    # add graph titles to i=1
    if (i==1) add_title=T
    
    # add x axis title for i=10 - "Days relative to announcement"
    if (i==10) add_xtitle=T        # dont use this for a title, only for labels
    
    # Subset dataset
    sub<-nams[which(nams$Keyword==nams$Keyword[i]),]
    sub<-sub[which(sub$Source!="bloom"),]
    sub<-sub[which(sub$Type.of.variable!="sentiment"),]
    sub<-sub[which(sub$Sentiment %in% c(NA,"finbert")),]
    idx<-c(paste("d",i,sep = "."),sub$Var);print(idx)
    DTS<-DT[,idx]  
    DTS$Date<-row.names(DTS)
    DTS<-data.frame(DTS)
    DTS$Date<-as.Date(DTS$Date)
    
    AV<-colnames(DTS)[-1]; AV=AV[which(AV!="Date")]
    if (vers=="att") AV<-AV[c(1,2,3,6)]
    if (vers=="sent") AV<-AV[c(4,5,7,8)]
    
    graphs<-list()
    for(att_var in AV){
      add_ytitle=F
      if (att_var == AV[1]) add_ytitle=T
      p<-event_graph(DTS,att_var=att_var,pre=5,post=5,
                     hjust=0.5,vjust=-5, title_size=16,base_size=16,
                     add_title=add_title,add_ytitle=add_ytitle,add_xtitle=add_xtitle, 
                     var_dict=var_dict,event_dict=event_dict,i=i,ar=0.5,
                     low="gray90", high="gray30",onecol="#8c8c8c",vers=vers)
      
      
      graphs[[paste("p",att_var,sep=".")]]<-p
    }
    
    all_graphs[[i]]<-graphs
    
    
    # get legend
    mylegend<-g_legend(graphs[[1]])
    # remove legends
    graphs<-lapply(graphs, remove_legend)
    g1 <- gridExtra::grid.arrange(grobs = graphs,ncol=length(AV),widths =c(1.2,rep(1,(length(AV)-1))))
    g2 <- grid.arrange(g1,mylegend, nrow=2,heights=c(10, 1))
    
    
    if (i==10){
      # Use version with legend on the bottom subgrid
      g<-g2
    } else {
      g<-g1
    }
    
    all_grids[[i]]<-g
  }
  
  
  
  setwd(results_wd)
  png(filename, units="in",width=12, height=16,res=300) #specifics of the image you want to save
  g_all<-gridExtra::grid.arrange(grobs = all_grids,nrow=length(all_grids),
                                 heights=c(1.2,rep(1,8),1.4), 
                                 bottom=textGrob("Days relative to announcement", gp=gpar(fontsize=16),vjust=-1))
  dev.off()
  setwd(parent_wd)
  setwd(figures_wd)
  
  return(g_all)
  

}



# Make econ dummies without moving the dummy a day prior
# Helper function for timing_table function
make_econ_dummy_new <- function(econ, calendar, from1, to1, from2, to2, opn, cls,
                                opt="dummy",move_hours=F){
  
  # extract list of unique events from the original bloomberg dataset "econ"
  events <- unique(econ$Event)
  
  # define trading days
  # load NYSE calendar
  load_quantlib_calendars(c(calendar),from=from1,to=to1)
  
  # make a sequence of trading days
  full_calendar <- paste('QuantLib/',calendar,sep='')
  td <- bizseq(from2, to2, full_calendar)
  
  # prepare a dataframe, first column has trading days, the rest of the columns
  # will be dummy variables corresponding to econ events
  dummies <- data.frame(matrix(ncol=(length(unique(econ$Event))+1),nrow=length(td), 
                               dimnames=list(NULL, c("td", events))))
  dummies$td <- td
  
  # Loop over events 
  for (i in 1:length(events)){
    print(paste(i,events[i]))
    # we will work with a separate dataframe (event.df) for each event
    # choose rows correspoding to event[i], columns Date Time, Event
    event.df <- filter(econ, Event == events[i])[,c(1,3,9,10,11,12)]
    # create open and close columns for each day the event occurred
    event.df$open <- get_date_time(as.Date(event.df[,1]),c(opn)) %>%
      force_tz("Etc/GMT+5")
    event.df$close <- get_date_time(as.Date(event.df[,1]),c(cls)) %>%
      force_tz("Etc/GMT+5")
    # find out status - whether the event occurred before, after or during trading hours
    event.df$status <- ifelse(event.df$`Date Time` > event.df$close, "after", 
                              ifelse(event.df$`Date Time` < event.df$open, "before", "during"))
    
    # Loop over dates
    for (date in 1:length(event.df[,1])){
      # get day from the first column 
      day=as.Date(event.df[date,1])
      
      if(move_hours==T){
        # if the event occurred before or during trading hours, move to the previous day 
        if ((event.df$status[date] == "before") || (event.df$status[date] == "during")){
          print(day)
          day <- day-1
          print(paste("before or during trading hours --> moved to previous day",day))
        } 
      }
      
      
      # if the day is not a trading day, roll to previous trading day
      if (length(which(dummies$td == day)) == 0){
        day <- adjust.previous(day,full_calendar)
        print(paste("nontrading day --> moved to previous trading day", day))
      }
      
      # find out the row number of the final date, this dummy belongs to
      event.df$row[date] <- which(dummies$td == day)  
      
      if (opt=="dummy"){
        # assign 1 to each row defined in the event.df dataframe, 
        # to the column corresponding to ith event + 1(first column is td)
        dummies[event.df$row[date],1+i] <- 1
      }
      
      if (opt=="est"){
        dummies[event.df$row[date],1+i] <- event.df$`# Ests.`[date]
      }
      
      if (opt=="std"){
        dummies[event.df$row[date],1+i] <- as.numeric(event.df$`Std Dev`[date])
      }
      
      if (opt=="unc"){
        dummies[event.df$row[date],1+i] <- as.numeric(event.df$Uncertainty[date])
      }
      
      if (opt=="range"){
        dummies[event.df$row[date],1+i] <- as.numeric(event.df$Range[date])
      }
      
    }
    dummies[is.na(dummies)] <- 0
  }
  # we don't need td as a column any more, change it to rownames instead
  rownames(dummies) <- dummies$td
  dummies<-dummies[,-1]
  return(dummies)
}




# Create an event graph for 1 att/sent dummy
# Create an event graph for 1 att/sent dummy
event_graph<-function(DTS,att_var="ge.1",pre=5,post=5,
                      hjust=0.5,vjust=-5, title_size=14,base_size=14,
                      add_title=F,add_ytitle=F,add_xtitle=F, 
                      var_dict,event_dict,i,ar=0.5,low="gray90", high="gray30",
                      onecol="gray50",vers="att"){
  
  
  # Title of the graph
  if (add_title) plot_title=var_dict[which(var_dict$var==att_var),"nam"]
  
  if (add_ytitle) ytitle=event_dict[which(event_dict$idx==i),"event"]
  
  
  #### DATA PREPARATION ###
  
  
  # Select events
  event_idx<-which(DTS[,1]==1)
  
  # Make sure there are complete sequences of days before and after the event
  event_idx <- event_idx[which(event_idx >= (pre+1))]
  event_idx <- event_idx[which(event_idx <= (nrow(DTS)-post))]
  
  # Corresponding dates
  event_dates<-DTS[event_idx,"Date"]
  
  # Choose range of days to look at
  days_seq<-seq(-pre,post)
  
  # Prepare an empty dataframe
  df <- as.data.frame(matrix(NA,nrow=length(event_idx),ncol=(1+length(days_seq)),
                             dimnames=list(NULL,c("Date",c(paste("event","b",pre:1,sep="."),paste("event",0,sep="."),paste("event","a",1:post,sep="."))))))
  # Assign dates
  df$Date<-event_dates
  
  # Loop to pick the ranges around events
  for (j in 1:length(event_idx)){
    e<-event_idx[j]
    slct<-c((e-pre):(e+post))
    df[j,-1]<-DTS[slct,att_var]
  }
  
  df<-df[complete.cases(df),]
  
  # MIN-MAX normalization
  mmin<-min(df[,-1])
  mmax<-max(df[,-1])
  
  mmscale<-function(x,mmin,mmax){
    y=scale(x, center = mmin, scale = mmax - mmin)
    return(y)
  }
  
  DF=df
  DF[,-1]=apply(DF[,-1],2,mmscale,mmin,mmax)
  
  
  # Melt 
  DF_melted = melt(DF,id.vars = "Date")
  
  
  #### 1 GGPLOT SUBPLOT ###
  
  p=ggplot(DF_melted, aes(x = variable, y = value)) + 
    
    # Do Date color gradient?
    {if(vers=="att"){
      geom_line(aes(group = Date,color="Attention"))
    }else{
      geom_line(aes(group = Date,color="Sentiment"))
    }
    }+
    
    {if(vers=="att"){
      stat_summary(aes(group=Date, color="Average Attention"), fun=mean, geom="line",size=1,linetype="dashed",group=1)
    }
    }+
    {if(vers=="sent"){
      stat_summary(aes(group=Date, color="Average Sentiment"), fun=mean, geom="line",size=1,linetype="dashed",group=1)
    }
    }+
    
    # stat_summary(aes(group=Date, color="Average Attention"), fun=mean, geom="line",size=1,linetype="dashed",group=1)+
    stat_summary(aes(group=Date), fun.y=mean, geom="point", color="black",
                 size=2.5,group=1,shape=18)+
    geom_vline(xintercept = 6, color = "black",alpha=0.5)+ 
    
    {if(add_title){
      ggtitle(plot_title) 
    } else {
      ggtitle(" ")
    }
    }+
    
    {if(add_xtitle){
      xlab("")
      # xlab("Days relative to announcement") 
    } else {
      xlab("")
    }
    }+
    
    {if(add_ytitle){
      ylab(ytitle) 
    } else {
      ylab("")
    }
    }+
    # xlab("Days relative to announcement") + ylab("")+
    # xlab("") + ylab("")+
    
    # Common theme for all subplots
    theme_light(base_size = base_size) +
    theme(
      # legend.position="none",
      legend.position="bottom",
      legend.justification = "right",
      legend.text = element_text(size=14),
      axis.title.y = element_text(angle = 90, vjust = 0.5),
      plot.title = element_text(hjust = hjust,vjust = -1, size = title_size)
      # ,plot.margin=unit(c(-0.5,0,-0.5,0), "cm"),
      # aspect.ratio = ar
    )+
    
    # guides(colour = guide_legend(override.aes = list(size=3)))+
    # guide_legend(override.aes = list(linewidth = 2))+
    
    # X and Y axis labels and ticks only on the leftmost and bottommost subplots
    {if(!add_ytitle){
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
    }
    }+
    {if(!add_xtitle){
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
            # ,
            # plot.margin=unit(c(-0.5,0,0,0), "cm")
      )
    }
    }+
    
    # Custom margins
    #                     unit(c(top,  right, bottom, left), units)
    {if(add_xtitle){
      if (add_ytitle){
        theme(plot.margin=unit(c(-0.75, 0.05,   0,      0), "cm"))
      } else{
        theme(plot.margin=unit(c(-0.75, 0.05,   0,      -0.75), "cm"))
      }
    } else if (add_title){
      if (add_ytitle){
        #                     unit(c(top,  right, bottom, left), units)
        theme(plot.margin=unit(c(0,    0.05,   -0.75,   0), "cm"))
      } else{
        theme(plot.margin=unit(c(0,    0.05,   -0.75,   -0.75), "cm"))
      }
    } else {
      #                     unit(c(top,  right, bottom, left), units)
      if (add_ytitle){
        theme(plot.margin=unit(c(-0.75, 0.05,   -0.75,   0), "cm"))
      } else{
        theme(plot.margin=unit(c(-0.75, 0.05,   -0.75,   -0.75), "cm"))
      }
    }
    }+
    
    # Simple X axis labels
    scale_x_discrete(labels=days_seq)+
    
    
    {if(vers=="att"){
      scale_color_manual(name = "", values = c("Attention" = onecol,"Average Attention" ="black"),limits = c("Attention", "Average Attention"))
    }
    }+
    {if(vers=="sent"){
      scale_color_manual(name = "", values = c("Sentiment" = onecol,"Average Sentiment" ="black"),limits = c("Sentiment", "Average Sentiment"))
    }
    }+
    
    
    # scale_color_manual(name = "", values = c("Attention" = onecol,"Average Attention" ="black"))+
    guides(colour = guide_legend(override.aes = list(size=10,
                                                     linetype = c(1, 2),
                                                     shape = c(1, 2))))
  # scale_shape_manual(name = "", values = c("Attention" = 1,"Average Attention" =2))
  
  return(p)
}



# get legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# remove legends
remove_legend<-function(graph){
  new_graph<-graph + theme(legend.position="none")
  return(new_graph)
}
