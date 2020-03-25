library(deSolve)
library(reshape)
library(googlesheets4)
sheets_deauth()
source("code/functions.R")

# ----------------------------------------------------------------------------
# Load data:
# --------------------

HospDataAll=read_sheet("https://docs.google.com/spreadsheets/d/1zZKKnZ47lqfmUGYDQuWNnzKnh-IDMy15LBaRmrBcjqE/edit#gid=1585003222",sheet="Hospital capacity")
HospDataAll=na.omit(HospDataAll)
hdata = as.data.frame(t(data.frame(HospDataAll[,c("Name","Value")], row.names="Name")))


function(input, output, session) {
  
  # Plot timecourse of all variables
  
  output$plot0 = renderPlotly({
   
    sim=SimSEIR(input)
    
    out.df=sim$out.df
    N=sim$N
    Ro=sim$Ro
    r=sim$r
    DoublingTime=sim$DoublingTime
    
    #combine exposed classes for plotting
    out.df$E0=out.df$E0+out.df$E1
    out.df$E1=NULL
    out.df=rename(out.df, c(E0="E"))
    
    #reformat data for plotting
    out.df2=rename(out.df, c(S="Susceptible", E="Exposed", I0="Infected.NoSymptoms", I1="Infected.Mild", I2="Infected.Severe", I3="Infected.Critical", R="Recovered", D="Dead"))
    out=melt(out.df,id="time")
    out2=melt(out.df2,id="time")
    out$variableName=out2$variable
    out$variableLegend = paste0(out$variableName,' (',out$variable,')')
    out$variableLegend = factor(out$variableLegend, levels = unique(out[["variableLegend"]]))
    
    #create data for plotting total infected
    Comb.df=out.df
    Comb.df$I0=rowSums(out.df[,c("I0","I1","I2","I3")]) # create total infected
    Comb.df=rename(Comb.df, c(I0="I"))
    Comb.df$I1=NULL
    Comb.df$I2=NULL
    Comb.df$I3=NULL
    
    Comb.df2=rename(Comb.df, c(S="Susceptible", E="Exposed", I="Infected", R="Recovered", D="Dead"))
    Comb=melt(Comb.df,id="time")
    Comb2=melt(Comb.df2,id="time")
    Comb$variableName=Comb2$variable
    Comb$variableLegend = paste0(Comb$variableName,' (',Comb$variable,')')
    Comb$variableLegend = factor(Comb$variableLegend, levels = unique(Comb[["variableLegend"]]))
    
    
    #plot
    
    if(input$PlotCombine=="Yes"){
      p=plot_ly(data = Comb, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')
    }else{
      if(input$AllowAsym=="Yes"){
        p=plot_ly(data = out, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')
      }else{
        #don't want to show the I0 class in the plot
        outSym=out[out$variable!="I0",]
        p=plot_ly(data = outSym, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')
      }
    }
    
    p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per",formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscale),
             annotations=list(text=HTML(paste("R", tags$sub(0),'=',formatC(Ro,digits=2)," <br>r =", formatC(r,digits=2)," per day <br>T",tags$sub(2)," = ",formatC(DoublingTime,digits=2)," days")),
                              showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0.35, align="left"))
    
    if(input$AllowSeason=="Yes"){
      
      Ro.Season=sim$Ro.Season
      tpeak=365+input$seas.phase
      tmin=180+input$seas.phase
      #print(Ro.Season)
      #print(Ro.Season$Ro.now)
      p=layout(p,annotations=list(text=HTML(paste("Seasonal R", tags$sub(0), ": <br>Initial R", tags$sub(0),'=',formatC(Ro.Season$Ro.now,digits=2),"<br>Peak R", tags$sub(0),'=',formatC(Ro.Season$Ro.max,digits=2),"@day",formatC(tpeak,format = "f",digits=0),"<br>Min R", tags$sub(0),'=',formatC(Ro.Season$Ro.min,digits=2),"@day",formatC(tmin,format = "f",digits=0))),
                                  showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0.05, align="left"))
      
    }
    p

  })
  
  #Plot timecourse with an intervention
  
  output$plotInt = renderPlotly({
    
    sim=SimSEIR(input)
    
    out.df=sim$out.df
    N=sim$N
    Ro=sim$Ro
    r=sim$r
    DoublingTime=sim$DoublingTime
    
    #combine exposed classes for plotting
    out.df$E0=out.df$E0+out.df$E1
    out.df$E1=NULL
    out.df=rename(out.df, c(E0="E"))
    
    simInt=SimSEIRintB(input)
    
    outInt.df=simInt$out.df
    RoInt=simInt$Ro
    rInt=simInt$r
    DoublingTimeInt=simInt$DoublingTime
    
    #combine exposed classes for plotting
    outInt.df$E0=outInt.df$E0+outInt.df$E1
    outInt.df$E1=NULL
    outInt.df=rename(outInt.df, c(E0="E"))
    
    if(input$VarShowInt=="Inf"){
      
      out.df$value=rowSums(out.df[,c("E","I0","I1","I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("E","I0","I1","I2","I3")])
      
    }else if(input$VarShowInt=="Cases"){
      out.df$value=rowSums(out.df[,c("I1","I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("I1","I2","I3")])
      
    }else if(input$VarShowInt=="Hosp"){
      out.df$value=rowSums(out.df[,c("I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("I2","I3")])

    }else{

      out.df$value=out.df[,input$VarShowInt] # create observed variable
      outInt.df$value=outInt.df[,input$VarShowInt]
      
    }
    out.df$Intervention="Baseline" # add intervention column
    outInt.df$Intervention="Intervention"
    outAll.df=rbind(out.df,outInt.df) #combine baseline and intervention
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    
    p=plot_ly(data = outAll.sub, x=~time, y=~value, color=~Intervention, type='scatter', mode='lines',colors=c("#a50f15","#fc9272"))
    
    p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per", formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscaleInt),
             annotations=list(text=HTML(paste("Baseline: <br>R", tags$sub(0),'=',format(Ro,nsmall=1)," <br>r =", format(r,digits=2)," per day <br>T",tags$sub(2)," = ",format(DoublingTime,digits=1)," days <br><br>Intervention: <br>R", tags$sub(0),'=',RoInt,"<br>r =", format(rInt,digits=2)," per day <br>T",tags$sub(2)," = ",format(DoublingTimeInt,digits=1)," days")),
                              showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="top",y=0.8, align="left")
             )
    
    if(input$AllowSeason=="Yes"){
      
      Ro.Season=sim$Ro.Season
      RoInt.Season=simInt$Ro.Season
      tpeak=365+input$seas.phase
      tmin=180+input$seas.phase
      
      p=layout(p,annotations=list(text=HTML(paste0("Seasonal R", tags$sub(0), ": <br>Initial R", tags$sub(0),'=',formatC(Ro.Season$Ro.now,digits=2),"/",formatC(RoInt.Season$Ro.now,digits=2),"<br>Peak R", tags$sub(0),'=',formatC(Ro.Season$Ro.max,digits=2),"/",formatC(RoInt.Season$Ro.max,digits=2),"<br>  @day ",formatC(tpeak,format = "f",digits=0),"<br>Min R", tags$sub(0),'=',formatC(Ro.Season$Ro.min,digits=2),"/",formatC(RoInt.Season$Ro.min,digits=2),"<br>  @day ",formatC(tmin,format = "f",digits=0))),
                                  showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0, align="left"))
      
    }
    
    p
    
  })
  
  output$plotCap = renderPlotly({
    
    sim=SimSEIR(input)
    
    out.df=sim$out.df
    N=sim$N
    Ro=sim$Ro
    r=sim$r
    DoublingTime=sim$DoublingTime
    
    #combine exposed classes for plotting
    out.df$E0=out.df$E0+out.df$E1
    out.df$E1=NULL
    out.df=rename(out.df, c(E0="E"))
    
    simInt=SimSEIRintB(input)
    
    outInt.df=simInt$out.df
    RoInt=simInt$Ro
    rInt=simInt$r
    DoublingTimeInt=simInt$DoublingTime
    
    #combine exposed classes for plotting
    outInt.df$E0=outInt.df$E0+outInt.df$E1
    outInt.df$E1=NULL
    outInt.df=rename(outInt.df, c(E0="E"))
    
    Tmax=input$Tmax
    
    #subset the relevant variables and add in a column for capacity
    capParams=SetHospCapacity(input)

    if(input$VarShowCap=="I3mv"){
      
      out.df$value=out.df[,"I3"] # create observed variable
      outInt.df$value=outInt.df[,"I3"]
      out.df$Intervention="Baseline" # add intervention column
      outInt.df$Intervention="Intervention"
      outAll.df=rbind(out.df,outInt.df) #combine baseline and intervention
      outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
      outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
      outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
      
      capData=data.frame("time"=seq(0, Tmax, length.out = 1e3),"value"=rep(1,1e3)*capParams["ConvVentCap"]*(N/1000), "Intervention"="Conventional Mechanical \n Ventilator Capacity")
      combData=rbind(outAll.sub,capData)
      capData=data.frame("time"=seq(0, Tmax, length.out = 1e3),"value"=rep(1,1e3)*capParams["ContVentCap"]*(N/1000), "Intervention"="Contingency Mechanical \n Ventilator Capacity")
      combData=rbind(combData,capData)
      capData=data.frame("time"=seq(0, Tmax, length.out = 1e3),"value"=rep(1,1e3)*capParams["CrisisVentCap"]*(N/1000), "Intervention"="Crisis Mechanical \n Ventilator Capacity")
      combData=rbind(combData,capData)
      
      p=plot_ly(data = combData, x=~time, y=~value, color=~Intervention, linetype=~Intervention, type='scatter', mode='lines', colors=c("#a50f15","#fc9272","grey","grey","grey"), linetypes=c("solid","solid","dash","dashdot","dot"))
      
    }else if(input$VarShowCap=="I3bed"){
      
      out.df$value=out.df[,"I3"] # create observed variable
      outInt.df$value=outInt.df[,"I3"]
      out.df$Intervention="Baseline" # add intervention column
      outInt.df$Intervention="Intervention"
      outAll.df=rbind(out.df,outInt.df) #combine baseline and intervention
      outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
      outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
      outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
      
      capData=data.frame("time"=seq(0, Tmax, length.out = 1e3),"value"=rep(1,1e3)*capParams["AvailICUBeds"]*(N/1000), "Intervention"="Available ICU Beds")
      combData=rbind(outAll.sub,capData)
      
      p=plot_ly(data = combData, x=~time, y=~value, color=~Intervention, linetype=~Intervention, type='scatter', mode='lines', colors=c("#a50f15","#fc9272","grey"), linetypes=c("solid","solid","dash"))
      
    }else if(input$VarShowCap=="Hosp"){
      
      out.df$value=rowSums(out.df[,c("I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("I2","I3")])
      out.df$Intervention="Baseline" # add intervention column
      outInt.df$Intervention="Intervention"
      outAll.df=rbind(out.df,outInt.df) #combine baseline and intervention
      outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
      outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
      outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
      
      capData=data.frame("time"=seq(0, Tmax, length.out = 1e3),"value"=rep(1,1e3)*capParams["AvailHospBeds"]*(N/1000), "Intervention"="Available Hospital Beds")
      combData=rbind(outAll.sub,capData)
      
      p=plot_ly(data = combData, x=~time, y=~value, color=~Intervention, linetype=~Intervention, type='scatter', mode='lines', colors=c("#a50f15","#fc9272","grey"), linetypes=c("solid","solid","dash"))
      
    }else{ #CasesCap
      out.df$value=rowSums(out.df[,c("I1","I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("I1","I2","I3")])
      out.df$Intervention="Baseline" # add intervention column
      outInt.df$Intervention="Intervention"
      outAll.df=rbind(out.df,outInt.df) #combine baseline and intervention
      outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
      outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
      outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
      
      capData=data.frame("time"=seq(0, Tmax, length.out = 1e3),"value"=rep(1,1e3)*capParams["AvailHospBeds"]*(N/1000), "Intervention"="Available Hospital Beds")
      combData=rbind(outAll.sub,capData)
      
      p=plot_ly(data = combData, x=~time, y=~value, color=~Intervention, linetype=~Intervention, type='scatter', mode='lines', colors=c("#a50f15","#fc9272","grey"), linetypes=c("solid","solid","dash"))
      
    }
    
    
    p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per",formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscaleCap), 
             annotations=list(text=HTML(paste("Baseline: <br>R", tags$sub(0),'=',format(Ro,nsmall=1)," <br>r =", format(r,digits=2)," per day <br>T",tags$sub(2)," = ",format(DoublingTime,digits=1)," days <br><br>Intervention: <br>R", tags$sub(0),'=',RoInt,"<br>r =", format(rInt,digits=2)," per day <br>T",tags$sub(2)," = ",format(DoublingTimeInt,digits=1), " days")),
                              showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="top",y=0.5, align="left")
    )
    
    p
    
  })
  
  # Show the rate parameter values using an HTML table
  output$ParameterTable <-renderTable(
    formattedModelParameters(), hover = T,bordered = T,striped = F, digits=3
  )
  
  
  formattedModelParameters <- reactive({
    
    ParamStruct=GetModelParams(input)
    pModel=ParamStruct$pModel
    N=ParamStruct$N
    pModel.df=data.frame(as.list(pModel))
    pModel.df$N=N
    
    #If asymptomatic infection is allowed
    if(input$AllowAsym=="Yes"){
      pModel.df$b0=pModel.df$b0*N
      names(pModel.df)[names(pModel.df)=="b0"] <- "b0*N"
    }else{
      pModel.df$b0=NULL
      pModel.df$f=NULL
      pModel.df$g0=NULL
    }
    
    # If presymptomatic transmission is allowed
    if(input$AllowPresym=="Yes"){
      pModel.df$be=pModel.df$be*N
      names(pModel.df)[names(pModel.df)=="be"] <- "be*N"
    }else{
      pModel.df$be=NULL
      pModel.df$a1=NULL
      names(pModel.df)[names(pModel.df)=="a0"] <- "a"
    }
    
    pModel.df$b1=pModel.df$b1*N
    pModel.df$b2=pModel.df$b3*N
    pModel.df$b3=pModel.df$b3*N
    
    names(pModel.df)[names(pModel.df)=="b1"] <- "b1*N"
    names(pModel.df)[names(pModel.df)=="b2"] <- "b2*N"
    names(pModel.df)[names(pModel.df)=="b3"] <- "b3*N"
    
    if(input$AllowSeason=="Yes"){
      names(pModel.df)[names(pModel.df)=="seas.amp"] <- "Seasonal.Amplitude"
      names(pModel.df)[names(pModel.df)=="seas.phase"] <- "Seasonal.Phase"
    }else{
      pModel.df$seas.amp=NULL
      pModel.df$seas.phase=NULL
    }
    
    pModel.df=melt(pModel.df)
    colnames(pModel.df)[1]="Parameter"
    colnames(pModel.df)[2]="Value"
   
    pModel.df
    
  }) 
  
  # Show the early ratios of cases of different types using an HTML table
  output$RatioTable <-renderTable(
    formattedRatios(), hover = T,bordered = T,striped = F, digits=1
  )
  
  formattedRatios <- reactive({
    
    ParamStruct=GetModelParams(input)
    pModel=ParamStruct$pModel
    N=ParamStruct$N

    #r value and ratios
    r.out=Getr_SEIR(pModel,N)
    MaxEigenVector=r.out$MaxEigenVector
    MaxEigenVector=MaxEigenVector[1:length(MaxEigenVector)-1] #remove D:D
    
    ratios.df=data.frame(MaxEigenVector)
    colnames(ratios.df)[1]="Value"
    #ratios.df = subset(ratios.df, select=c(2,1))
    #
    ratios.df$Ratio=c("E0:D","E1:D","I0:D","I1:D","I2:D","I3:D","R:D")
    ratios.df = ratios.df[c(2,1)]
    ratios.df
    
    
  }) 
  
  # Display the model diagram
  output$plot4 <- renderImage({
    filename <- normalizePath(file.path('./images',"model_diagram.png"))
    
    list(src = filename, height=200, width=500)
    
  }, deleteFile = FALSE)
  
  
  output$parameterDesc <- renderUI({
    tags$iframe(src="Parameters.nb.html",width="100%",frameBorder="0",height="7000px")
  })
  
  output$Tutorial <- renderUI({
    tags$iframe(src="Tutorial.html",width="100%",frameBorder="0",height="5000px")
  })
  
  # Return the case fatality rate to the user as the % severe infections is changed
  
  output$CFR <- renderText({ 
    CFR=(input$ProbDeath/100)*(input$FracCritical)
    HTML(paste("<b> Case fatality ratio:</b>",CFR,"%"))
  })
  
  # ------------Set the sliders/forms that have dynamic values based on other sliders ----------------------
  
  #Get default hospital capacity parameters and create sliders 
  output$HospBedper <- renderUI({
    numericInput("HospBedper","Total (per 1000 ppl)",value = signif(hdata$HospBedper,digits=3), min = 0, step = 0.1)
  })
  output$HospBedOcc <- renderUI({
    numericInput("HospBedOcc","Occupancy (%)",value = signif(hdata$HospBedOcc,digits=3)*100, min = 0, max = 100, step = 0.1)
  })
  output$ICUBedper <- renderUI({
    numericInput("ICUBedper","Total (per 1000 ppl)",value = signif(hdata$ICUBedper,digits=3), min = 0, step = 0.01)
  })
  output$ICUBedOcc <- renderUI({
    numericInput("ICUBedOcc","Occupancy (%)",value = signif(hdata$ICUBedOcc,digits=3)*100, min = 0, max = 100, step = 1)
  })
  output$IncFluOcc <- renderUI({
    numericInput("IncFluOcc","Increased occupancy during flu season (%)",value = signif(hdata$IncFluOcc,digits=3)*100, min = 0, max = 100, step = 1)
  })
  output$ConvVentCap <- renderUI({
    numericInput("ConvMVCap","Conventional",value = signif(hdata$ConvMVCap,digits=3), min = 0, step = 0.01)
  })
  output$ContVentCap <- renderUI({
    numericInput("ContMVCap","Contingency",value = signif(hdata$ContMVCap,digits=3), min = 0, step = 0.01)
  })
  output$CrisisVentCap <- renderUI({
    numericInput("CrisisMVCap","Crisis",value = signif(hdata$CrisisMVCap,digits=3), min = 0, step = 0.01)
  })
  
  #make sure the fraction of individuals in each stage of infection sums to 100%
  observeEvent(input$FracSevere,  {
    maxFracCritical=100-input$FracSevere
    updateSliderInput(session = session, inputId = "FracCritical", max = maxFracCritical)
  })
  
  #Make sure the part of the incubation period that leads to transmission is less than total incubation period
  observeEvent(input$IncubPeriod,  {
    maxPresymPeriod=input$IncubPeriod
    updateSliderInput(session = session, inputId = "PresymPeriod", max = maxPresymPeriod)
  })
  
  #Make sure the intervention doesn't end before it starts, and doesn't end after total simulation time
  #Just do for Intervention tab, Capacity tab will copy these values
  observeEvent(input$Tint,  {
    updateSliderInput(session = session, inputId = "Tend", min = input$Tint)
    #updateSliderInput(session = session, inputId = "TendC", min = input$Tint)
  })
  observeEvent(input$Tmax,  {
    updateSliderInput(session = session, inputId = "Tend", max = input$Tmax)
    #updateSliderInput(session = session, inputId = "TendC", max = input$Tmax)
    
    # if(input$Tmax<input$Tend){
    #   updateSliderInput(session = session, inputId = "Tend", value = input$Tmax)
    # }
    # if(input$Tmax<input$TendC){
    #   updateSliderInput(session = session, inputId = "TendC", value = input$Tmax)
    # }
    
  })
  
  #Update intervention sliders on capacity tab to match intervention tab
  observeEvent(input$Tint,  {
    updateSliderInput(session = session, inputId = "TintC", value = input$Tint)
  })
  observeEvent(input$Tend,  {
    updateSliderInput(session = session, inputId = "TendC", value = input$Tend)
  })
  observeEvent(input$s1,  {
    updateSliderInput(session = session, inputId = "s1C", value = input$s1)
  })
  observeEvent(input$s2,  {
    updateSliderInput(session = session, inputId = "s2C", value = input$s2)
  })
  observeEvent(input$s3,  {
    updateSliderInput(session = session, inputId = "s3C", value = input$s3)
  })
  observeEvent(input$s0,  {
    updateSliderInput(session = session, inputId = "s0C", value = input$s0)
  })
  
  #And vice versa
  
  observeEvent(input$TintC,  {
    updateSliderInput(session = session, inputId = "Tint", value = input$TintC)
  })
  observeEvent(input$TendC,  {
    updateSliderInput(session = session, inputId = "Tend", value = input$TendC)
  })
  observeEvent(input$s1C,  {
    updateSliderInput(session = session, inputId = "s1", value = input$s1C)
  })
  observeEvent(input$s2C,  {
    updateSliderInput(session = session, inputId = "s2", value = input$s2C)
  })
  observeEvent(input$s3C,  {
    updateSliderInput(session = session, inputId = "s3", value = input$s3C)
  })
  observeEvent(input$s0C,  {
    updateSliderInput(session = session, inputId = "s0", value = input$s0C)
  })

  # Reset all parameters if the RESET button is pushed
  observeEvent(input$reset,{
    updateSliderInput(session,'IncubPeriod',value = 5)
    updateSliderInput(session,'DurMildInf',value = 6)
    updateSliderInput(session,'FracSevere',value = 15)
    updateSliderInput(session,'FracCritical',value = 6)
    updateSliderInput(session,'ProbDeath',value = 40)
    updateSliderInput(session,'DurHosp',value = 6)
    updateSliderInput(session,'TimeICUDeath',value = 8)
    updateSliderInput(session,'b1',value = 0.5)
    updateSliderInput(session,'b2',value = 0.1)
    updateSliderInput(session,'b3',value = 0.1)
    updateSliderInput(session,'N',value = 1000)
    updateSliderInput(session,'Tmax',value = 300)
    updateSliderInput(session,'InitInf',value = 1)
  })
   
}