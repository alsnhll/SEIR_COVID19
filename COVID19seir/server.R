library(deSolve)
library(reshape)
library(googlesheets4)
sheets_deauth()

# ----------------------------------------------------------------------------
# Load data:
# --------------------

HospDataAll=read_sheet("https://docs.google.com/spreadsheets/d/1zZKKnZ47lqfmUGYDQuWNnzKnh-IDMy15LBaRmrBcjqE/edit#gid=1585003222",sheet="Hospital capacity")
HospDataAll=na.omit(HospDataAll)
hdata = as.data.frame(t(data.frame(HospDataAll[,c("Name","Value")], row.names="Name")))

# ----------------------------------------------------------------------------
# SetODEs_SEIR function:
# --------------------

SetODEs_SEIR=function(t,y,p){
  S = y[1]
  E = y[2]
  I1 = y[3]
  I2 = y[4]
  I3 = y[5]
  R = y[6]
  D = y[7]
  
  with(as.list(p),{
    
    dS.dt = -(b1*I1+b2*I2+b3*I3)*S
    dE.dt=(b1*I1+b2*I2+b3*I3)*S-a*E
    dI1.dt=a*E-g1*I1-p1*I1
    dI2.dt=p1*I1-g2*I2-p2*I2
    dI3.dt=p2*I2-g3*I3-u*I3
    dR.dt=g1*I1+g2*I2+g3*I3
    dD.dt=u*I3
    
    return(list(c(dS.dt, dE.dt, dI1.dt, dI2.dt, dI3.dt, dR.dt, dD.dt)))
  })
}

# ----------------------------------------------------------------------------
# GetSpread_SEIR function:
# --------------------
GetSpread_SEIR = function(pModel,N,Tmax,y0){
  
  t = seq(from=0, to=Tmax, by=1)
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out = ode(y=y0, times=t, func=SetODEs_SEIR, parms=pModel)
  
  df = as.data.frame(out)
  
  return(df)
}

# ----------------------------------------------------------------------------
# GetParams_SEIR function:
# --------------------

GetParams_SEIR = function(pClin){
  
  with(as.list(pClin),{
    
    a=1/IncubPeriod
    
    g1=(1/DurMildInf)*FracMild
    p1=(1/DurMildInf)-g1
    
    p2=(1/DurHosp)*(FracCritical/(FracSevere+FracCritical))
    g2=(1/DurHosp)-p2
    
    if(FracCritical==0){
      u=0
    }else{
      u=(1/TimeICUDeath)*(CFR/FracCritical)
    }

    g3=(1/TimeICUDeath)-u
    
    return(c(a=a,g1=g1,g2=g2,g3=g3,p1=p1,p2=p2,u=u))
  })
  
}

GetModelParams = function(input){
  
  IncubPeriod=input$IncubPeriod  #Incubation period, days
  DurMildInf=input$DurMildInf #Duration of mild infections, days
  FracSevere=input$FracSevere/100 #Fraction of infections that are severe
  FracCritical=input$FracCritical/100 #Fraction of infections that are critical
  FracMild=1-FracSevere-FracCritical  #Fraction of infections that are mild
  ProbDeath=input$ProbDeath  #Probability of dying given critical infection
  CFR=ProbDeath*FracCritical/100 #Case fatality rate (fraction of infections resulting in death)
  TimeICUDeath=input$TimeICUDeath #Time from ICU admission to death, days
  DurHosp=input$DurHosp #Duration of hospitalization, days
  
  pClin=c(IncubPeriod=IncubPeriod, DurMildInf=DurMildInf,FracMild=FracMild, FracSevere=FracSevere,FracCritical=FracCritical,CFR=CFR,TimeICUDeath=TimeICUDeath,DurHosp=DurHosp)

  pModel=GetParams_SEIR(pClin)
  
  N=10^(input$LogN)
  
  b1=input$b1/N
  b2=input$b21*b1
  b3=input$b31*b1
  b=c(b1,b2,b3)
  pModel=c(b=b,pModel)
  
  return(list("N"=N,"pModel"=pModel))
  
}

GetRo_SEIR = function(p,N){
  
  with(as.list(p),{
    
    Ro=N*((b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3))))

    return(Ro)
  })
  
}


Getr_SEIR = function(out,t1,t2,V){
  
  outV=out[out$variable==V,]

  value1=outV$value[which.min(abs(t1-outV$time))]
  value2=outV$value[which.min(abs(t2-outV$time))]

  r=(log(value2)-log(value1))/(t2-t1)

  DoublingTime=log(2)/r
  
  return(list("r"=r,"DoublingTime"=DoublingTime))

}

SetHospCapacity=function(){

  AvailHospBeds=hdata$HospBedper*(1-hdata$HospBedOcc*(1+hdata$IncFluOcc)) #Available hospital beds per 1000 ppl in US based on total beds and occupancy
  AvailICUBeds=hdata$ICUBedper*(1-hdata$ICUBedOcc*(1+hdata$IncFluOcc)) #Available ICU beds per 1000 ppl in US, based on total beds and occupancy. Only counts adult not neonatal/pediatric beds
  ConvVentCap=hdata$ConvMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using conventional protocols
  ContVentCap=hdata$ContMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using contingency protocols
  CrisisVentCap=hdata$CrisisMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using crisis protocols
  
  capParams=c("AvailHospBeds"=AvailHospBeds,"AvailICUBeds"=AvailICUBeds,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap)

  return(capParams)
}

SetHospCapacitySliders=function(input){
  
  AvailHospBeds=input$HospBedper*(100-input$HospBedOcc*(1+input$IncFluOcc/100))/100 #Available hospital beds per 1000 ppl in US based on total beds and occupancy
  AvailICUBeds=input$ICUBedper*(100-input$ICUBedOcc*(1+input$IncFluOcc/100))/100 #Available ICU beds per 1000 ppl in US, based on total beds and occupancy. Only counts adult not neonatal/pediatric beds
  ConvVentCap=input$ConvMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using conventional protocols
  ContVentCap=input$ContMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using contingency protocols
  CrisisVentCap=input$CrisisMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using crisis protocols
  
  capParams=c("AvailHospBeds"=AvailHospBeds,"AvailICUBeds"=AvailICUBeds,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap)

    return(capParams)
}

function(input, output, session) {
  observeEvent(input$reset,{
    updateSliderInput(session,'IncubPeriod',value = 5)
    updateSliderInput(session,'DurMildInf',value = 6)
    updateSliderInput(session,'FracSevere',value = 15)
    updateSliderInput(session,'FracCritical',value = 5)
    updateSliderInput(session,'ProbDeath',value = 40)
    updateSliderInput(session,'DurHosp',value = 4)
    updateSliderInput(session,'TimeICUDeath',value = 10)
    updateSliderInput(session,'b1',value = 0.33)
    updateSliderInput(session,'b21',value = 0)
    updateSliderInput(session,'b31',value = 0)
    updateSliderInput(session,'LogN',value = 3)
    updateSliderInput(session,'Tmax',value = 300)
    updateSliderInput(session,'InitInf',value = 1)
  })
  
  # Plot timecourse of all variables
  
  output$plot0 = renderPlotly({

    ParamStruct=GetModelParams(input)
    pModel=ParamStruct$pModel
    N=ParamStruct$N
    Tmax=input$Tmax
    
    # Set initial conditions and time interval
    E0=input$InitInf
    S0 = N-E0
    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
 
    #get Ro value
    Ro=GetRo_SEIR(pModel,N)

    #run ODEs
    out.df=GetSpread_SEIR(pModel,N,Tmax,y0)
    out.df2=rename(out.df, c(S="Susceptible",E="Exposed", I1="Infected.Mild", I2="Infected.Severe", 
                              I3="Infected.Critical", R="Recovered", D="Dead"))
    out=melt(out.df,id="time")
    out2=melt(out.df2,id="time")
    out$variableName=out2$variable
    out$variableLegend = paste0(out$variableName,' (',out$variable,')')
    out$variableLegend = factor(out$variableLegend, levels = unique(out[["variableLegend"]]))
    #out$value[out$value<1.0]=0
    
    #get r value
    V="E" #variable to calculate r for
    tpeak=out.df[which.max(select(out.df,"time",V)[,2]),"time"];

    t2=tpeak/4
    t1=tpeak/8
    r.out=Getr_SEIR(out,t1,t2,V)
    r=r.out$r
    DoublingTime=r.out$DoublingTime
    
    p=plot_ly(data = out, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')

    p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per",formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscale),
             annotations=list(text=HTML(paste("R", tags$sub(0),'=',format(Ro,nsmall=1)," <br>r =", format(r,digits=2)," per day <br>T",tags$sub(2)," = ",format(DoublingTime,digits=1)," days")),
                              showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0.4, align="left"))

  })
  
  #Plot timecourse with an intervention
  
  output$plotInt = renderPlotly({
    
    ParamStruct=GetModelParams(input)
    pModel=ParamStruct$pModel
    N=ParamStruct$N
    Tmax=input$Tmax
    
    # Set initial conditions and time interval
    E0=input$InitInf
    S0 = N-E0
    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
    
    #get Ro value
    Ro=GetRo_SEIR(pModel,N)
    
    out.df=GetSpread_SEIR(pModel,N,Tmax,y0)
    out=melt(out.df,id="time")

    #get r value
    V="E" #variable to calculate r for
    tpeak=out.df[which.max(select(out.df,"time",V)[,2]),"time"];
    
    t2=tpeak/4
    t1=tpeak/8
    r.out=Getr_SEIR(out,t1,t2,V)
    r=r.out$r
    DoublingTime=r.out$DoublingTime
    
    # INTERVENTION
    
    # intervention parameters
    pModelInt=pModel
    pModelInt["b1"]=pModelInt["b1"]*(1-input$s1/100)
    pModelInt["b2"]=pModelInt["b2"]*(1-input$s2/100)
    pModelInt["b3"]=pModelInt["b3"]*(1-input$s3/100)
    
    RoInt=GetRo_SEIR(pModelInt,N)
    
    # start time of intervention
    
    Tint=input$Tint
    Tend=input$Tend
    
    validate(
      need(Tint < Tmax, "Must have intervention time less that total simulation time")
    )
    
    iInt=which.min(abs(Tint-out$time)) # find nearest time to Tint

    # Set initial conditions and time interval
    S0 = out.df[iInt,"S"]
    E0 = out.df[iInt,"E"]
    I10 = out.df[iInt,"I1"]
    I20 = out.df[iInt,"I2"]
    I30 = out.df[iInt,"I3"]
    D0 = out.df[iInt,"D"]
    R0 = out.df[iInt,"R"]
    y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
    
    #Run intervention time course until Tend. Up to time Tint, use baseline solution
    Trun=Tend-Tint

    outInt.df=GetSpread_SEIR(pModelInt,N,Trun,y0)
    outInt.df$time=outInt.df$time+Tint
    
    #--After intervention ends, run with regular parameaters up to time Tmax
    Trun2=Tmax-Tend

    if(Trun2==0){
    }else{
      #Set initial conditions and time interval
      #Round all numbers to lowest intergar, so if less than 1, go to zero
      iEnd=nrow(outInt.df)
      
      if(input$RoundOne=="True"){
      S0 = round(outInt.df[iEnd,"S"])
      E0 = round(outInt.df[iEnd,"E"])
      I10 = round(outInt.df[iEnd,"I1"])
      I20 = round(outInt.df[iEnd,"I2"])
      I30 = round(outInt.df[iEnd,"I3"])
      D0 = round(outInt.df[iEnd,"D"])
      R0 = round(outInt.df[iEnd,"R"])
      }else{
        S0 = outInt.df[iEnd,"S"]
        E0 = outInt.df[iEnd,"E"]
        I10 = outInt.df[iEnd,"I1"]
        I20 = outInt.df[iEnd,"I2"]
        I30 = outInt.df[iEnd,"I3"]
        D0 = outInt.df[iEnd,"D"]
        R0 = outInt.df[iEnd,"R"]
      }
    
      y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
      #run with parameters back to baseline
      outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
      outIntOff.df$time=outIntOff.df$time+Tend

      #combine vectors
      outInt.df=rbind(outInt.df,outIntOff.df)
    }
    

    if(input$VarShowInt=="Inf"){
      
      out.df$value=rowSums(out.df[,c("E", "I1","I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("E", "I1","I2","I3")])
      
    }else if(input$VarShowInt=="Cases"){
      out.df$value=rowSums(out.df[,c("I1","I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("I1","I2","I3")])
      
    }else if(input$VarShowInt=="Hosp"){
      out.df$value=rowSums(out.df[,c("I2","I3")]) # create observed variable
      outInt.df$value=rowSums(outInt.df[,c("I2","I3")])
      out.df$Intervention="Baseline" # add intervention column

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
    
    #get r value for intervention
    #note, to do this need to simulate from beginning, just up to peak of epidemic without intervention
    
    E0=input$InitInf
    S0 = N-E0
    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
    outIntZero.df=GetSpread_SEIR(pModelInt,N,tpeak,y0)
    outIntZero=melt(outIntZero.df,id="time")
    
    tpeakInt=outIntZero.df[which.max(select(outIntZero.df,"time",V)[,2]),"time"];
    
    t2=tpeakInt/4
    t1=tpeakInt/8
    r.out=Getr_SEIR(outIntZero,t1,t2,V)
    rInt=r.out$r
    DoublingTimeInt=r.out$DoublingTime
    
    p=plot_ly(data = outAll.sub, x=~time, y=~value, color=~Intervention, type='scatter', mode='lines',colors=c("#a50f15","#fc9272"))
    
    p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per", formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscaleInt),
             annotations=list(text=HTML(paste("Baseline: <br>R", tags$sub(0),'=',format(Ro,nsmall=1)," <br>r =", format(r,digits=2)," per day <br>T",tags$sub(2)," = ",format(DoublingTime,digits=1)," days <br><br>Intervention: <br>R", tags$sub(0),'=',RoInt,"<br>r =", format(rInt,digits=2)," per day <br>T",tags$sub(2)," = ",format(DoublingTimeInt,digits=1)," days")),
                              showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="top",y=0.5, align="left")
             )
    
  })
  
  output$plotCap = renderPlotly({
 
    ParamStruct=GetModelParams(input)
    pModel=ParamStruct$pModel
    N=ParamStruct$N
    Tmax=input$Tmax
    
    # Set initial conditions and time interval
    E0=input$InitInf
    S0 = N-E0
    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
    
    #get Ro value
    Ro=GetRo_SEIR(pModel,N)
    
    out.df=GetSpread_SEIR(pModel,N,Tmax,y0)
    out=melt(out.df,id="time")
    
    #get r value
    V="E" #variable to calculate r for
    tpeak=out.df[which.max(select(out.df,"time",V)[,2]),"time"];
    
    t2=tpeak/4
    t1=tpeak/8
    r.out=Getr_SEIR(out,t1,t2,V)
    r=r.out$r
    DoublingTime=r.out$DoublingTime
    
    # INTERVENTION
    
    # intervention parameters
    pModelInt=pModel
    pModelInt["b1"]=pModelInt["b1"]*(1-input$s1C/100)
    pModelInt["b2"]=pModelInt["b2"]*(1-input$s2C/100)
    pModelInt["b3"]=pModelInt["b3"]*(1-input$s3C/100)
    
    RoInt=GetRo_SEIR(pModelInt,N)
    
    # start time of intervention
    
    Tint=input$TintC
    Tend=input$TendC
    
    validate(
      need(Tint < Tmax, "Must have intervention time less that total simulation time")
    )
    
    iInt=which.min(abs(Tint-out$time)) # find nearest time to Tint
    
    # Set initial conditions and time interval
    S0 = out.df[iInt,"S"]
    E0 = out.df[iInt,"E"]
    I10 = out.df[iInt,"I1"]
    I20 = out.df[iInt,"I2"]
    I30 = out.df[iInt,"I3"]
    D0 = out.df[iInt,"D"]
    R0 = out.df[iInt,"R"]
    y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
    
    #Run intervention time course until Tend. Up to time Tint, use baseline solution
    Trun=Tend-Tint
    
    outInt.df=GetSpread_SEIR(pModelInt,N,Trun,y0)
    outInt.df$time=outInt.df$time+Tint
    
    #--After intervention ends, run with regular parameaters up to time Tmax
    Trun2=Tmax-Tend
    
    if(Trun2==0){
    }else{
      #Set initial conditions and time interval
      #Round all numbers to lowest intergar, so if less than 1, go to zero
      iEnd=nrow(outInt.df)
      if(input$RoundOneCap=="True"){
        S0 = round(outInt.df[iEnd,"S"])
        E0 = round(outInt.df[iEnd,"E"])
        I10 = round(outInt.df[iEnd,"I1"])
        I20 = round(outInt.df[iEnd,"I2"])
        I30 = round(outInt.df[iEnd,"I3"])
        D0 = round(outInt.df[iEnd,"D"])
        R0 = round(outInt.df[iEnd,"R"])
      }else{
        S0 = outInt.df[iEnd,"S"]
        E0 = outInt.df[iEnd,"E"]
        I10 = outInt.df[iEnd,"I1"]
        I20 = outInt.df[iEnd,"I2"]
        I30 = outInt.df[iEnd,"I3"]
        D0 = outInt.df[iEnd,"D"]
        R0 = outInt.df[iEnd,"R"]
      }
      y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
      #run with parameters back to baseline
      outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
      outIntOff.df$time=outIntOff.df$time+Tend
      
      #combine vectors
      outInt.df=rbind(outInt.df,outIntOff.df)
    }
    
    #get r value for intervention
    #note, to do this need to simulate from beginning, just up to peak of epidemic without intervention
    
    E0=input$InitInf
    S0 = N-E0
    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
    outIntZero.df=GetSpread_SEIR(pModelInt,N,tpeak,y0)
    outIntZero=melt(outIntZero.df,id="time")
    
    tpeakInt=outIntZero.df[which.max(select(outIntZero.df,"time",V)[,2]),"time"];
    
    t2=tpeakInt/4
    t1=tpeakInt/8
    r.out=Getr_SEIR(outIntZero,t1,t2,V)
    rInt=r.out$r
    DoublingTimeInt=r.out$DoublingTime
    
    #subset the relevant variables and add in a column for capacity
    capParams=SetHospCapacitySliders(input)

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
      outInt.df$value=outInt.df[,input$VarShowInt]
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
    
  })
  
  # Show the values using an HTML table
  output$ParameterTable <-renderTable(
    formattedModelParameters(), hover = T,bordered = T,striped = F, digits=3
  )
  
  
  formattedModelParameters <- reactive({
    
    ParamStruct=GetModelParams(input)
    pModel=ParamStruct$pModel
    N=ParamStruct$N
    pModel.df=data.frame(as.list(pModel))
    pModel.df$N=N
    
    pModel.df$b1=pModel.df$b1*N
    pModel.df$b2=pModel.df$b3*N
    pModel.df$b3=pModel.df$b3*N
    names(pModel.df)[names(pModel.df)=="b1"] <- "b1*N"
    names(pModel.df)[names(pModel.df)=="b2"] <- "b2*N"
    names(pModel.df)[names(pModel.df)=="b3"] <- "b3*N"
    
    pModel.df=melt(pModel.df)
    colnames(pModel.df)[2]="value (/day)"
    pModel.df
    
  }) 
  
  output$plot4 <- renderImage({
    filename <- normalizePath(file.path('./images',"model_diagram.png"))
    
    list(src = filename, height=200, width=500)
    
  }, deleteFile = FALSE)
  
  url = a("GitHub", href="https://github.com/alsnhll/SEIR_COVID19")
  output$tab = renderUI({
    tagList("Rscripts used to make this R Shiny web application are available on", url,". Contact Alison Hill alhill@fas.harvard.edu with questions. Thanks to Anjalika Nande, Andrei Gheorghe, Ski Krieger, Sherrie Xie, and Mike Levy for feedback on early versions of this tool.")

  })
  
  output$parameterDesc <- renderUI({
    tags$iframe(src="Parameters.nb.html",width="100%",frameBorder="0",height="5000px")
  })
  
  output$CFR <- renderText({ 
    CFR=(input$ProbDeath/100)*(input$FracCritical)
    HTML(paste("<b> Case fatality ratio:</b>",CFR,"%"))
  })
  
  output$N <- renderText({ 
    N=round(10^(input$LogN))
    HTML(paste("<b> N = </b>",formatC(N,big.mark=",",format="f",digits=0),""))
  })
  
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
  
  #Make sure the intervention doesn't end before it starts, and doesn't end after total simulation time
  observeEvent(input$Tint,  {
    updateSliderInput(session = session, inputId = "Tend", min = input$Tint)
    updateSliderInput(session = session, inputId = "TendC", min = input$Tint)
  })
  observeEvent(input$Tmax,  {
    updateSliderInput(session = session, inputId = "Tend", max = input$Tmax)
    updateSliderInput(session = session, inputId = "TendC", max = input$Tmax)
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

  
}