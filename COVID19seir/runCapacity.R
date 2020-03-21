library(deSolve)
library(plotly)
library(dplyr)
library(reshape)
library(htmltools)
library(googlesheets4)
sheets_deauth()
source("code/functions.R")

# Set parameters
# (in Shiny app these are input by sliders)

IncubPeriod= 5 #Duration of incubation period
DurMildInf= 6 #Duration of mild infections
FracSevere= 15 #% of infections that are severe
FracCritical= 5 #% of infections that are critical
ProbDeath= 40 #Death rate for critical infections
DurHosp= 4 #Duration of severe infection/hospitalization
TimeICUDeath= 10 #Duration critical infection/ICU stay
b1= 0.33 #Transmission rate (mild infections)
b21= 0 #Transmission rate (severe infections, relative to mild)
b31= 0 #Transmission rate (critical infections, relative to mild)
LogN= 3 #Total population size (log10)
Tmax= 300 #Initial # infected
InitInf= 1 #Maximum time
yscale="linear"

#Intervention parameters
VarShowCap="Hosp" # This is the variable to be plotted. Options: "Critical Infections (I3) vs ICU beds"="I3bed", "Critical Infections (I3) vs ventilator capacity"="I3mv", "Severe + Critical Infections (I2+I3) vs Hospital Beds"="Hosp", "All symptomatic cases (I1+I2+I3) vs Hospital Beds"="CasesCap"
TintC = 0 #Intervention start time (days)
TendC = 300 #Intervention end time (days)
s1C=30 #Reduction in transmission from mild infections
s2C=0 #Reduction in transmission from severe infections
s3C = 0 #Reduction in transmission rate from critical infections
RoundOneCap = TRUE #Round values to nearest integar post-intervention?"

#Capacity parameters (US)
HospBedper=2.8 #Total hospital beds per 1000 ppl 
HospBedOcc=66 # average % occupancy of hospital beds
ICUBedper=0.26 # Total ICU beds per 1000 ppl
ICUBedOcc=68 # average % occupancy of hospital beds
IncFluOcc=10 # increase in occupancy during flu season
ConvVentCap=0.062 # conventional capacity for mechanical ventilation, per 1000 ppl
ContVentCap=0.155 # contingency capacity for mechanical ventilation, per 1000 ppl
CrisisVentCap=0.418 # crisis capacity for mechanical ventilation, per 1000 ppl

#Put these into an input structure
input=list("IncubPeriod"=IncubPeriod,"DurMildInf"=DurMildInf,"FracSevere"=FracSevere,"FracCritical"=FracCritical,"ProbDeath"=ProbDeath,"DurHosp"=DurHosp,"TimeICUDeath"=TimeICUDeath,"b1"=b1,"b21"=b21,"b31"=b31,"LogN"=LogN,"Tmax"=Tmax,"InitInf"=InitInf,"yscale"=yscale,"VarShowCap"=VarShowCap,"TintC"=TintC,"Tend"=TendC,"s1C"=s1C,"s2C"=s2C,"s3C"=s3C,"RoundOneCap"=RoundOneCap,"HospBedper"=HospBedper,"HospBedOcc"=HospBedOcc,"ICUBedper"=ICUBedper,"ICUBedOcc"=ICUBedOcc,"IncFluOcc"=IncFluOcc,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap)

CFR=(input$ProbDeath/100)*(input$FracCritical) #Case fatality rate

# Run simulations

sim=SimSEIR(input)

out.df=sim$out.df
N=sim$N
Ro=sim$Ro
r=sim$r
DoublingTime=sim$DoublingTime

simInt=SimSEIRintB(input)

outInt.df=simInt$out.df
RoInt=simInt$Ro
rInt=simInt$r
DoublingTimeInt=simInt$DoublingTime

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
         annotations=list(text=HTML(paste("Baseline: <br>R", tags$sub(0),'=',formatC(Ro,digits=2)," <br>r =", formatC(r,digits=2)," per day <br>T",tags$sub(2)," = ",formatC(DoublingTime,digits=2)," days <br><br>Intervention: <br>R", tags$sub(0),'=',formatC(RoInt,digits=2),"<br>r =", formatC(rInt,digits=2)," per day <br>T",tags$sub(2)," = ",formatC(DoublingTimeInt,digits=2), " days")),
                          showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="top",y=0.5, align="left")
)
p