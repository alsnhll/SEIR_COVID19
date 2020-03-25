library(deSolve)
library(plotly)
library(dplyr)
library(reshape)
library(htmltools)
source("code/functions.R")

# Set parameters
# (in Shiny app these are input by sliders)

IncubPeriod= 5 #Duration of incubation period
DurMildInf= 6 #Duration of mild infections
FracSevere= 15 #% of symptomatic infections that are severe
FracCritical= 5 #% of symptomatic infections that are critical
ProbDeath= 40 #Death rate for critical infections
DurHosp= 5 #Duration of severe infection/hospitalization
TimeICUDeath= 8 #Duration critical infection/ICU stay

AllowPresym="No"
AllowAsym="No"
FracAsym=25 #Fraction of all infections that are asymptomatic
PresymPeriod=2 #Length of infectious phase of incubation period
DurAsym=6 #Duration of asympatomatic infection

b1= 0.5 #Transmission rate (mild infections)
b2= 0.1 #Transmission rate (severe infections)
b3= 0.1 #Transmission rate (critical infections)
be = 0.5 #Transmission rate (pre-symptomatic)
b0 = 0.5 #Transmission rate (asymptomatic infections)
N= 1000 #Total population size
Tmax= 300 #Initial # infected
InitInf= 1 #Maximum time
yscale="linear"

AllowSeason="No"
seas.amp=0.0 #relative amplitude of seasonal fluctuations, in [0,1]
seas.phase=0 #phase of seasonal fluctuations, measuered in days relative to time zero when peak will occur (0=peak occurs at time zero, 30 = peak occurs one month after time zero). Can be negative
#Note that the beta values chosen above will be those for time zero, wherever that occurs in the seasonality of things.

#Intervention parameters
VarShowInt="E" #This is the variable to be plotted. Options "E0", "E1","I0", "I1", "I2", "I3", "R", "D", "Suceptible (S)"="S", "Exposed (E)"="E", "Mild Infections (I1)"="I1", "Severe Infections (I2)"="I2", "Critical Infections (I3)"="I3", "Recovered (R)"="R", "Dead (D)"="D", "All infected (E + all I)"="Inf","All symptomatic (I1+I2+I3)"="Cases","All hospitalized (I2+I3)"="Hosp"),
Tint = 30 #Intervention start time (days)
Tend = 300 #Intervention end time (days)
s0=0 #Reduction in transmission from asymptomatic/presymptomatic infections
s1=30 #Reduction in transmission from mild infections
s2=0 #Reduction in transmission from severe infections
s3 =0 #Reduction in transmission rate from critical infections
RoundOne = FALSE #Round values to nearest integar post-intervention?"

#Put these into an input structure
input=list("IncubPeriod"=IncubPeriod,"DurMildInf"=DurMildInf,"FracSevere"=FracSevere,"FracCritical"=FracCritical,"ProbDeath"=ProbDeath,"DurHosp"=DurHosp,"TimeICUDeath"=TimeICUDeath,"FracAsym"=FracAsym, "PresymPeriod"=PresymPeriod, "DurAsym"=DurAsym, "be"=be,"b0"=b0,"b1"=b1,"b2"=b2,"b3"=b3,"seas.amp"=seas.amp, "seas.phase"=seas.phase,"N"=N,"Tmax"=Tmax,"InitInf"=InitInf,"yscale"=yscale,"AllowPresym"=AllowPresym,"AllowAsym"=AllowAsym,"AllowSeason"=AllowSeason,"VarShowInt"=VarShowInt,"Tint"=Tint,"Tend"=Tend,"s0"=s0,"s1"=s1,"s2"=s2,"s3"=s3,"RoundOne"=RoundOne)

CFR=(input$ProbDeath/100)*(input$FracCritical) #Case fatality rate

# Run simulations

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
  
  out.df$value=rowSums(out.df[,c("E","I0", "I1","I2","I3")]) # create observed variable
  outInt.df$value=rowSums(outInt.df[,c("E","I0", "I1","I2","I3")])
  
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

if(input$seas.amp>0){
  
  Ro.Season=sim$Ro.Season
  RoInt.Season=simInt$Ro.Season
  tpeak=365+input$seas.phase
  tmin=180+input$seas.phase
  
  p=layout(p,annotations=list(text=HTML(paste0("Seasonal R", tags$sub(0), ": <br>Initial R", tags$sub(0),'=',formatC(Ro.Season$Ro.now,digits=2),"/",formatC(RoInt.Season$Ro.now,digits=2),"<br>Peak R", tags$sub(0),'=',formatC(Ro.Season$Ro.max,digits=2),"/",formatC(RoInt.Season$Ro.max,digits=2),"<br>  @day ",formatC(tpeak,format = "f",digits=0),"<br>Min R", tags$sub(0),'=',formatC(Ro.Season$Ro.min,digits=2),"/",formatC(RoInt.Season$Ro.min,digits=2),"<br>  @day ",formatC(tmin,format = "f",digits=0))),
                              showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0, align="left"))
  
}

p

