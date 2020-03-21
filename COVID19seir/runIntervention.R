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
VarShowInt="Cases" #This is the variable to be plotted. Pptions "E", "I1", "I2", "I3", "R", "D", "All infected (E+I1+I2+I3)"="Inf","All symptomatic (I1+I2+I3)"="Cases","All hospitalized (I2+I3)"="Hosp"
Tint = 0 #Intervention start time (days)
Tend = 300 #Intervention end time (days)
s1=30 #Reduction in transmission from mild infections
s2=0 #Reduction in transmission from severe infections
s3 = 0 #Reduction in transmission rate from critical infections
RoundOne = TRUE #Round values to nearest integar post-intervention?"

#Put these into an input structure
input=list("IncubPeriod"=IncubPeriod,"DurMildInf"=DurMildInf,"FracSevere"=FracSevere,"FracCritical"=FracCritical,"ProbDeath"=ProbDeath,"DurHosp"=DurHosp,"TimeICUDeath"=TimeICUDeath,"b1"=b1,"b21"=b21,"b31"=b31,"LogN"=LogN,"Tmax"=Tmax,"InitInf"=InitInf,"yscale"=yscale,"VarShowInt"=VarShowInt,"Tint"=Tint,"Tend"=Tend,"s1"=s1,"s2"=s2,"s3"=s3,"RoundOne"=RoundOne)

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


p=plot_ly(data = outAll.sub, x=~time, y=~value, color=~Intervention, type='scatter', mode='lines',colors=c("#a50f15","#fc9272"))

p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per", formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscaleInt),
         annotations=list(text=HTML(paste("Baseline: <br>R", tags$sub(0),'=',formatC(Ro,digits=2)," <br>r =", formatC(r,digits=2)," per day <br>T",tags$sub(2)," = ",formatC(DoublingTime,digits=2)," days <br><br>Intervention: <br>R", tags$sub(0),'=',formatC(RoInt,digits=2),"<br>r =", formatC(rInt,digits=2)," per day <br>T",tags$sub(2)," = ",formatC(DoublingTimeInt,digits=2)," days")),
                          showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="top",y=0.5, align="left")
)

p