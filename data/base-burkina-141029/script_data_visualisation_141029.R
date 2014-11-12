#===Loading packages nedded==========
install.packages("zoo")
install.packages("lubridate") # for extracting date, month, week from time series
install.packages("lattice")
require("lattice")
require("zoo")
require(lubridate)


#==========Loading "zoo" formatted time series==========

load("data/base-burkina-141029/141029_meningite.RData")

names(menin["incidence"][[1]]) # getting the district names in the list "cas" of menin list

#=================================================

##=============Weekly Incidence data.================

BOULSA.incid<-zoo(menin["incidence"][[1]]["BOULSA"][[1]])         # incid for BOULSA district
DAFRA.incid<-zoo(menin["incidence"][[1]]["DAFRA"][[1]])         # incid for DAFRA district
DANDE.incid<-zoo(menin["incidence"][[1]]["DANDE"][[1]])         # incid for DANDE district
DA.incid<-zoo(menin["incidence"][[1]][4][[1]])                  # incid for DA district
HOUNDA.incid<-zoo(menin["incidence"][[1]][5][[1]])              # incid for HOUNDA district
K.VIGUE.incid<-zoo(menin["incidence"][[1]][6][[1]])             # incid for KARANGASSO VIGUE district
LENA.incid<-zoo(menin["incidence"][[1]]["LENA"][[1]])           # incid for LENA district
ORODARA.incid<-zoo(menin["incidence"][[1]]["ORODARA"][[1]])     # incid for ORODARA district
OUAHIGOUYA.incid<-zoo(menin["incidence"][[1]]["OUAHIGOUYA"][[1]]) # incid for OUAHIGOUYA district
SEGUENEGA.incid<-zoo(menin["incidence"][[1]]["SEGUENEGA"][[1]])   # incid for SEGUENEGA district
YAKO.incid<-zoo(menin["incidence"][[1]]["YAKO"][[1]])        # incid for YAKO district

#==============extracting dates into a vector=====
vtime=time(SEGUENEGA.incid) # extracting time vector (as date)
vtime_year<-year(vtime) #extraction of years only
vtime_month<-month(vtime)
vtime_week<-week(vtime)
vtime_day<-day(vtime)

#==============Weekly number of cases====================

BOULSA.data<-zoo(menin["cas"][[1]]["BOULSA"][[1]])         # data for BOULSA district
DAFRA.data<-zoo(menin["cas"][[1]]["DAFRA"][[1]])         # data for DAFRA district
DANDE.data<-zoo(menin["cas"][[1]]["DANDE"][[1]])         # data for DANDE district
DA.data<-zoo(menin["cas"][[1]][4][[1]])                  # data for DA district
HOUNDA.data<-zoo(menin["cas"][[1]][5][[1]])              # data for HOUNDA district
K.VIGUE.data<-zoo(menin["cas"][[1]][6][[1]])             # data for KARANGASSO VIGUE district
LENA.data<-zoo(menin["cas"][[1]]["LENA"][[1]])           # data for LENA district
ORODARA.data<-zoo(menin["cas"][[1]]["ORODARA"][[1]])     # data for ORODARA district
OUAHIGOUYA.data<-zoo(menin["cas"][[1]]["OUAHIGOUYA"][[1]]) # data for OUAHIGOUYA district
SEGUENEGA.data<-zoo(menin["cas"][[1]]["SEGUENEGA"][[1]])   # data for SEGUENEGA district
YAKO.data<-zoo(menin["cas"][[1]]["YAKO"][[1]])        # data for YAKO district


#============Part(1)Visualising the health centers data for each district=======

#SEGUENEGA.data[c(1:10),c(1:10)] # view only ten first health centers data

#Plotting data for the csps kalsaka health center (SEGUENEGA district)

#plot(vtime,SEGUENEGA.incid[,2],type="l",lwd=2,col="blue")

#plotting data for csps kalsaka health center (2006)

plot(vtime[vtime>="2006-01-02"&vtime<="2006-12-29"],SEGUENEGA.incid[,9][vtime>="2006-01-02"&vtime<="2006-12-29"]*1e+05
     ,pch=16,type="b", xlab="Time (Weeks)", ylab="Weekly incidence of suspected cases/100,000", 
     main="Meningitis cases reporting in CSPS Kalsaka \n (Burkina-Faso) - 2006")


pdf(file="plots/data_visualisation.pdf", title="Data visualisation per district\n Burkina-Faso (2006)")
par(las=1, oma=c(3,3,4,5))

xyplot(SEGUENEGA.incid*1e+05,type="b",main="SEGUENEGA District health centers n=20",sub="SEGUENEGA District health centers",layout=c(4,5))

xyplot(YAKO.incid[,c(1:20)]*1e+05,type="b",main="YAKO District health centers n=49",sub="YAKO District health centers",layout=c(4,5))
xyplot(YAKO.incid[,c(21:41)]*1e+05,type="b",main="YAKO District health centers n=49",sub="YAKO District health centers",layout=c(4,5))
xyplot(YAKO.incid[,c(42:49)]*1e+05,type="b",main="YAKO District health centers n=49",sub="YAKO District health centers",layout=c(4,5))

xyplot(OUAHIGOUYA.incid[,c(1:20)]*1e+05,type="b",main="OUAHIGOUYA District health centers n=60",sub="OUAHIGOUYA District health centers",layout=c(4,5))
xyplot(OUAHIGOUYA.incid[,c(21:41)]*1e+05,type="b",main="OUAHIGOUYA District health centers n=60",sub="OUAHIGOUYA District health centers",layout=c(4,5))
xyplot(OUAHIGOUYA.incid[,c(42:60)]*1e+05,type="b",main="OUAHIGOUYA District health centers n=60",sub="OUAHIGOUYA District health centers",layout=c(4,5))

xyplot(ORODARA.incid[,c(1:20)]*1e+05,type="b",main="ORODARA District health centers n=45",sub="ORODARA District health centers",layout=c(4,5))
xyplot(ORODARA.incid[,c(21:41)]*1e+05,type="b",main="ORODARA District health centers n=45",sub="ORODARA District health centers",layout=c(4,5))
xyplot(ORODARA.incid[,c(42:45)]*1e+05,type="b",main="ORODARA District health centers n=45",sub="ORODARA District health centers",layout=c(4,5))

xyplot(LENA.incid*1e+05,type="b",main="LENA District health centers n=13",sub="LENA District health centers",layout=c(4,5))

xyplot(K.VIGUE.incid*1e+05,type="b",main="K.VIGUE District health centers n=8",sub="K.VIGUE District health centers",layout=c(4,5))

xyplot(HOUNDA.incid[,c(1:20)]*1e+05,type="b",main="HOUNDE District health centers n=27",sub="HOUNDA District health centers",layout=c(4,5))
xyplot(HOUNDA.incid[,c(21:27)]*1e+05,type="b",main="HOUNDE District health centers n=27",sub="HOUNDA District health centers",layout=c(4,5))

xyplot(DA.incid[,c(1:20)]*1e+05,type="b",main="DA District health centers n=27",sub="DA District health centers",layout=c(4,5))
xyplot(DA.incid[,c(21:27)]*1e+05,type="b",main="DA District health centers n=27",sub="DA District health centers",layout=c(4,5))

xyplot(DANDE.incid[,c(1:20)]*1e+05,type="b",main="DANDE District health centers n=25",sub="DANDE District health centers",layout=c(4,5))
xyplot(DANDE.incid[,c(21:25)]*1e+05,type="b",,main="DANDE District health centers n=25",sub="DANDE District health centers",layout=c(4,5))

xyplot(DAFRA.incid[,c(1:20)]*1e+05,type="b",main="DAFRA District health centers n=34",sub="DAFRA District health centers",layout=c(4,5))
xyplot(DAFRA.incid[,c(21:34)]*1e+05,type="b",main="DAFRA District health centers n=34",sub="DAFRA District health centers",layout=c(4,5))

xyplot(BOULSA.incid[,c(1:20)]*1e+05,type="b",main="BOULSA District health centers n=31",sub="BOULSA District health centers",layout=c(4,5))
xyplot(BOULSA.incid[,c(21:31)]*1e+05,type="b",main="BOULSA District health centers n=31",sub="BOULSA District health centers",layout=c(4,5))

dev.off()



## part(2) data visualisation ===================
#Part(2) visualisation of data for choosing health centers with outbreak

par(mar=c(2,5,2,5))
pdf(file='plots/segunega.pdf',title="Data visualisation at health center levels")

xyplot(SEGUENEGA.data[,c(1:9)], ylab="Weekly incidence/100,000 (suspected cases)",
       main="Data visualisation at health center levels\n seguenega",
       layout=c(2,4), sub="text here", xlim=vtime[vtime>="2006-01-02"&vtime<="2006-12-29"])

plot(vtime[vtime>="2006-01-02"&vtime<="2006-12-29"],SEGUENEGA.incid[,9][vtime>="2006-01-02"&vtime<="2006-12-29"]
     ,pch=16,type="b", xlab="Time (Weeks)", ylab="Weekly numbers of suspected cases", 
     main="Meningitis cases reporting in CSPS Kalsaka \n (Burkina-Faso) - 2006")


dev.off()


###fonction générique pourextraire le coredata (observation) de la serie temporelle====

# Ce 11/11/2014 calcul des indicateurs pour le district de Hounde

hounde.incid.data<-coredata(HOUNDA.incid) # Generic functions for extracting only the matrix of observations from the full time series.
as.vector(cbind(data.frame(hounde.incid.data)[1][,"csps.bassé"]))

sample=data.frame(hounde.incid.data) 


###===============

#plotting function
incid_plot<-function(x, main="", ylim=NULL, sub=""){
    xyplot(x*1e+05, 
           pch=16,type="b",ylim=ylim,main=main,cex=.7,
           sub=sub,layout=c(3,3),
           panel = function( x,y,...) {
               panel.abline( h=75, lty = "dotted", col = "black")
               panel.xyplot( x,y,...)
           })   
} 

# defining graphical parameters for hounde district.

main.hounde<-as.character("Weekly incidence/100,000 \n Sanitary District of Houndé (Burkina-Faso) \n Dotted line represent treshold for outbreak")
sub.hounde<-as.character("Data represented by health centers")

pdf(file='plots/hounde.pdf',width=15,height=10)

incid_plot(
        HOUNDA.incid[,c(1:27)][year(vtime)<=2006], 
        main=main.hounde,
        sub=sub.hounde,
        ylim=c(0,350)
        )

incid_plot(
    HOUNDA.incid[,c(1:27)][year(vtime)>2006&year(vtime)<=2011], 
    main=main.hounde,
    sub=sub.hounde,
    ylim=c(0,350)
    )

incid_plot(
    HOUNDA.incid[,c(1:27)][year(vtime)==2012], 
    main=main.hounde,
    sub=sub.hounde,
    ylim=c(0,350)
    )

dev.off()

# Plotting all years for hounde district hs.

pdf(file='plots/hounde_2004_2012_all.pdf',width=15,height=10)

incid_plot(
    HOUNDA.incid[,c(1:27)][year(vtime)>=2004], 
    main=main.hounde,
    sub=sub.hounde,
    ylim=c(0,350))

dev.off()

fe_fs_houde<-c(
    "csps bassé",
    "csps bohokari",
    "bouahoun",
    "csps bouéré",
    "csps dougoumanto ii",
    "csps fafo",
    "csps gombélédougou",
    "csps koumbia",
    "csps sara"
    )

fe_fs_houde.index<-c(
    1,4,6,7,9,10,12,19,25
    )

# ploting only health center which experience epidemic at least once 

pdf(file='plots/fe_fs_houde_2004_2012.pdf',width=15,height=10)

incid_plot(
    HOUNDA.incid[,fe_fs_houde.index][year(vtime)>=2004], 
    main=main.hounde,
    sub=sub.hounde,
    ylim=c(0,350))

dev.off()

#===

pdf(file='plots/fe_fs_houde_2006.pdf',width=15,height=10)

incid_plot(
    HOUNDA.incid[,fe_fs_houde.index][year(vtime)==2006], 
    main=main.hounde,
    sub=sub.hounde,
    ylim=c(0,350)
    )

# dry season only in epidemic year 2006

incid_plot(
    HOUNDA.incid[,fe_fs_houde.index][yearmon(as.yearmon(vtime))>="janv. 2006"&yearmon(as.yearmon(vtime))<="mai 2006"], 
    main=main.hounde,
    sub=sub.hounde,
    ylim=c(0,350)
)

dev.off()

#calculation of the median value of weekly pic
#incidences together with min and max incidence during the dry season.





