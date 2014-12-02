#==============extracting dates into a vector=====

vtime=time(SEGUENEGA.incid) # extracting time vector (as date)
vtime[2]-vtime[1] # calculating the number of days from t1 to t2

start(HOUNDA.incid) # time serie start date

end(HOUNDA.incid) # time serie start date

subset.houde<-as.data.frame( # subseting data data base using a time window
    HOUNDA.incid[as.Date(c("2006/11/06", "2006/05/01")),fe_fs_houde.index]
    )

## =====================================================================
#   Subsetting data by season per health center group for each district
## =====================================================================

# The following seasons are defined for burkina-faso: 
# Source: Site web ministere des affaires etrangère burkina-faso
# www.mae.gov.bf/climat bf.html (visité au mois de novembre)

## Dry season (all): November - May
## Dry season harmattan: November - February
## Dry season hot (T>=40°C): March - Mai

## Wet / rainy season: June - October
#-------------------------------------------
# Hounde district
v_month<-month(time(HOUNDA.incid))
HOUNDA.incid$season<-zoo(HOUNDA.incid$season==0)
HOUNDA.incid$season[which(v_month==11 | v_month==12 | v_month==1 | v_month==2)]<-HOUNDA.incid$season=="dry harmattan"
HOUNDA.incid$season[which(v_month==3 | v_month==4 | v_month==5)]<-"dry hot"
HOUNDA.incid$season[which(v_month==6 | v_month==7 | v_month==8 | v_month==9 | v_month==10)]<-"rainy"


xyplot(HOUNDA.incid[which(HOUNDA.incid[,28]=="dry harmattan"),fe_fs_houde.index])
xyplot(HOUNDA.incid)

dry_hounde_fe<-window(
    HOUNDA.incid[,fe_fs_houde.index], 
    start=as.Date("2006/05/01"),
    end=as.Date("2006-11-27")
    ) 

dry_hounde_fe_harmattan<-window(
    HOUNDA.incid[,fe_fs_houde.index], 
    start=as.Date("2006/05/01"),
    end=as.Date("2006-11-27")
    )

HOUNDA.incid[test,fe_fs_houde.index]

test=month(vtime)==c(11,12,1,2,3,4,5)


sapply(HOUNDA.incid[!is.na(HOUNDA.incid)][,10],function(x) max(x))


plot(test[, 1:3]*1e+05, plot.type = "single", col = c("red", "blue"), lwd = 1)

plot(test[, 1:10]*1e+05, type="b",lwd = 1,pch=16, ylim=c(0,350))


sapply(test*1e+05,function(x) max(x))

#=====================================================
"Year 2006 district of Hounde"

dry_hounde=window(HOUNDA.incid, start = as.Date("2005-11-28"), end = as.Date("2006-05-29"))
harmattan_hounde<-window(HOUNDA.incid, start = as.Date("2005-11-28"), end = as.Date("2006-02-27"))
wet_hounde<-window(HOUNDA.incid, start = as.Date("2006-06-05"), end = as.Date("2006-10-30"))



wet_hounde_2006<-cbind(min=sapply(wet_hounde*1e+05,function(x) min(x,na.rm=FALSE)),
                  mean=sapply(wet_hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                  max=sapply(wet_hounde*1e+05,function(x) max(x,na.rm=FALSE)))

mean(wet_hounde_2006[,1], na.rm=TRUE)
mean(wet_hounde_2006[,2], na.rm=TRUE)
mean(wet_hounde_2006[,3], na.rm=TRUE)




dry_hounde_2006<-cbind(min=sapply(dry_hounde*1e+05,function(x) min(x,na.rm=FALSE)),
      mean=sapply(dry_hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
      max=sapply(dry_hounde*1e+05,function(x) max(x,na.rm=FALSE)))

mean(dry_hounde_2006[,1], na.rm=TRUE)
mean(dry_hounde_2006[,2], na.rm=TRUE)
mean(dry_hounde_2006[,3], na.rm=TRUE)


harmattan_hounde_2006<-cbind(min=sapply(harmattan_hounde*1e+05,function(x) min(x,na.rm=FALSE)),
                       mean=sapply(harmattan_hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                       max=sapply(harmattan_hounde*1e+05,function(x) max(x,na.rm=FALSE)))


mean(harmattan_hounde_2006[,1], na.rm=TRUE)
mean(harmattan_hounde_2006[,2], na.rm=TRUE)
mean(harmattan_hounde_2006[,3], na.rm=TRUE)
#======================


# Hounde district 2004-2012

# formation avec foyer
dry.fe.hounde<-HOUNDA.incid[month(time(HOUNDA.incid))==11:12
                  |month(time(HOUNDA.incid))==1:5][,fe_fs_houde.index]

dry.fe.hounde.incid<-cbind(min=sapply(dry.fe.hounde*1e+05,function(x) min(x,na.rm=TRUE)),
        mean=sapply(dry.fe.hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
        max=sapply(dry.fe.hounde*1e+05,function(x) max(x,na.rm=TRUE)))

mean(dry.fe.hounde.incid[,1], na.rm=TRUE)
mean(dry.fe.hounde.incid[,2], na.rm=TRUE)
mean(dry.fe.hounde.incid[,3], na.rm=TRUE)
dry.fe.hounde.incid


wet.fe.hounde<-HOUNDA.incid[month(time(HOUNDA.incid))==6:10][,fe_fs_houde.index]
wet.fe.hounde.incid<-cbind(min=sapply(wet.fe.hounde*1e+05,function(x) min(x,na.rm=TRUE)),
                     mean=sapply(wet.fe.hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                     max=sapply(wet.fe.hounde*1e+05,function(x) max(x,na.rm=TRUE)))


mean(wet.fe.hounde.incid[,1], na.rm=TRUE)
mean(wet.fe.hounde.incid[,2], na.rm=TRUE)
mean(wet.fe.hounde.incid[,3], na.rm=TRUE)
wet.fe.hounde.incid

# period harmattan
harmattan.fe.hounde<-HOUNDA.incid[month(time(HOUNDA.incid))==c(11,12,1,2)][,fe_fs_houde.index]
harmattan.fe.hounde.incid<-cbind(min=sapply(harmattan.fe.hounde*1e+05,function(x) min(x,na.rm=TRUE)),
                      mean=sapply(harmattan.fe.hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                      max=sapply(harmattan.fe.hounde*1e+05,function(x) max(x,na.rm=TRUE)))


mean(harmattan.fe.hounde.incid[,1], na.rm=TRUE)
mean(harmattan.fe.hounde.incid[,2], na.rm=TRUE)
mean(harmattan.fe.hounde.incid[,3], na.rm=TRUE)
harmattan.fe.hounde.incid

#===================


# formation sans foyer hounde
dry.non_fe.hounde<-HOUNDA.incid[month(time(HOUNDA.incid))==11:12
                            |month(time(HOUNDA.incid))==1:5][,non_fe_fs_houde.index]

dry.non_fe.hounde.incid<-cbind(min=sapply(dry.non_fe.hounde*1e+05,function(x) min(x,na.rm=TRUE)),
                           mean=sapply(dry.non_fe.hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                           max=sapply(dry.non_fe.hounde*1e+05,function(x) max(x,na.rm=TRUE)))

mean(dry.non_fe.hounde.incid[,1], na.rm=TRUE)
mean(dry.non_fe.hounde.incid[,2], na.rm=TRUE)
mean(dry.non_fe.hounde.incid[,3], na.rm=TRUE)
dry.non_fe.hounde.incid


wet.non_fe.hounde<-HOUNDA.incid[month(time(HOUNDA.incid))==6:10][,non_fe_fs_houde.index]
wet.non_fe.hounde.incid<-cbind(min=sapply(wet.non_fe.hounde*1e+05,function(x) min(x,na.rm=TRUE)),
                           mean=sapply(wet.non_fe.hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                           max=sapply(wet.non_fe.hounde*1e+05,function(x) max(x,na.rm=TRUE)))


mean(wet.non_fe.hounde.incid[,1], na.rm=TRUE)
mean(wet.non_fe.hounde.incid[,2], na.rm=TRUE)
mean(wet.non_fe.hounde.incid[,3], na.rm=TRUE)
wet.non_fe.hounde.incid

# period harmattan
harmattan.non_fe.hounde<-HOUNDA.incid[month(time(HOUNDA.incid))==c(11,12,1,2)][,non_fe_fs_houde.index]
harmattan.non_fe.hounde.incid<-cbind(min=sapply(harmattan.non_fe.hounde*1e+05,function(x) min(x,na.rm=TRUE)),
                                 mean=sapply(harmattan.non_fe.hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                                 max=sapply(harmattan.non_fe.hounde*1e+05,function(x) max(x,na.rm=TRUE)))


mean(harmattan.non_fe.hounde.incid[,1], na.rm=TRUE)
mean(harmattan.non_fe.hounde.incid[,2], na.rm=TRUE)
mean(harmattan.non_fe.hounde.incid[,3], na.rm=TRUE)
harmattan.non_fe.hounde.incid

# calcul  incidence moyenne pendant dépassement seuil houndé:
#=========


# Lena pseudo district
ts.Lena.complete

    
    



















## ORODARA DISTRICT 2004-2012

dry_orodara<-ORODARA.incid[month(time(ORODARA.incid))==11:12
                  |month(time(ORODARA.incid))==1:5]

dry_orodara_all<-cbind(min=sapply(dry_orodara*1e+05,function(x) min(x,na.rm=TRUE)),
                      mean=sapply(dry_orodara*1e+05,function(x) mean(x,na.rm=TRUE)),
                      max=sapply(dry_orodara*1e+05,function(x) max(x,na.rm=TRUE)))

mean(dry_orodara_all[,1], na.rm=TRUE)
mean(dry_orodara_all[,2], na.rm=TRUE)
mean(dry_orodara_all[,3], na.rm=TRUE)

#wet orodara

wet_orodara<-ORODARA.incid[month(time(ORODARA.incid))==6:10]

wet_orodara_all<-cbind(min=sapply(wet_orodara*1e+05,function(x) min(x,na.rm=TRUE)),
                       mean=sapply(wet_orodara*1e+05,function(x) mean(x,na.rm=TRUE)),
                       max=sapply(wet_orodara*1e+05,function(x) max(x,na.rm=TRUE)))

mean(wet_orodara_all[,1], na.rm=TRUE)
mean(wet_orodara_all[,2], na.rm=TRUE)
mean(wet_orodara_all[,3], na.rm=TRUE)

# harmattan orodara
harmattan_orodara<-ORODARA.incid[month(time(ORODARA.incid))==c(11,12,1,2)]

harmattan_orodara_all<-cbind(min=sapply(harmattan_orodara*1e+05,function(x) min(x,na.rm=TRUE)),
                       mean=sapply(harmattan_orodara*1e+05,function(x) mean(x,na.rm=TRUE)),
                       max=sapply(harmattan_orodara*1e+05,function(x) max(x,na.rm=TRUE)))

mean(harmattan_orodara_all[,1], na.rm=TRUE)
mean(harmattan_orodara_all[,2], na.rm=TRUE)
mean(harmattan_orodara_all[,3], na.rm=TRUE)

