install.packages("zoo")
require("zoo")

names(menin["cas"][[1]]) # getting the district names in the list "cas" of menin list

# [1] "BOULSA"           "DAFRA"            "DANDE"            "DÃ”"             
# [5] "HOUNDÃ‰"          "KARANGASSO VIGUE" "LENA"             "ORODARA"         
# [9] "OUAHIGOUYA"       "SEGUENEGA"        "YAKO"   

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



vtime=time(SEGUENEGA.data) # extracting time vector (as date)
test=date(SEGUENEGA.data[1,1:471])

SEGUENEGA.data[c(1:10),c(1:10)] # view only ten first health centers data



# Visualising the health centers data for each district.

install.packages("lattice")
require("lattice")

xyplot(SEGUENEGA.data)
xyplot(DAFRA.data[,c(1:10)]) # plotting data for the first 10 health centers
xyplot(DANDE.data[,c(10:20)]) # plotting data for the first 10 health centers

# Plotting data for the csps kalsaka health center (SEGUENEGA district)

plot(vtime,SEGUENEGA.data[,2],type="l",lwd=2,col="blue")

# plotting data for csps kalsaka health center (2006)

#pdf(file="plots/data_csps_kalsaka.pdf", width=8, height=8, 
    #title="Data visualisation at health center levels")

plot(vtime[vtime>="2006-01-02"&vtime<="2006-12-29"],SEGUENEGA.data[,9][vtime>="2006-01-02"&vtime<="2006-12-29"]
     ,pch=16,type="b", xlab="Time (Weeks)", ylab="Weekly numbers of suspected cases", 
     main="Meningitis cases reporting in CSPS Kalsaka \n (Burkina-Faso) - 2006")


## Incidence.

SEGUENEGA.incid<-zoo(menin["incidence"][[1]]["SEGUENEGA"][[1]])   # data for SEGUENEGA district

# Now with 2 graphs
pdf(file='plots/segunega.pdf',title="Data visualisation at health center levels")

xyplot(SEGUENEGA.data[,9], ylab="Weekly numbers of suspected cases",title="Data visualisation at health center levels\n seguenega")

plot(vtime[vtime>="2006-01-02"&vtime<="2006-12-29"],SEGUENEGA.data[,9][vtime>="2006-01-02"&vtime<="2006-12-29"]
     ,pch=16,type="b", xlab="Time (Weeks)", ylab="Weekly numbers of suspected cases", 
     main="Meningitis cases reporting in CSPS Kalsaka \n (Burkina-Faso) - 2006")


dev.off()
