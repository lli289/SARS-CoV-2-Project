# Investigator: Lillian
# Last update: 04/29/2023
data <- read.delim("meta.Africa.tsv")

# count swabs
table(data$Specimen)
# nasopharyngeal swab
21+63+76+698+1+2+3+38+7+43+38+3+963+1+24089+615+134+49+625

# Nasopharyngeal swab and Oropharyngeal swab
341+147++12+1+260+22925+489+16+17+28+59+
  2+529+1+16+7+2+66+4+12+57+1+1+1+220+6+2+2+8+1+1

# Oropharyngeal swab
1+244+21+1857+1+46+17406+1+2+17

# number of sequences >29000
Len <- data %>%
  filter(seqlenth > 29000)
dim(Len)[1] # 149090 
dim(Len)[1]/dim(data)[1] # 97%

# count countries
dim(table(data$Country))[1] #55

Y2020 <- data %>%
  filter(Year %in% 2020)
dim(table(Y2020$Country)) # 29

Y2021 <- data %>%
  filter(Year %in% 2021)
dim(table(Y2021$Country)) # 52

Y2022 <- data %>%
  filter(Year %in% 2022)
dim(table(Y2022$Country)) # 54

# count number of sequences each year
dim(Y2020)[1] # 5241
dim(Y2021)[1] # 60149
dim(Y2022)[1] # 80274
dim(Y2023)[1] # 7674
5241+60149+80274+7674 # checked total

# contributing African countries
sum(sort(table(data$Country,useNA = "always"), decreasing =TRUE)[1:10])/dim(data)[1] # 77%

# south africa between 2020 and 2022
A <- data %>%
  filter(Year %in% c("2020", "2021", "2022"))
sum(sort(table(A$Country,useNA = "always"), decreasing =TRUE)[1])/dim(data)[1] # 31%

# table 1
table(data$Sequencing.technology)[-3]
A <- round(table(data$Sequencing.technology)[-3]/sum(table(data$Sequencing.technology)[-3])*100,2)
A
sum(A) # checked

c(dim(data %>%
        filter(Sequencing.technology == 'Illumina' & Subregions == "Northern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Illumina' & Subregions == "Eastern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Illumina' & Subregions == "Central Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Illumina' & Subregions == "Western Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Illumina' & Subregions == "Southern Africa"))[1])

c(dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Subregions == "Northern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Subregions == "Eastern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Subregions == "Central Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Subregions == "Western Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Subregions == "Southern Africa"))[1])

c(dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Subregions == "Northern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Subregions == "Eastern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Subregions == "Central Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Subregions == "Western Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Subregions == "Southern Africa"))[1])

c(dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Subregions == "Northern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Subregions == "Eastern Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Subregions == "Central Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Subregions == "Western Africa"))[1],
  dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Subregions == "Southern Africa"))[1])
6119+17915+4472+15382+44099+1176+33650+2179+11072+13120+1098+314+1+55+1302+111+12+336+12 # checked

c(round(dim(data %>%
              filter(Sequencing.technology == 'Illumina' & Year == "2020"))[1]/table(data$Year)[1]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Illumina' & Year == "2021"))[1]/table(data$Year)[2]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Illumina' & Year == "2022"))[1]/table(data$Year)[3]*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == 'Nanopore' & Year == "2020"))[1]/table(data$Year)[1]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Nanopore' & Year == "2021"))[1]/table(data$Year)[2]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Nanopore' & Year == "2022"))[1]/table(data$Year)[3]*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == 'Ion Torrent' & Year == "2020"))[1]/table(data$Year)[1]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Ion Torrent' & Year == "2021"))[1]/table(data$Year)[2]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Ion Torrent' & Year == "2022"))[1]/table(data$Year)[3]*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == 'Sanger' & Year == "2020"))[1]/table(data$Year)[1]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Sanger' & Year == "2021"))[1]/table(data$Year)[2]*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == 'Sanger' & Year == "2022"))[1]/table(data$Year)[3]*100,2))

dim(data %>%
      filter(Sequencing.technology == 'Illumina' & Year == "2020"))[1]/table(data$Year)[1]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Year == "2020"))[1]/table(data$Year)[1]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Year == "2020"))[1]/table(data$Year)[1]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Year == "2020"))[1]/table(data$Year)[1]*100 # 99.96184 

dim(data %>%
      filter(Sequencing.technology == 'Illumina' & Year == "2021"))[1]/table(data$Year)[2]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Year == "2021"))[1]/table(data$Year)[2]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Year == "2021"))[1]/table(data$Year)[2]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Year == "2021"))[1]/table(data$Year)[2]*100 # 99.18203

dim(data %>%
      filter(Sequencing.technology == 'Illumina' & Year == "2022"))[1]/table(data$Year)[3]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Nanopore' & Year == "2022"))[1]/table(data$Year)[3]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Ion Torrent' & Year == "2022"))[1]/table(data$Year)[3]*100 +
  dim(data %>%
        filter(Sequencing.technology == 'Sanger' & Year == "2022"))[1]/table(data$Year)[3]*100 # 99.76206

# check number of genomes of subregion
sort(table(data$Subregions,useNA = "always"), decreasing =TRUE)
sum(sort(table(data$Subregions,useNA = "always"), decreasing =TRUE)) # 153338 checked

# Illumina and Nanopore percentage
round(table(data$Sequencing.technology)[1]/dim(data)[1]*100,0) # 57%
round(table(data$Sequencing.technology)[4]/dim(data)[1]*100,0) # 40%
round(sum(table(data$Sequencing.technology)[-c(1,4)])/dim(data)[1]*100,0) # 2%
table(data$Sequencing.technology,useNA = "always")

# yearly trends of technology
# 22% in 2020 to 44% in 2022 obtained from table 1
# 75% in 2020 to 54% in 2022 obtained from table 1

# Reunion, kenya, botswana, and ghana
A <- data %>%
  filter(Country %in% c("Reunion", "Kenya", "Botswana", "Ghana"))
round(table(A$Sequencing.technology,useNA="always")[3]/sum(table(A$Sequencing.technology,useNA="always"))*100,0) # 84%

# table 2
sort(table(data$Country,useNA = "always"), decreasing =TRUE)[1:10]
round(sort(table(data$Country,useNA = "always"), decreasing =TRUE)[1:10]/dim(data)[1]*100,2)
c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "SouthAfrica"))[1]/50298*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "SouthAfrica"))[1]/50298*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "SouthAfrica" ))[1]/50298*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Reunion"))[1]/19177*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Reunion"))[1]/19177*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Reunion" ))[1]/19177*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Kenya"))[1]/12026*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Kenya"))[1]/12026*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Kenya" ))[1]/12026*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Nigeria"))[1]/7989*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Nigeria"))[1]/7989*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Nigeria" ))[1]/7989*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Mauritius"))[1]/6433*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Mauritius"))[1]/6433*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Mauritius" ))[1]/6433*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Senegal"))[1]/6068*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Senegal"))[1]/6068*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Senegal" ))[1]/6068*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Botswana"))[1]/5260*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Botswana"))[1]/5260*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Botswana" ))[1]/5260*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Ghana"))[1]/5156*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Ghana"))[1]/5156*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Ghana" ))[1]/5156*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "Egypt"))[1]/3302*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "Egypt"))[1]/3302*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "Egypt" ))[1]/3302*100,2))

c(round(dim(data %>%
              filter(Sequencing.technology == "Illumina" & Country == "DRCongo"))[1]/2072*100,2),
  round(dim(data %>%
              filter(Sequencing.technology == "Nanopore" & Country == "DRCongo"))[1]/2072*100,2),
  round(dim(data %>%
              filter(!Sequencing.technology %in% c("Illumina", "Nanopore") & Country == "DRCongo" ))[1]/2072*100,2))

std <- function(x) sd(x)/sqrt(length(x))

temp <- data%>%
  filter(Country == "SouthAfrica")
c(round(mean(temp$totalDeletions,na.rm=TRUE),2),
  round(std(temp$totalDeletions[-which(is.na(temp$totalDeletions))]),2),
  round(mean(temp$totalInsertions,na.rm=TRUE),2),
  round(std(temp$totalInsertions[-which(is.na(temp$totalInsertions))]),2),
  round(mean(temp$totalNonACGTNs,na.rm=TRUE),2),
  round(std(temp$totalNonACGTNs[-which(is.na(temp$totalNonACGTNs))]),2))

temp <- data%>%
  filter(Country == "Reunion")
c(round(mean(temp$totalDeletions,na.rm=TRUE),2),
  round(std(temp$totalDeletions),2),
  round(mean(temp$totalInsertions,na.rm=TRUE),2),
  round(std(temp$totalInsertions),2),
  round(mean(temp$totalNonACGTNs,na.rm=TRUE),2),
  round(std(temp$totalNonACGTNs),2))

temp <- data%>%
  filter(Country == "Kenya")
c(round(mean(temp$totalDeletions,na.rm=TRUE),2),
  round(std(temp$totalDeletions),2),
  round(mean(temp$totalInsertions,na.rm=TRUE),2),
  round(std(temp$totalInsertions),2),
  round(mean(temp$totalNonACGTNs,na.rm=TRUE),2),
  round(std(temp$totalNonACGTNs),2))

temp <- data%>%
  filter(Country == "Nigeria")
c(round(mean(temp$totalDeletions,na.rm=TRUE),2),
  round(std(temp$totalDeletions[-which(is.na(temp$totalDeletions))]),2),
  round(mean(temp$totalInsertions,na.rm=TRUE),2),
  round(std(temp$totalInsertions[-which(is.na(temp$totalInsertions))]),2),
  round(mean(temp$totalNonACGTNs,na.rm=TRUE),2),
  round(std(temp$totalNonACGTNs[-which(is.na(temp$totalNonACGTNs))]),2))

temp <- data%>%
  filter(Country == "Mauritius")
c(round(mean(temp$totalDeletions),2),
  round(std(temp$totalDeletions),2),
  round(mean(temp$totalInsertions),2),
  round(std(temp$totalInsertions),2),
  round(mean(temp$totalNonACGTNs),2),
  round(std(temp$totalNonACGTNs),2))

temp <- data%>%
  filter(Country == "Senegal")
c(round(mean(temp$totalDeletions,na.rm=TRUE),2),
  round(std(temp$totalDeletions[-which(is.na(temp$totalDeletions))]),2),
  round(mean(temp$totalInsertions,na.rm=TRUE),2),
  round(std(temp$totalInsertions[-which(is.na(temp$totalInsertions))]),2),
  round(mean(temp$totalNonACGTNs,na.rm=TRUE),2),
  round(std(temp$totalNonACGTNs[-which(is.na(temp$totalNonACGTNs))]),2))

temp <- data%>%
  filter(Country == "Botswana")
c(round(mean(temp$totalDeletions),2),
  round(std(temp$totalDeletions),2),
  round(mean(temp$totalInsertions),2),
  round(std(temp$totalInsertions),2),
  round(mean(temp$totalNonACGTNs),2),
  round(std(temp$totalNonACGTNs),2))

temp <- data%>%
  filter(Country == "Ghana")
c(round(mean(temp$totalDeletions),2),
  round(std(temp$totalDeletions),2),
  round(mean(temp$totalInsertions),2),
  round(std(temp$totalInsertions),2),
  round(mean(temp$totalNonACGTNs),2),
  round(std(temp$totalNonACGTNs),2))

temp <- data%>%
  filter(Country == "Egypt")
c(round(mean(temp$totalDeletions,na.rm=TRUE),2),
  round(std(temp$totalDeletions[-which(is.na(temp$totalDeletions))]),2),
  round(mean(temp$totalInsertions,na.rm=TRUE),2),
  round(std(temp$totalInsertions[-which(is.na(temp$totalInsertions))]),2),
  round(mean(temp$totalNonACGTNs,na.rm=TRUE),2),
  round(std(temp$totalNonACGTNs[-which(is.na(temp$totalNonACGTNs))]),2))

temp <- data%>%
  filter(Country == "DRCongo")
c(round(mean(temp$totalDeletions),2),
  round(std(temp$totalDeletions),2),
  round(mean(temp$totalInsertions),2),
  round(std(temp$totalInsertions),2),
  round(mean(temp$totalNonACGTNs),2),
  round(std(temp$totalNonACGTNs),2))

# genome quality
L <- data %>%
  filter(qc.overallStatus == "bad")
dim(L)[1]

H <- data %>%
  filter(qc.overallStatus == "good")
dim(H)[1]

# missing
summary(data$totalMissing) # median 467 < 5000

# quality, substitutions, deletion, insertation vs good quality.
(mean(L$totalSubstitutions)-mean(H$totalSubstitutions))/mean(H$totalSubstitutions) # 18% lower
(mean(L$totalDeletions)-mean(H$totalDeletions))/mean(H$totalDeletions) # 16% lower
(mean(L$totalInsertions)-mean(H$totalInsertions))/mean(H$totalInsertions) # 50% higher

# kenya, botswana, and ghana
A <- data %>%
  filter(Country %in% c("Kenya", "Botswana", "Ghana"))
table(A$Sequencing.technology)[3]/dim(A)[1]

# south africa, nigeria, mauritius, and egypt
B <- data %>%
  filter(Country %in% c("SouthAfrica", "Nigeria", "Mauritius", "Egypt"))
table(B$Sequencing.technology) # primary Illumina

# table 3
c(c(dim(data %>%
          filter(Sequencing.technology == 'Illumina' & Subregions == 'Northern Africa'))[1],
    dim(data %>%
          filter(Sequencing.technology == 'Nanopore' & Subregions == 'Northern Africa'))[1]),
  c(dim(data %>%
          filter(Sequencing.technology == 'Illumina' & Subregions == 'Eastern Africa'))[1],
    dim(data %>%
          filter(Sequencing.technology == 'Nanopore' & Subregions == 'Eastern Africa'))[1]),
  c(dim(data %>%
          filter(Sequencing.technology == 'Illumina' & Subregions == 'Central Africa'))[1],
    dim(data %>%
          filter(Sequencing.technology == 'Nanopore' & Subregions == 'Central Africa'))[1]),
  c(dim(data %>%
          filter(Sequencing.technology == 'Illumina' & Subregions == 'Western Africa'))[1],
    dim(data %>%
          filter(Sequencing.technology == 'Nanopore' & Subregions == 'Western Africa'))[1]),
  c(dim(data %>%
          filter(Sequencing.technology == 'Illumina' & Subregions == 'Southern Africa'))[1],
    dim(data %>%
          filter(Sequencing.technology == 'Nanopore' & Subregions == 'Southern Africa'))[1]))
# n.content
temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Northern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Northern Africa')

c(round(mean(temp1$N.content, na.rm=TRUE),4),
  round(std(temp1$N.content[-which(is.na(temp1$N.content))]),4),
  round(mean(temp2$N.content, na.rm=TRUE),4),
  round(std(temp2$N.content),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Eastern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Eastern Africa')

c(round(mean(temp1$N.content, na.rm=TRUE),4),
  round(std(temp1$N.content),4),
  round(mean(temp2$N.content, na.rm=TRUE),4),
  round(std(temp2$N.content),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Central Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Central Africa')

c(round(mean(temp1$N.content, na.rm=TRUE),4),
  round(std(temp1$N.content),4),
  round(mean(temp2$N.content, na.rm=TRUE),4),
  round(std(temp2$N.content),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Western Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Western Africa')

c(round(mean(temp1$N.content, na.rm=TRUE),4),
  round(std(temp1$N.content[-which(is.na(temp1$N.content))]),4),
  round(mean(temp2$N.content, na.rm=TRUE),4),
  round(std(temp2$N.content[-which(is.na(temp2$N.content))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Southern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Southern Africa')

c(round(mean(temp1$N.content, na.rm=TRUE),4),
  round(std(temp1$N.content[-which(is.na(temp1$N.content))]),4),
  round(mean(temp2$N.content, na.rm=TRUE),4),
  round(std(temp2$N.content),4))

# substitution
temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Northern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Northern Africa')

c(round(mean(temp1$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp1$totalSubstitutions[-which(is.na(temp1$totalSubstitutions))]),4),
  round(mean(temp2$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp2$totalSubstitutions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Eastern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Eastern Africa')

c(round(mean(temp1$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp1$totalSubstitutions),4),
  round(mean(temp2$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp2$totalSubstitutions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Central Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Central Africa')

c(round(mean(temp1$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp1$totalSubstitutions),4),
  round(mean(temp2$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp2$totalSubstitutions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Western Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Western Africa')

c(round(mean(temp1$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp1$totalSubstitutions[-which(is.na(temp1$totalSubstitutions))]),4),
  round(mean(temp2$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp2$totalSubstitutions[-which(is.na(temp2$totalSubstitutions))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Southern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Southern Africa')

c(round(mean(temp1$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp1$totalSubstitutions[-which(is.na(temp1$totalSubstitutions))]),4),
  round(mean(temp2$totalSubstitutions, na.rm=TRUE),4),
  round(std(temp2$totalSubstitutions),4))

# TAT
temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Northern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Northern Africa')

c(round(mean(temp1$TAT, na.rm=TRUE),4),
  round(std(temp1$TAT[-which(is.na(temp1$TAT))]),4),
  round(mean(temp2$TAT, na.rm=TRUE),4),
  round(std(temp2$TAT[-which(is.na(temp2$TAT))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Eastern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Eastern Africa')

c(round(mean(temp1$TAT, na.rm=TRUE),4),
  round(std(temp1$TAT[-which(is.na(temp1$TAT))]),4),
  round(mean(temp2$TAT, na.rm=TRUE),4),
  round(std(temp2$TAT[-which(is.na(temp2$TAT))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Central Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Central Africa')

c(round(mean(temp1$TAT, na.rm=TRUE),4),
  round(std(temp1$TAT[-which(is.na(temp1$TAT))]),4),
  round(mean(temp2$TAT, na.rm=TRUE),4),
  round(std(temp2$TAT[-which(is.na(temp2$TAT))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Western Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Western Africa')

c(round(mean(temp1$TAT, na.rm=TRUE),4),
  round(std(temp1$TAT[-which(is.na(temp1$TAT))]),4),
  round(mean(temp2$TAT, na.rm=TRUE),4),
  round(std(temp2$TAT[-which(is.na(temp2$TAT))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Southern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Southern Africa')

c(round(mean(temp1$TAT, na.rm=TRUE),4),
  round(std(temp1$TAT[-which(is.na(temp1$TAT))]),4),
  round(mean(temp2$TAT, na.rm=TRUE),4),
  round(std(temp2$TAT[-which(is.na(temp2$TAT))]),4))

# total deletion
temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Northern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Northern Africa')

c(round(mean(temp1$totalDeletions, na.rm=TRUE),4),
  round(std(temp1$totalDeletions[-which(is.na(temp1$totalDeletions))]),4),
  round(mean(temp2$totalDeletions, na.rm=TRUE),4),
  round(std(temp2$totalDeletions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Eastern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Eastern Africa')

c(round(mean(temp1$totalDeletions, na.rm=TRUE),4),
  round(std(temp1$totalDeletions),4),
  round(mean(temp2$totalDeletions, na.rm=TRUE),4),
  round(std(temp2$totalDeletions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Central Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Central Africa')

c(round(mean(temp1$totalDeletions, na.rm=TRUE),4),
  round(std(temp1$totalDeletions),4),
  round(mean(temp2$totalDeletions, na.rm=TRUE),4),
  round(std(temp2$totalDeletions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Western Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Western Africa')

c(round(mean(temp1$totalDeletions, na.rm=TRUE),4),
  round(std(temp1$totalDeletions[-which(is.na(temp1$totalDeletions))]),4),
  round(mean(temp2$totalDeletions, na.rm=TRUE),4),
  round(std(temp2$totalDeletions[-which(is.na(temp2$totalDeletions))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Southern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Southern Africa')

c(round(mean(temp1$totalDeletions, na.rm=TRUE),4),
  round(std(temp1$totalDeletions[-which(is.na(temp1$totalDeletions))]),4),
  round(mean(temp2$totalDeletions, na.rm=TRUE),4),
  round(std(temp2$totalDeletions),4))

# total insertation
temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Northern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Northern Africa')

c(round(mean(temp1$totalInsertions, na.rm=TRUE),4),
  round(std(temp1$totalInsertions[-which(is.na(temp1$totalInsertions))]),4),
  round(mean(temp2$totalInsertions, na.rm=TRUE),4),
  round(std(temp2$totalInsertions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Eastern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Eastern Africa')

c(round(mean(temp1$totalInsertions, na.rm=TRUE),4),
  round(std(temp1$totalInsertions),4),
  round(mean(temp2$totalInsertions, na.rm=TRUE),4),
  round(std(temp2$totalInsertions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Central Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Central Africa')

c(round(mean(temp1$totalInsertions, na.rm=TRUE),4),
  round(std(temp1$totalInsertions),4),
  round(mean(temp2$totalInsertions, na.rm=TRUE),4),
  round(std(temp2$totalInsertions),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Western Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Western Africa')

c(round(mean(temp1$totalInsertions, na.rm=TRUE),4),
  round(std(temp1$totalInsertions[-which(is.na(temp1$totalInsertions))]),4),
  round(mean(temp2$totalInsertions, na.rm=TRUE),4),
  round(std(temp2$totalInsertions[-which(is.na(temp2$totalInsertions))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Southern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Southern Africa')

c(round(mean(temp1$totalInsertions, na.rm=TRUE),4),
  round(std(temp1$totalInsertions[-which(is.na(temp1$totalInsertions))]),4),
  round(mean(temp2$totalInsertions, na.rm=TRUE),4),
  round(std(temp2$totalInsertions),4))

# totalNonACGTNs
temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Northern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Northern Africa')

c(round(mean(temp1$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp1$totalNonACGTNs[-which(is.na(temp1$totalNonACGTNs))]),4),
  round(mean(temp2$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp2$totalNonACGTNs),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Eastern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Eastern Africa')

c(round(mean(temp1$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp1$totalNonACGTNs),4),
  round(mean(temp2$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp2$totalNonACGTNs),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Central Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Central Africa')

c(round(mean(temp1$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp1$totalNonACGTNs),4),
  round(mean(temp2$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp2$totalNonACGTNs),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Western Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Western Africa')

c(round(mean(temp1$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp1$totalNonACGTNs[-which(is.na(temp1$totalNonACGTNs))]),4),
  round(mean(temp2$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp2$totalNonACGTNs[-which(is.na(temp2$totalNonACGTNs))]),4))

temp1 <- data %>%
  filter(Sequencing.technology == 'Illumina' & Subregions == 'Southern Africa')
temp2 <- data %>%
  filter(Sequencing.technology == 'Nanopore' & Subregions == 'Southern Africa')

c(round(mean(temp1$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp1$totalNonACGTNs[-which(is.na(temp1$totalNonACGTNs))]),4),
  round(mean(temp2$totalNonACGTNs, na.rm=TRUE),4),
  round(std(temp2$totalNonACGTNs),4))

# TAT more than 500
A <- data %>%
  filter(!TAT =="NA")
sum(A$TAT>500) # 3819

# TAT more than 500 and VoC
B <- A %>%
  filter(TAT > 500)
table(B$Variant_call)
5+1+10 # 16

# VoC median
b <- A %>%
  filter(Variant_call == "beta")
median(b$TAT) # 55

d <- A %>%
  filter(Variant_call == "delta")
median(d$TAT) # 40

o <- A %>%
  filter(Variant_call == "omicron")
median(o$TAT) # 19

# TAT in the total data set
sum(A$TAT%in% c(0:30)) # 37736
sum(A$TAT%in% c(30:60)) # 40211

# shortest TAT
temp <- split(A,A$Country)
sort(sapply(temp,function(x) mean(x$TAT)),decreasing = FALSE)

twofilters <- data %>%
  filter(!Variant_call%in% "other")
library(ggplot2)

ggplot(twofilters[twofilters$totalSubstitutions<100,], 
       aes(x=TAT, y=totalSubstitutions, color=Variant_call)) +
  geom_point() + scale_x_continuous(breaks = seq(0, 670, 10))+
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme(legend.position = "bottom", text = element_text(size = 8)) +
  scale_color_manual(values = c("alpha" = "red",
                                "beta"="orange",
                                "delta"="green",
                                "gamma" = "blue",
                                "omicron" = "purple")) +
  guides(colour = guide_legend(nrow = 1)) + 
  geom_vline(xintercept = 30, linetype="dotted", 
             color = "black", linewidth=1.5) + 
  geom_vline(xintercept = 60, linetype="dotted", 
             color = "black", linewidth=1.5) + 
  geom_vline(xintercept = 300, linetype="dotted", 
             color = "black", linewidth=1.5) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  xlab("Turnaround Time") + ylab("Number of Substitutions") +
  labs(title = "Number of Substituton by TAT with technology and variant filters applied",
       subtitle  = "Number of observations: 35387, VoC: alpha(2130), beta(8511), delta(19673), gamma(19), omicron(5054)")
