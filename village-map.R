# read in csv of reports
reports <- st_read("Kaitlyn Data/Kellymadeitwork2_18_16.csv")
u_reports<-reports[, -c(3, 4, 5, 6, 7)]
u_reports<- unique(u_reports)

#making a table out of the count of villages 
length(unique(u_reports$Village))
table<- u_reports %>% 
    count(Village)
table<- rename(table, NAME = Village)
merged<- merge(villages, table, by = "NAME", all.x= TRUE)
View(merged)

#atiteti weird lat long
merged<-merged[!(merged$NAME=="Atiteti"),]

ggplot(merged) +
    geom_sf(aes(size = n))