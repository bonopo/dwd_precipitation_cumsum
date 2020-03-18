#install.packages(c("pack"))
 # install.packages("imager")
sapply(c("dplyr", "magrittr", "utils", "tidyverse", "lubridate", "RCurl","imager", "gridExtra"), require, character.only = T )
setwd("C:/Users/Menke/Documents/Uni/R_practice/scripts/precip_cumsum_germany/")


precip.cumsum = function(id = "01443", cnp = FALSE, year = as.integer(substr(date(), 21,24)))
{
   #preambel
  do.call(file.remove, list(list.files("./extr_data/rec/", full.names = TRUE)))
  do.call(file.remove, list(list.files("./extr_data/old/", full.names = TRUE)))
  hist_file = NULL


  #downloading recent
download.file(paste0("ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_", id, "_akt.zip"), destfile = "rec.zip",  mode="wb")




unzip("./rec.zip", exdir = "./extr_data/rec")

#importing recent
files= list.files("./extr_data/rec")
clima_rec = read.csv2(paste0("./extr_data/rec/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill=F, sep=";", dec=".")%>% 
  mutate(date=ymd(MESS_DATUM))

#downloading historic
url <- "ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
filenames <- getURL(url,ftp.use.epsv = FALSE,dirlistonly = TRUE) 
filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
hist_file = grep(as.character(id), filenames) 
  if(is.null(hist_file))
    {
    stop("No historic Data was found online.")
  }

download.file(filenames[hist_file], destfile = "old.zip",  mode="wb")
unzip("./old.zip", exdir = "./extr_data/old")

#importing historic
files= list.files("./extr_data/old")
clima_old = read.csv2(paste0("./extr_data/old/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill=F, sep=";", dec=".") %>% 
  mutate(date=ymd(MESS_DATUM))

#rbinding the two df together
#correcting date for merging

clima_cpl = clima_rec %>% 
  filter(date > clima_old$date %>% tail(.,1)) %>% 
  rbind(clima_old,. )

#no NAs (if not error)
 clima_cpl$RSK[which(clima_cpl$RSK<0)] = 0 
 #can not do NA if not cumsum doesn't work (better average of that period)
 clima_cpl$RSK[is.na(clima_cpl$RSK<0)] = 0 

#climate normal period
if(cnp == TRUE){
cat("Please enter Climate Normal Period (Default: 1961 - 1990; leave empty for default!!)")
cnp_begin = readline(prompt = paste("The Time series begins", clima_cpl$date[1],"\n")) %>% as.integer()
cnp_end = readline(prompt = paste("The Time Series ends", tail(clima_cpl$date,1),"\n")) %>% as.integer()
} else { 
  cnp_begin = 1961
  cnp_end = 1990
}
  
 
 
 
#climate data of interest subset
if(year == as.integer(substr(date(), 21,24)))
  {
  clima_int = clima_cpl %>% 
  filter(year(date) >= year ) %>% 
    mutate(cs_ns = cumsum(RSK))
}else{
  clima_int = clima_cpl %>% 
  filter(year(date) >= year & year(date) < year+1) %>% 
    mutate(cs_ns = cumsum(RSK))
}

clima_ref = clima_cpl %>% 
  filter(year(date) >= cnp_begin & year(date) < cnp_end+1) %>% 
  mutate(day = dmy(paste0(day(date), "-",month(date), "-" , year))) %>%   group_by(day) %>% 
  summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>% 
  mutate(cum_sum = cumsum(mn_dy_ns))

#percentage of rain 
int = which(tail(clima_int$date,1) == clima_ref$day)
if(is.numeric(int)){
  ratio_precip = (clima_int$cs_ns[int]/clima_ref$cum_sum[int]) *100
}else{
  ratio_precip = NA
}
clima_int %<>% select(date, cs_ns) %>% as.tbl
clima_ref %<>% select(date=day, cum_sum) %>% as.tbl()

plot_data = merge(x=clima_int, y = clima_ref, by= "date", all.x = T )

print(
  ggplot(data = plot_data)+
  geom_line(aes(x=date, y= cs_ns))+
  geom_line(data= clima_ref, aes(x=date, y=cum_sum), col="red", show.legend = T)+
  ylab("cumulative precipitaion [mm]")+
     xlab(paste0("1.1.",cnp_begin," - ","31.12.",cnp_end))+
    theme(axis.title.x = element_text(colour = "red"))+
    annotate(geom="text", clima_int$date[1], max(clima_int$cs_ns),  hjust = -0.2, vjust = -1, label=paste0(round(ratio_precip,1)," %"))+
    ggtitle(paste("id:",id, clima_int$date[nrow(clima_int)]))
    
) 


 }

#cnp = edit climate normal period (year to compate to)
#year= which year to analyse
#id= which city (freiburg is default)
# stationsname: 
#freiburg = 01443
#hannover =02014 
# dessau (sachsen-anhalt) = 00948 #gibts net
#HH = 01975  
#neu strelitz = 03577 #gibts net

#hannover
precip.cumsum(cnp=F, year = 2017, id="02014")
precip.cumsum(cnp=F, year = 2018, id="02014")
precip.cumsum(cnp=F, year = 2019, id="02014")
precip.cumsum(cnp=F, year = 2020, id="02014")
#freiburg
precip.cumsum(cnp=F, year = 2020, id="01443")

#hamburg
precip.cumsum(cnp=F, year = 2019, id="01975")
