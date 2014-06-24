
iplocation <- function(ip=""){
  response    <- readLines(paste("http://www.datasciencetoolkit.org//ip2coordinates/",ip,sep=""))
  success     <- !any(grepl("null",response))
  ip <- grep("[[:digit:]]*\\.[[:digit:]]*\\.[[:digit:]]*\\.[[:digit:]]*",response,value=T)
    match <- regexpr("[[:digit:]]*\\.[[:digit:]]*\\.[[:digit:]]*\\.[[:digit:]]*",ip)
    ip <- substr(ip,match,as.integer(attributes(match)[1])+match-1)
  if(success==T){
    extract <- function(label,response){
            text <- grep(label,response,value=T)
            match <- regexpr(paste('"',label,'"',": ",sep=""),text)
            text <- substr(text,match+as.integer(attributes(match)[1]),nchar(text))
            if(grepl("[[:digit:]]",text)){
                    text <- substr(text,1,nchar(text)-2)
            }
            else{
                    text <- substr(text,2,nchar(text)-2)
                }
            if( regexpr('"',text)!= -1){
                text<-substr(text,2,nchar(text))
            }
            print(text)
            text
        }
  }
  RESULT <- list()
  RESULT$success     <- success
  RESULT$ip          <- ip
  if(success==T){
    RESULT$latitude    <- as.numeric(extract("latitude",response))
    RESULT$longitude   <- as.numeric(extract("longitude",response))
    RESULT$country     <- extract("country_name",response)
    RESULT$locality    <- extract("locality",response)
    RESULT$postalcode  <- extract("postal_code",response)
    RESULT$region      <- extract("region",response)
    RESULT$countrycode <- extract("country_code3",response)
  }
  RESULT
}

ip <- read.csv(file.choose())$ip_address ##raw_turk_7.csv

IP.Dat <- NULL
for (i in unique(ip)[-c(16,33)]){
	Res <- iplocation(i)
	if(Res$success == F){
		Res$latitude = NA
		Res$longitude = NA
		Res$country = NA
	}
	IP.dat <- data.frame(i = i, lat = Res$latitude, long = Res$longitude, IP = Res$ip, Country = Res$country )
	IP.Dat <- rbind(IP.Dat, IP.dat)
}

IP.Dat <- subset(IP.Dat, !is.na(Country))

require(ggplot2)
require(maps)

#Get world map info
world_map <- map_data("world")

#Creat a base plot
p <- ggplot() + coord_fixed()

#Add map to base plot
base_world <- p + geom_polygon(data=world_map,
                               aes(x=long,
                                   y=lat,
                                   group=group), col = I("grey30"), fill = I("grey30"))

map_with_jitter <- base_world+geom_point(data=IP.Dat,
                                            aes(x=long,
                                                y=lat),
                                                col = I("skyblue"),
                                         position="jitter") + theme(legend.position = "none")


map_with_jitter + xlab("") + ylab("")

###

IP.Dat$Countries <- ifelse(IP.Dat$Country == "India" |IP.Dat$Country == "United States", IP.Dat$Country, "Others" )

qplot(reorder(Countries, Countries,length), data = IP.Dat) + coord_flip() + xlab("Countries")