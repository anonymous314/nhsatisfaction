
load_unitsurvey <- function(unitfile){
  require(dplyr)
  require(readxl)
  require(reshape2)
  
  origdata <- read_excel(unitfile, 
                         sheet = "Särskilt boende, enheter", skip = 3) 
  
  cleandata <- origdata %>%
    ## remove disaggregated NH units
    filter(is.na(`Enhetens namn som del av boendet (t.ex. avdelning vid boende )`) |
             `Enhetens namn som del av boendet (t.ex. avdelning vid boende )` == "Aggregerad") %>%
    ## generate final varliables
    mutate(name = `Boendets namn i Äldreguiden`,
           muni = gsub(",.*","",Kommunnamn), ## remove names of sub-municipal areas for matching
           munin = substr(`Kommun-/stadsdels-kod`,1,4),## shorten muni ID numbers to not include sub-municipal areas for matching
           unitprivate = ifelse(Regiform == "Enskild",1,0)) %>%
    dplyr::select(-(1:4),-(8:9))
  
}



load_usersurvey <- function(userfile) {
  require(dplyr)
  require(readxl)
  require(reshape2)
  
  origdata <- read_excel(userfile, sheet = "Sammanst enheter",
                         #col_names = F,
                         na = "x", 
                         skip = 10,
                         col_types = c(rep("text",8),rep("numeric",164)))[-c(1,2),]
  
  colnames(origdata)[4:9] <- c("n1","n2","n3","n4","svarsfrekvens","regi.enskilt")
  
  cleandata <- origdata %>% 
    dplyr::select(-contains("X_"),"X__132","X__133") %>% 
    filter(!is.na(`Kommun-kod`))
  
  # Identify municipalities and NHs which report their data at the wrong level manually
  
  nerkommun <- c("Härjedalen",
                 "Norrtälje",
                 "Sigtuna",
                 "Uppsala",
                 "Nyköping",
                 "Flen",
                 "Katrineholm",
                 "Finspång",
                 "Norrköping",
                 "Vaggeryd",
                 "Jönköping",
                 "Värnamo",
                 "Uppvidinge",
                 "Mörbylånga",
                 "Kalmar",
                 "Nybro",
                 "Osby",
                 "Vara",
                 "Västervik",
                 "Borgholm", 
                 "Ronneby", 
                 "Kristianstad",
                 "Ängelholm",
                 "Hässleholm",
                 "Varberg",
                 "Sotenäs",
                 "Munkedal",
                 "Mölndal",
                 "Kungälv", 
                 "Vänersborg",
                 "Trollhättan",
                 "Gullspång",
                 "Torsås", 
                 "Alingsås",
                 "Lidköping",
                 "Forshaga",
                 "Jokkmokk", 
                 "Karlstad",
                 "Lindesberg",
                 "Vansbro", 
                 "Malung-Sälen",
                 "Ludvika",
                 "Ljusdal",
                 "Bollnäs",
                 "Hudiksvall",
                 "Östersund",
                 "Luleå")
  nerniva <- c("Centrala Kumla",
               "Privat utförare",
               "Boende i Täby", 
               "Enskild", 
               "Offentlig",
               "Vård & omsorg", 
               "Vardaga Äldreomsorg AB",
               "Särskilt boende",
               "Kommunal",
               "Skänninge/Väderstad",
               "Omsorg i Helsingborg",
               "Vardaga",
               "Extern")
  
  # one municipality/NH needed to me moved down two levels
  cleandata[cleandata$`Kommun/stadsdel` == "Örebro" & cleandata$n1 == "Egen Regi", 4:7] <- c(cleandata[cleandata$`Kommun/stadsdel` == "Örebro" & cleandata$n1 == "Egen Regi",6:7],NA,NA)
  
  # move NHs which are reported at the wrong level down one (from n2 to n1)
  cleandata[cleandata$`Kommun/stadsdel` %in% nerkommun | cleandata$n1 %in% nerniva, 4:7] <- c(cleandata[cleandata$`Kommun/stadsdel` %in% nerkommun | cleandata$n1 %in% nerniva,5:7],NA)
  
  cleandata <- cleandata %>% 
    # filter out disaggregated NH units and "unknown providers"
    filter(is.na(n2) & !is.na(n1) & !(n1 == "Okänd utförare")) %>%
    # Generate final variables
    mutate(name = n1,
           munin = ifelse(nchar(`Kommun-kod`) == 3, paste0("0",`Kommun-kod`),`Kommun-kod`),
           userprivate = round(regi.enskilt/100),## Rounded down one NH which reported 45% private ownership
           userresponse = ((as.numeric(substr(svarsfrekvens,1,2))+10)/100)) %>% # range was always 20%, so took lower bound and added 10 to generate an average response rate
    dplyr::select(-(1:9))
  
  return(cleandata)
}

load_muni <- function(munifile){
  require(tidyr)
  require(dplyr)
  munidata <- read_excel(munifile) 
  
  names(munidata) <- c("var","muni","year","value")
  
  munidata <- spread(munidata, key=var,value=value) %>% dplyr::select(-year)
  names(munidata) <- c("muni","pop65innh","pop65","popkm","costperpt","nhage","polcontrol","taxpower")
  munidata$polcontrol <- munidata$polcontrol - 1 ## Rescale this so that negative values represent left control, and positive values represent right control
  
  return(munidata)
}

clean_user_data <- function(userdata){
  
  # Shorten variable names
  
  colnames(userdata)[1:27] <- gsub( " .*$", "", colnames(userdata)[1:27])
  colnames(userdata)[1:27] <- gsub( "F", "user", colnames(userdata)[1:27])
  
  # Handle question 26
  
  colnames(userdata)[c(25,27,28)] <- c("user26_self","user26_help","user26_other")
  
  return(userdata)
}


clean_unit_data <- function(unitdata,corrfile){
  
  # Correct some unit names that were different in the user survey
  
  namecorr <- read.csv2(corrfile) %>%
    mutate(munin = ifelse(nchar(munin) == 3, paste0("0",munin), munin))
  
  for(i in 1:nrow(namecorr)){
    unitdata$name[which(unitdata$name == namecorr$name[i])] <- as.character(namecorr$newname[i])
  }
  
  # Shorten variable names
  
  colnames(unitdata)[9:31] <- paste0("unit",gsub( ". .*$", "", colnames(unitdata)[9:31]))
  
  
  # select variables for analysis
  unitdata <- unitdata %>%
    dplyr::select(name,
                  muni,
                  munin,
                  size = names(unitdata)[4], # Hack to get around encoding errors, sorry.
                  unitprivate,
                  typegen = names(unitdata)[1],
                  typedem = names(unitdata)[2],
                  typeserv = names(unitdata)[3],
                  9:31) %>%
    # Transform variables for analysis
    mutate(
      size = as.numeric(size),
      typegen = ifelse(typegen == "Ja",1,0),
      typedem = ifelse(typedem == "Ja",1,0),
      typeserv = ifelse(typeserv == "Ja",1,0),
      unit1 = ifelse(unit1 == "Ja",1,0),
      unit1a = as.factor(unit1a),
      unit2 = as.numeric(unit2),
      unit3 = as.numeric(unit3),
      unit4 = ifelse(unit4 == "Ej aktuellt",NA,ifelse(unit4 == "Ja",1,0)),
      unit5 = as.numeric(unit5),
      unit6a = ifelse(unit6a == "Ja",1,0),
      unit6b = ifelse(unit6b == "Ja",1,0),
      unit6c = ifelse(unit6c == "Ja",1,0),
      unit7 = ifelse(unit7 == "Ja",1,0),
      unit8 = ifelse(unit8 == "Ja",1,0),
      unit8a = as.factor(unit8a),
      unit8b = as.factor(unit8b),
      unit9 = ifelse(unit9 == "Ja",1,0),
      unit10 = ifelse(unit10 == "Ja",1,0),
      unit11 = ifelse(unit11 == "Ja, för alla",1,0), # 12 units reported a positive response for "some patients". Categorized these as negative
      unit12 = ifelse(unit12 == "Ja, för alla",1,0), # 8 units reported a positive response for "some patients". Categorized these as negative
      unit13 = as.numeric(unit13),
      unit14 = as.numeric(unit14),
      unit15 = as.numeric(unit15),
      unit16 = as.numeric(unit16),
      unit17 = as.numeric(unit17),
      unit18 = as.numeric(unit18)) %>% 
    ## recode binned interval variables to average number of times per month
    mutate(unit1a = as.numeric(as.character(recode_factor(unit1a,
                                                          "Ej aktuellt" = 0,
                                                          "Mer sällan än en gång i halvåret" = 0.17,
                                                          "Mer sällan än en gång i månaden men minst en gång i halvåret" = 0.5,
                                                          "Mer sällan än en gång i veckan men minst en gång i månaden" = 1,
                                                          "En gång i veckan eller oftare" = 4))),
           unit8a = as.numeric(as.character(recode_factor(unit8a,
                                                          "Mer sällan än en gång i månaden eller inte alls" = 0.25,
                                                          "Minst en gång i månaden" = 0.5,
                                                          "En till två gånger per vecka" = 1.5,
                                                          "Tre till sex gånger per vecka" = 4.5,
                                                          "Sju gånger i veckan eller oftare" = 8))),
           unit8b = as.numeric(as.character(recode_factor(unit8b,
                                                          "Mer sällan än en gång i månaden eller inte alls" = 0.25,
                                                          "Minst en gång i månaden" = 0.5,
                                                          "En gång i veckan eller oftare" = 4))))
  
  return(unitdata)
  
}



aggregate_user_data <- function(userdata){
  
  userdata$sattot <- userdata %>% dplyr::select(user4:user19,user21:user25,user27) %>% scale() %>% rowMeans(na.rm=TRUE) %>% scale() %>% as.numeric()
  userdata$srhtot <- userdata %>% dplyr::select(user1:user3,user20) %>% scale() %>% rowMeans(na.rm=TRUE) %>% scale() %>% as.numeric()
  userdata$fill_self <- userdata %>% dplyr::select(user26_self,user26_help) %>% rowSums(na.rm = FALSE)
  
  userdata <- userdata %>% dplyr::select(name,
                                      munin,
                                      userprivate,
                                      userresponse,
                                      sattot,
                                      srhtot,
                                      user24,
                                      fill_self) %>%
    mutate(srhtot = ifelse(is.nan(srhtot),NA,srhtot),
           sattot = ifelse(is.nan(sattot),NA,sattot)) ## get rid of NaNs produces by scaling
  return(userdata)  

}

aggregate_unit_data <- function(data){
  
  data$residentcouncil <- data %>% dplyr::select(unit1:unit1a) %>% scale() %>% rowMeans(na.rm = TRUE)
  data$actionplan <- data %>% dplyr::select(unit2:unit3) %>% scale() %>% rowMeans(na.rm = TRUE)
  data$meals <- data %>% dplyr::select(unit4:unit5) %>% scale() %>% rowMeans(na.rm = TRUE)
  data$safetyroutines <- data %>% dplyr::select(unit6a:unit7) %>% scale() %>% rowMeans(na.rm = TRUE)
  data$activity <- data %>% dplyr::select(unit8a:unit8b) %>% scale() %>% rowMeans(na.rm = TRUE)
  data$carecoord <- data %>% dplyr::select(unit9:unit10) %>% scale() %>% rowMeans(na.rm = TRUE)
  data$medreview <- data %>% dplyr::select(unit11:unit12) %>% scale() %>% rowMeans(na.rm = TRUE)
  
  
  
  # aggregate staffing level variables to give equal weight to each day
  data$rns <- rowSums(cbind(data$unit13*5,data$unit14*2),na.rm=TRUE)/7
  data$rns[data$rns == 0] <- NA
  data$staff <- rowSums(cbind(data$unit15*5,data$unit16*2),na.rm=TRUE)/7
  data$staff[data$staff == 0] <- NA
  data$edu <- rowSums(cbind(data$unit17*5,data$unit18*2),na.rm=TRUE)/7
  data$edu[data$edu == 0] <- NA
  
  data <- data %>% dplyr::select(name,
                                 muni,
                                 munin,
                                 size, 
                                 unitprivate, 
                                 typegen, 
                                 typedem, 
                                 typeserv, 
                                 residentcouncil, 
                                 actionplan,
                                 meals,
                                 safetyroutines,
                                 activity,
                                 carecoord,
                                 medreview,
                                 rns,
                                 staff, 
                                 edu)
  
  return(data)
  
}


combine_unit_user_muni <- function(unitdata,userdata,munidata){
  
  combdata <- inner_join(unitdata,userdata, by= c("name","munin"))
  combdata <- left_join(combdata,munidata, by="muni")
  combdata$id <- paste(combdata$munin,combdata$name)
  
  # In 10 cases, conflicting data on public/private ownership was reported. 
  # We were able to establish the correct form of ownership through internet 
  # searches, except for in one case where ownership appeared to change during,
  # 2016, which we excluded.
  
  privcorr <- c("Lenalundsgården" = 0,
              "Sofiagårdens VoB" = 1,
              "Roma äldreboende" = 0, 
              "Styrkan   -   IN" = 0,
              "Patrikshills äldreboende" = 1,
              "Ängsviken" = 0, 
              "Brogården äldreboende" = NA,
              "Båtsmansgärdet" = 0,
              "Trumslagarbackens ålderdomshem" = 1, 
              "Västervik" = 0)
  
  combdata$unitprivate[which(combdata$unitprivate != combdata$userprivate)] <- privcorr
  
  combdata$private <- combdata$unitprivate
  
  combdata <- dplyr::select(combdata,-unitprivate,-userprivate)
  
  return(combdata)
  
}


transform_variable_types <- function(data, cutoff){
  #data <- 
    ifelse(length(unique(data)) < cutoff,
           as.factor(data),
           ifelse(sum(!is.na(sapply(data,as.numeric))) / length(data) > 0.5, 
                  as.numeric(data),
                  as.character(data)))
  #return(data)
}

descriptive_table <- function(data){
  data.frame(
    mean = round(sapply(data, mean, na.rm=TRUE),2),
    stdev = round(sapply(data, sd, na.rm=TRUE),2),
    median = round(sapply(data, median, na.rm=TRUE),2),
    iqr = round(sapply(data, IQR, na.rm=TRUE),2),
    missing = sapply(data, function(x){sum(is.na(x))}))
}

get_names <- function(x){
  sapply(x, function(y){
    y <- prettynames[which(names(prettynames) == y)]
  })
}

coefs_controlled <- function(test,control,data, out = "sattot", w = 1){
  
  if(length(w)==1) w = rep(w,nrow(data))
  
  frml <- formula(paste(out,"~",test,control))
  
  fit <- robcov(ols(frml,data=data,x=TRUE,y=TRUE))
  
  return(c(fit$coefficients[2], confint(fit)[2,]))
  
}

coefs_single <- function(test,data, out = "sattot", w = 1){
  
  if(length(w)==1) w = rep(w,nrow(data))
  
  frml <- formula(paste(out,"~",test))
  
  fit <- robcov(ols(frml,data=data,x=TRUE,y=TRUE, weights = w))
  
  return(c(fit$coefficients[2], confint(fit)[2,]))
  
}

coefs_ml_controlled <- function(test,control,data, out = "sattot", w = 1){
  
  if(length(w)==1) w = rep(w,nrow(data))
  
  frml <- formula(paste(out,"~",test,control,"+ pop65innh + pop65 + popkm + costperpt +  nhage + polcontrol + taxpower + (1|munin)"))
  
  fit <- lmer(frml,data=data, weights = w)
  
  fs <- summary(fit)
  
  return(c(fs$coefficients[2,1], confint(fit, method = "boot")[4,]))
  
}

coefs_ml_single <- function(test,data, out = "sattot", w = 1){
  
  if(length(w)==1) w = rep(w,nrow(data))
  
  frml <- formula(paste(out,"~",test,"+ pop65innh + pop65 + popkm + costperpt + nhage + polcontrol + taxpower + (1|munin)"))
  
  fit <- lmer(frml,data=data, weights = w)
  
  fs <- summary(fit)
  
  return(c(fs$coefficients[2,1], confint(fit, method = "boot")[4,]))
  
}

