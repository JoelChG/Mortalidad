# Script to generate datasets
library(tidyverse); library(plotly)
library(kableExtra); library(stringr)

fileList <- list.files(path = "C:/Users/chave/Desktop/Mortalidad", 
                       full.names = TRUE, 
                       include.dirs = TRUE)

dirs <- list.files(path = fileList[1], 
                   full.names = FALSE)

csvPaths <- c()

for(i in 1:9){
        path <- paste(fileList[i], dirs[2], sep = "/")
        csvPaths <- c(csvPaths, list.files(path = path,
                                           full.names = TRUE))
}

datasets <- grep("defunciones", csvPaths, ignore.case = TRUE)

dataFrames <- list()
for(i in datasets){
        dataFrames[[i]] <- read.csv(file = csvPaths[i], 
                                    header = TRUE)
}

mortalidad2012 <- dataFrames[[1]]
mortalidad2013 <- dataFrames[[2]]
mortalidad2014 <- dataFrames[[3]]
mortalidad2015 <- dataFrames[[4]]
mortalidad2016 <- dataFrames[[5]]
mortalidad2017 <- dataFrames[[6]]
mortalidad2018 <- dataFrames[[8]]
mortalidad2019 <- dataFrames[[9]]
mortalidad2020 <- dataFrames[[10]]
rm(dataFrames)

data.frame(
        Year = c(2012:2020), 
        Observations = c(
                dim(mortalidad2012)[1],
                dim(mortalidad2013)[1],
                dim(mortalidad2014)[1],
                dim(mortalidad2015)[1],
                dim(mortalidad2016)[1],
                dim(mortalidad2017)[1],
                dim(mortalidad2018)[1],
                dim(mortalidad2019)[1],
                dim(mortalidad2020)[1]
        ), 
        Variables = c(
                dim(mortalidad2012)[2],
                dim(mortalidad2013)[2],
                dim(mortalidad2014)[2],
                dim(mortalidad2015)[2],
                dim(mortalidad2016)[2],
                dim(mortalidad2017)[2],
                dim(mortalidad2018)[2],
                dim(mortalidad2019)[2],
                dim(mortalidad2020)[2]
        )
) %>%
        kbl() %>%
        kable_styling(bootstrap_options = "striped",
                      full_width = TRUE,
                      position = "center")

mortalidad2012 <- mortalidad2012 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2013 <- mortalidad2013 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2014 <- mortalidad2014 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2015 <- mortalidad2015 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2016 <- mortalidad2016 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2017 <- mortalidad2017 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2018 <- mortalidad2018 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2019 <- mortalidad2019 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )
mortalidad2020 <- mortalidad2020 %>% 
        select(ent_ocurr,
               causa_def,
               sexo,
               edad,
               dia_ocurr,
               mes_ocurr,
               anio_ocur
        )

reclass <- function(x){
        transmute(x,
                  state = as.factor(ent_ocurr), 
                  dec_cause = as.factor(causa_def), 
                  gender = as.factor(sexo),
                  age = as.character(edad),
                  date = as.Date(paste(
                          dia_ocurr, 
                          mes_ocurr, 
                          anio_ocur, 
                          sep = "/"
                  ), "%d/%m/%Y")
        )
}

mortalidad2012 <- reclass(mortalidad2012)
mortalidad2013 <- reclass(mortalidad2013)
mortalidad2014 <- reclass(mortalidad2014)
mortalidad2015 <- reclass(mortalidad2015)
mortalidad2016 <- reclass(mortalidad2016)
mortalidad2017 <- reclass(mortalidad2017)
mortalidad2018 <- reclass(mortalidad2018)
mortalidad2019 <- reclass(mortalidad2019)
mortalidad2020 <- reclass(mortalidad2020)

catalDirs <- list(
        paste(fileList[1], dirs[1], sep = "/"), 
        paste(fileList[2], dirs[1], sep = "/"), 
        paste(fileList[3], dirs[1], sep = "/"), 
        paste(fileList[4], dirs[1], sep = "/"), 
        paste(fileList[5], dirs[1], sep = "/"), 
        paste(fileList[6], dirs[1], sep = "/"), 
        paste(fileList[7], dirs[1], sep = "/"), 
        paste(fileList[8], dirs[1], sep = "/"), 
        paste(fileList[9], dirs[1], sep = "/")
)

catList <- c(5)
get_catalogues <- function(n){
        x <- catalDirs[[n]]
        y <- list.files(x, full.names = TRUE)[catList]
        print(y)
}
cats <- list()
for(i in 1:9){
        x <- get_catalogues(i)
        cats[[i]] <- c(x)
}

metadata <- list()
for(i in 1:9){
        metadata[[i]] <- read.csv(cats[[i]])
}

cie10 <- data.frame(metadata[[1]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2012$dec_cause <- factor(x = mortalidad2012$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[2]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2013$dec_cause <- factor(x = mortalidad2013$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[3]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2014$dec_cause <- factor(x = mortalidad2014$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[4]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2015$dec_cause <- factor(x = mortalidad2015$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[5]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2016$dec_cause <- factor(x = mortalidad2016$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[6]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2017$dec_cause <- factor(x = mortalidad2017$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[7]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2018$dec_cause <- factor(x = mortalidad2018$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[8]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2019$dec_cause <- factor(x = mortalidad2019$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)
cie10 <- data.frame(metadata[[9]])
cnames <- c("code", "desc")
colnames(cie10) <- cnames
mortalidad2020$dec_cause <- factor(x = mortalidad2020$dec_cause, 
                                   levels = cie10$code, 
                                   labels = cie10$desc)


mortalidad2012 <- mortalidad2012 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2013 <- mortalidad2013 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2014 <- mortalidad2014 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2015 <- mortalidad2015 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2016 <- mortalidad2016 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2017 <- mortalidad2017 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2018 <- mortalidad2018 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2019 <- mortalidad2019 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)
mortalidad2020 <- mortalidad2020 %>% 
        extract(age, 
                into = c("age_code", "age_value"),
                "(.{1})(.{3})", remove=TRUE)

numericAge <- function(x){
        ageCodes <- c("Horas", "Dias", "Meses", "años")
        if(x$age_code == "1"){
                x$age_value <- as.numeric(x$age_value)
                mutate(x, age_numeric = x$age_value, 
                       age_factor = ageCodes[1])
        } else if(x$age_code == "2"){
                x$age_value <- as.numeric(x$age_value)
                mutate(x, age_numeric = x$age_value, 
                       age_factor = ageCodes[2])
        } else if(x$age_code == "3"){
                x$age_value <- as.numeric(x$age_value)
                mutate(x, age_numeric = x$age_value, 
                       age_factor = ageCodes[3])
        } else if(x$age_code == "4"){
                x$age_value <- as.numeric(x$age_value)
                mutate(x, age_numeric = x$age_value, 
                       age_factor = ageCodes[4])
        }
}

mortalidad2012 <- numericAge(mortalidad2012)
mortalidad2013 <- numericAge(mortalidad2013)
mortalidad2014 <- numericAge(mortalidad2014)
mortalidad2015 <- numericAge(mortalidad2015)
mortalidad2016 <- numericAge(mortalidad2016)
mortalidad2017 <- numericAge(mortalidad2017)
mortalidad2018 <- numericAge(mortalidad2018)
mortalidad2019 <- numericAge(mortalidad2019)
mortalidad2020 <- numericAge(mortalidad2020)

mortalidad2012$age_value <- as.numeric(mortalidad2012$age_value)
mortalidad2012 <- mortalidad2012 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2013$age_value <- as.numeric(mortalidad2013$age_value)
mortalidad2013 <- mortalidad2013 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2014$age_value <- as.numeric(mortalidad2014$age_value)
mortalidad2014 <- mortalidad2014 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2015$age_value <- as.numeric(mortalidad2015$age_value)
mortalidad2015 <- mortalidad2015 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2016$age_value <- as.numeric(mortalidad2016$age_value)
mortalidad2016 <- mortalidad2016 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2017$age_value <- as.numeric(mortalidad2017$age_value)
mortalidad2017 <- mortalidad2017 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2018$age_value <- as.numeric(mortalidad2018$age_value)
mortalidad2018 <- mortalidad2018 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2019$age_value <- as.numeric(mortalidad2019$age_value)
mortalidad2019 <- mortalidad2019 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)
mortalidad2020$age_value <- as.numeric(mortalidad2020$age_value)
mortalidad2020 <- mortalidad2020 %>%
        filter(age_code == "4") %>%
        select(state, 
               dec_cause, 
               gender, 
               age_numeric,
               date)

MortalidadGeneral <- bind_rows(
        mortalidad2012, 
        mortalidad2013,
        mortalidad2014,
        mortalidad2015,
        mortalidad2016,
        mortalidad2017,
        mortalidad2018,
        mortalidad2019,
        mortalidad2020)

write.csv(MortalidadGeneral,
          "C:/Users/chave/Desktop/Mortalidad/Mortalidad/MortalidadGeneral.csv", 
          row.names = FALSE)

MortalidadGeneral <- MortalidadGeneral %>%
        mutate(month = as.factor(format(date, "%m")), 
               year = as.factor(format(date, "%Y")))
Mortalidad_estados <- MortalidadGeneral %>%
        group_by(state, dec_cause, year) %>%
        summarize(n())
Mortalidad_anual <- MortalidadGeneral %>%
        group_by(dec_cause, year) %>%
        summarize(n())
MortalidadTotal_anual_estados <- MortalidadGeneral %>%
        group_by(state, year) %>%
        summarize(n())
MortalidadTotal_anual <- MortalidadGeneral %>%
        group_by(gender, year) %>%
        summarize(n())
