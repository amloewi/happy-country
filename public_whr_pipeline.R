library(DAAG)
library(glmnet)
library(stringr)
library(ggplot2)
library(data.table)
library(missForest)
library(RColorBrewer)

# library(reshape2)
library(reshape)


######################################################################
######################### A NOTE TO THE USER #########################
######################################################################
# This code is provided entirely for transparency, and replication of
# early stage data cleaning and joining processes. (For the ambitious,
# it can provide a helpful starting place to add more data to the search.)
# It is NOT required to replicate the models or main results of the paper,
# which can be done entirely with the code in the RMarkdown file that
# contains the text of the paper, as this draws on pre-processed data.
# If you do choose to build the dataset from scratch be aware that
# it involves a highly intensive imputation process that took 10+ 
# hours to run on the original machine where it was executed.
#
# In addition, one key component of the data is NOT included with
# the provided files, being too large. It can be found at 
# https://v-dem.net/data/the-v-dem-dataset/country-year-v-dem-fullothers-v13/
######################################################################


# The finished file -- for convenience.
# df11 <- read.csv("code and data/df11 imputed 5-7.csv", stringsAsFactors = F)

# SET THIS TO POINT AT YOUR OWN COPY OF THE PROVIDED FILES
setwd("/path/to/code and data")

syn <- read.csv("~/Code/tmip/whr saved/code and data/country_synonym_table.csv")

# A function that takes a table of *standardized* country names, and 
# swaps them in to the given dataset in place of the non-standard ones
# Critical to have this centralized for so many inconsistent datasets.
standardize_countries <- function(df, country="country", dedupe=T) {
    
    df[, country] <- trimws(df[, country])
    syn$country <- trimws(syn$country)
    syn$correction <- trimws(syn$correction)
    
    for (i in 1:nrow(df)) {
        if (df[i, country] %in% syn$country) {
            standardized <- syn$correction[which(syn$country==df[i, country])][1]
            if(standardized=="") { print(df$Country.name[i])}
            df[i, country] <- standardized
        } else {
            # Either it's okay, or it's totally missing
        }
    }
    
    # Some of these are only in one place, but this is easier than hunting
    # 1) remove the country 'xx'
    df <- df[df[, country]!="xx", ]
    # 2) remove the leading space in "Palestinian Territories"
    df[, country][df[, country]==" Palestinian Territories"] <- "Palestinian Territories"
    # 3) remove the asterisks (but it appears it's only in whr2022)
    df[, country] <- str_remove(df[, country], fixed("*"))
    # 4) remove dupes (Why are there some doubles, i.e 10 appearances? Not many though.)
    if (dedupe) {
        df <- df[!duplicated(df[, country]), ] # NO
    }
    
    return(df[df[, country]!="", ])
}

# A function that merges two dfs by country but only after calling
# standardize_countries on both of them
setwd("~/Code/tmip/whr saved/code and data/")
syn_path <- "country_synonym_table.csv"
country_merge <- function(x, y, x_country="country", y_country="country", syn_path=syn_path) {
    syn <- read.csv(syn_path, stringsAsFactors = F)
    x <- standardize_countries(x, x_country)
    y <- standardize_countries(y, y_country)
    
    for (i in 1:nrow(y)) {
        if (!(y[i, y_country]) %in% x[, x_country]) {
            print(simpleWarning(paste("The dataset didn't have:", y[i, y_country])))
        }
    }
    z <- merge(x, y, by.x=x_country, by.y=y_country, all.x=T)
    return(z)
}

# Tests for country_merge
df1 <- data.frame('country'=c('Chile', 'Swaziland', 'Qatar'), 'nums'=4:6)
df2 <- data.frame('country'=c('Chil', 'Eswatini', 'Bongoland'), 'nums'=1:3)
out <- country_merge(df1, df2, 'country', 'country')

country_year_merge <- function(x, y) {

    if ('country' %in% names(x)) x[, 'country'] <- NULL
    if ('year'    %in% names(x)) x[, 'year'] <- NULL
    if ('country' %in% names(y)) y[, 'country'] <- NULL
    if ('year'    %in% names(y)) y[, 'year'] <- NULL
    
    for (i in 1:nrow(y)) {
        if (!(y[i, 'country_year']) %in% x[, 'country_year']) {
            print(simpleWarning(paste("The dataset didn't have:", y[i, 'country_year'])))
        }
    }
    z <- merge(x, y, by="country_year", all.x=T)
    return(z)
}

country_merge_multiple <- function(l) {
    # Takes a list of lists: list(list(path=p, country=c), ...)
    # Does assume each component is clean though, and ready for analysis;
    # have a function cleaning each as necessary!
    print(paste0("Merging ", l[[1]]$path))
    everything <- read.csv(l[[1]]$path, stringsAsFactors = F)
    for (i in 2:length(l)) {
        print(paste0("...with ", l[[i]]$path))
        next_df <- read.csv(l[[i]]$path, stringsAsFactors = F)
        everything <- country_merge(everything, next_df,
                                    x_country=l[[1]]$country,
                                    y_country=l[[i]]$country)
    }
    return(everything)
}


country_year_merge_multiple <- function(l) {
    # Takes a list of lists: list(list(path=p, country=c), ...)
    # Does assume each component is clean though, and ready for analysis;
    # have a function cleaning each as necessary!
    print(paste0("Merging ", l[[1]]$path))
    everything <- read.csv(l[[1]]$path, stringsAsFactors = F)
    for (i in 2:length(l)) {
        print(paste0("...with ", l[[i]]$path))
        next_df <- read.csv(l[[i]]$path, stringsAsFactors = F)
        everything <- country_year_merge(everything, next_df) #,
                                    #x_country=l[[1]]$country_year,
                                    #y_country=l[[i]]$country_year)
    }
    return(everything)
}


# Tests for country_merge_multiple
# cmm_test_l <- list(
#     list(path="whr/whr kaggle data/2019.csv",
#          country="Country.or.region"),
#     list(path="whr/whr 2019/Figure2.6-Table 1.csv",
#          country="Country"),
#     list(path="whr/whr 2019/Figure2.6-Table 1.csv",
#          country="Country")
# )
# out <- country_merge_multiple(cmm_test_l)
# plot(out$Score, out$Happiness.score.x) # looks good!


#################################
# CLEANING THE DATA FOR JOINING #
#################################

# Each of the following seven functions ingest, clean, and save, a 
# specific dataset. These components have the format that is expected
# for the next step, which joins them and imputes any missing values.
# They draw on files in the '/raw components' directory, 
# (with the exception of the vdem file mentioned in the introduction) and output
# finished products to the '/clean components' directory. 
# Subsequent code then reads and joins these cleaned files.

prepare_whr <- function() {
    whr <- read.csv("raw components/DataForTable2.1WHR2023.csv", stringsAsFactors = F)
    whr <- standardize_countries(whr, "Country.name", dedupe=F)
    whr$country_year <- mapply(paste0, whr$Country.name, sep="_", whr$year) # weird; put sep in the middle
    whr$Country.name <- NULL
    write.csv(whr, "clean components/whr country_year clean.csv", row.names=F)
    return(whr)
}
whr <- prepare_whr()


prepare_vdem <- function() {
    vdem <- read.csv("raw components/V-Dem-CY-Full+Others-v13.csv",
                     stringsAsFactors = F)
    vdem <- standardize_countries(vdem, "country_name", dedupe=F)
    vdem$country_year <- mapply(paste0, vdem$country_name, sep="_", vdem$year)
    vdem <- vdem[, -c(1:3, 5:22)] # keep 'year'
    for (j in 2:(ncol(vdem)-1)) { # exempt "country_year", now at the end
        vdem[, j] <- as.numeric(vdem[, j])
    }
    codes <- c("_codelow", "_codehigh", "_nr", "_sd", "_mean", "_osp", "_ord")
    to_remove <- grep(paste(codes, collapse="|"), names(vdem))
    vdem <- vdem[, -to_remove]
    write.csv(vdem, "clean components/vdem country_year clean.csv", row.names = F)
    return(vdem)
}
vdem <- prepare_vdem()


prepare_spi <- function() {
    spi <- read.csv("raw components/2011-2022 SPI dataset-Table 1.csv", stringsAsFactors = F, skip=2)
    spi <- standardize_countries(spi, "Country", dedupe=F)
    spi$country_year <- mapply(paste0, spi$Country, sep="_", spi$SPI..year)
    spi$year <- spi$SPI..year
    spi <- spi[, -c(1:5)]
    vars <- !(names(spi) %in% c("country_year", "year"))
    names(spi)[vars] <- paste0("spi_", names(spi)[vars])
    write.csv(spi, "clean components/spi country_year clean.csv", row.names=F)
    return(spi)
}
spi <- prepare_spi()


prepare_unemployment <- function() {
    une <- read.csv("raw components/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_5994651.csv",
                    skip=4)
    # https://stackoverflow.com/questions/57435780/name-variables-during-multiple-melt-with-data-table
    une <- melt(une, measure.vars=names(une)[grepl("X", names(une))], value.name="value")
    une$variable <- as.character(1960:2022)[une$variable]
    une <- standardize_countries(une, "Country.Code", dedupe=F)
    une$country_year <- mapply(paste0, une$Country.Code, sep="_", une$variable)
    une <- une[, c("country_year", "value", "variable")]
    names(une)[2:3] <- c("wb_unemployment", "year")
    write.csv(une, paste0("clean components/unemployment country_year clean.csv"),
              row.names=F)
    return(une)
}
une <- prepare_unemployment()


prepare_sdg_ed <- function() {
    sdg <- read.csv("raw components/SDG_DATA_NATIONAL.csv",
                    stringsAsFactors = F)
    sdg <- dcast(sdg, country_id + year ~ indicator_id, value.var="value", fun.aggregate = sum)
    sdg_names <- read.csv("raw components/SDG_LABEL.csv",
                          stringsAsFactors = F)
    sdg[sdg==0] <- NA
    contained <- sdg_names$INDICATOR_ID[sdg_names$INDICATOR_ID %in% names(sdg)]
    contained <- sort(contained)
    sdg_names <- sdg_names[sdg_names$INDICATOR_ID %in% contained, ]
    sdg_names <- sdg_names[order(sdg_names$INDICATOR_ID), ]
    sdg <- standardize_countries(sdg, "country_id", dedupe=F)
    sdg$country_year <- mapply(paste0, sdg$country_id, sep="_", sdg$year)
    sdg <- sdg[, c("country_year", "year", contained)]
    names(sdg) <- c("country_year", "year", paste0("ed_", sdg_names$INDICATOR_LABEL_EN))
    write.csv(sdg, "clean components/ed country_year clean.csv", row.names=F)
    return(sdg)
}
sdged <- prepare_sdg_ed()


prepare_sdg <- function() {
    sdg <- read.csv("raw components/SDGData.csv", stringsAsFactors = F)
    # https://stackoverflow.com/questions/57435780/name-variables-during-multiple-melt-with-data-table
    sdg <- melt(sdg, measure.vars=names(sdg)[grepl("X", names(sdg))], value.name="value")
    sdg$variable <- as.character(1990:2020)[sdg$variable]
    sdg <- dcast(sdg, Country.Code + variable ~ Indicator.Name,
                 value.var="value", fun.aggregate = sum)
    sdg <- standardize_countries(sdg, "Country.Code", dedupe=F)
    sdg$country_year <- mapply(paste0, sdg$Country.Code, sep="_", sdg$variable)
    sdg$year <- sdg$variable
    sdg$Country.Code <- NULL
    sdg$variable <- NULL
    vars <- !(names(sdg) %in% c("country_year", "year"))
    names(sdg)[vars] <- paste0("sdg_", names(sdg)[vars])
    write.csv(sdg, paste0("clean components/sdg country_year clean.csv"), row.names=F)
    return(sdg)
}
sdg <- prepare_sdg()


prepare_spilong <- function() {
    spilong <- read.csv("raw components/1990-2020 Time-series data-Table 1.csv",
                        stringsAsFactors = F, skip=1)
    spilong <- standardize_countries(spilong, "Country", dedupe=F)
    spilong$country_year <- mapply(paste0, spilong$Country, sep="_", spilong$SPI..year)
    spilong$year <- spilong$SPI..year
    spilong <- spilong[, -c(1:6)]
    vars <- !(names(spilong) %in% c("country_year", "year"))
    names(spilong)[vars] <- paste0("spilong_", names(spilong)[vars])
    
    spilong$spilong_Population.size..number.of..people. <- as.numeric(
        str_remove_all(
            trimws(spilong$spilong_Population.size..number.of..people.),
        ",")
    )
    
    write.csv(spilong, "clean components/spilong country_year clean.csv", row.names=F)
    return(spilong)
}
spilong <- prepare_spilong()

prepare_sipri_2 <- function() {
    percap <- read.csv("~/Code/tmip/whr saved/SIPRI-Milex-data-1949-2024_2/Per capita-Table 1.csv",
                      skip=6, na.strings = "...")
    dollars <- read.csv("~/Code/tmip/whr saved/SIPRI-Milex-data-1949-2024_2/Constant (2023) US$-Table 1.csv",
                       skip=5, na.strings = "...")
    pctgdp <- read.csv("~/Code/tmip/whr saved/SIPRI-Milex-data-1949-2024_2/Share of GDP-Table 1.csv",
                       skip=5, na.strings = "...")
    pctgov <- read.csv("~/Code/tmip/whr saved/SIPRI-Milex-data-1949-2024_2/Share of Govt. spending-Table 1.csv",
                       skip=7, na.strings = "...")
    
    dfs <- list("percap"=percap, "dollars"=dollars, "pctgdp"=pctgdp, "pctgov"=pctgov)
    complete <- list()
    for (i in 1:length(dfs)) {
        dfs[[i]] <- reshape::melt(dfs[[i]], id.vars="Country", measure.vars=names(dfs[[i]])[-(1:3)])
        dfs[[i]]$year <- sapply(strsplit(as.character(dfs[[i]]$variable), "X"),
                                function (x) x[2])
        dfs[[i]]$variable <- NULL
        dfs[[i]]$value <- as.numeric(strsplit(as.character(dfs[[i]]$value), "%"))
        names(dfs[[i]])[1] <- "country" # lower
        dfs[[i]]$country <- as.character(dfs[[i]]$country)
        names(dfs[[i]])[2] <- c("sipri_percap", "sipri_dollars", "sipri_pctgdp",
                                "sipri_pctgov")[i]
        
        # error if I replace obj in list with a different sized object
        complete[[i]] <- standardize_countries(dfs[[i]], "country", dedupe=F)
        complete[[i]]$country_year <- paste0(complete[[i]]$country, "_", complete[[i]]$year)
        # Else messy after joins
        complete[[i]]$country <- NULL
        complete[[i]]$year <- NULL
    }
    # Merge on country_year (meaning creation, meaning standardize, is above)
    sipri <- merge(complete[[1]], complete[[2]], by="country_year")
    sipri <- merge(sipri,         complete[[3]], by="country_year")
    sipri <- merge(sipri,         complete[[4]], by="country_year")

    sipri[sipri=="xxx"] <- NA
    sipri[sipri==""] <- NA
    
    # output
    # done?
    write.csv(sipri, "~/Code/tmip/whr saved/code and data/clean components/sipri all country_year clean.csv", row.names=F)
    return(sipri)
    
    # names(sipri)[1] <- "country"
    # sipri <- standardize_countries(sipri, "country")
    # names(sipri)[-1] <- paste0("sipri_", names(sipri)[-1])
    # sipri[sipri==""] <- NA
    # sipri[sipri=="xxx"] <- NA
    # sipri <- sipri[, -(2:19)] # cut Notes, 1988-2004
    # melted <- melt(sipri, id="country", measure=names(sipri)[-1])
    # 
    # # names(melted)[2] <- 'year'
    # melted$year <- unlist(lapply(strsplit(as.character(melted$variable), "sipri_X"), function(x) x[2]))
    # melted$country_year <- paste0(melted$country, "_", melted$year)
    # melted <- melted[, -2] # remove "variable, ex. sipri_X2005 etc
    # names(melted)[2] <- "per.capita.military.spending" # was "value"
    # 
}

# https://data.worldbank.org/indicator/SM.POP.NETM
# API_SM.POP.NETM_DS2_en_csv_v2_85182
prepare_immigration <- function() {
    img <- read.csv("~/Code/tmip/whr saved/API_SM.POP.NETM_DS2_en_csv_v2_85182/API_SM.POP.NETM_DS2_en_csv_v2_85182.csv",
                    skip = 3)
    img <- reshape::melt(img, id.vars=1, measure.vars=names(img)[5:69])
    names(img) <- c("country", "Xyear", "immigration")
    img$year <- sapply(strsplit(as.character(img$Xyear), "X"), function (x) x[2])
    img <- standardize_countries(img, dedupe=F)
    img$country_year <- paste0(img$country, "_", img$year)
    # write and done?
    img$year <- NULL
    img$Xyear <- NULL    
    img$country <- NULL
    write.csv(img, "~/Code/tmip/whr saved/code and data/clean components/img country_year clean.csv",
              row.names=F)
    return(img)
}
img <- prepare_immigration()

# https://ourworldindata.org/migration#explore-data-on-migration-refugees-and-asylum-seekers
# "~/Downloads/share-of-the-population-that-was-born-in-another-country.csv"
prepare_foreign <- function() {
    fgn <- read.csv("~/Code/tmip/whr saved/share-of-the-population-that-was-born-in-another-country.csv")
    names(fgn) <- c("country", "year", "pct.foreign.born")
    fgn <- standardize_countries(fgn, dedupe=F)
    fgn$country_year <- paste0(fgn$country, "_", fgn$year)
    fgn$country <- NULL
    fgn$year <- NULL
    write.csv(fgn, "~/Code/tmip/whr saved/code and data/clean components/foreign country_year clean.csv", row.names=F)
    return(fgn)
}
foreign <- prepare_foreign()

setwd("~/Code/tmip/whr saved/code and data")
data_sources_list_2005_2023 <- list(
    list(path="clean components/whr country_year clean.csv", key="country_year")
    , list(path="clean components/vdem country_year clean.csv", key="country_year")
    , list(path="clean components/spi country_year clean.csv", key="country_year")
    , list(path="clean components/unemployment country_year clean.csv", key="country_year")
    , list(path="clean components/ed country_year clean.csv", key="country_year")
    , list(path="clean components/sdg country_year clean.csv", key="country_year")
    , list(path="clean components/spilong country_year clean.csv", key="country_year")
    
    , list(path="clean components/sipri all country_year clean.csv", key="country_year")    
    , list(path="clean components/img country_year clean.csv", key="country_year")
    , list(path="clean components/lit country_year clean.csv", key="country_year")
    , list(path="clean components/foreign country_year clean.csv", key="country_year")
)
dfwhr_all <- country_year_merge_multiple(data_sources_list_2005_2023)

dfwhr_all$immigration.per.capita <- (dfwhr_all$immigration / dfwhr_all$spilong_Population.size..number.of..people.)
dfwhr_all$spilong_GDP.per.capita..PPP...constant.2017.international... <- NULL # there's a 2019 version
write.csv(dfwhr_all, "whr country_year not imputed 5-7.csv", row.names=F) # 2311 x 3920
# write.csv(dfwhr_all, "whr country_year plus variables not imputed.csv", row.names=F) # 2199 x 3922

dfwhr_all <- read.csv("whr country_year not imputed 5-7.csv", stringsAsFactors = F)



###########################
####### THE MODELS ########
###########################

################################################################################
# THIS BLOCK OF CODE TAKES SEVERAL HOURS TO RUN                                #
# For convenience we recommend simply using the completed, imputed, dataset    #
################################################################################
#
whr <- read.csv("~/Code/tmip/whr saved/whr country_year not imputed 5-7.csv")
whr$year <- unlist(lapply(strsplit(whr$country_year, "_"), function (x) x[2]))
df11 <- whr[whr$year >= 2011,]

# 'service' (below) is 27.3% missing, but is the single substantial variable from the 
# 2005 model that is <25% in the 2005+ data, but >25% in the 2011+ data. 
# We thus made this exception for it to see if it is still recovered.
# Then, as the answer is Yes but it does not meet threshold rules, in some models it is 
# replaced by Women's Social and Political Power that is well within threshold,
# and substantial contributor, and in fact has a smaller p-value. 

# Preserves that one variable exception
service <- df11$sdg_Employment.in.services..female....of.female.employment...modeled.ILO.estimate.
df11 <- df11[, colMeans(is.na(df11)) < 0.25]
df11$sdg_Employment.in.services..female....of.female.employment...modeled.ILO.estimate. <- service

df11 <- df11[!duplicated(df11$country_year), ]

set.seed(5)
###############################################################
###### This imputation takes 4+ hours on an M3 w/24gb ram #####
###############################################################
Sys.time()
df11_imp = missForest(df11[, -which(names(df11) %in% c("year", "country_year"))])$ximp
Sys.time()

df11_pre_impute <- df11

df11_imp$year        <- as.character(df11$year) # [-391]
df11_imp$year_effect <- as.factor(df11_imp$year)
df11_imp$country_year <- as.character(df11$country_year) #[-391]
df11_imp$country <- unlist(sapply(df11_imp$country_year, function(x) {strsplit(x, "_")[[1]][1]}))

##### write.csv(df11_imp, "lifesat joined y>=2011 na<25 imputed +country_year.csv", row.names=F) # just a different name
# write.csv(df11_imp, "main model data.csv", row.names=F)

df11 <- write.csv(df11_imp, "df11 imputed 5-7.csv")
df11 <- read.csv("df11 imputed 5-7.csv")



#####################################
##### Doing the variable search #####
#####################################

# A very lengthy function with two major roles.
# 1. Selecting and modeling random subsets of the dataset, to 
#    produce the final set of "votes" on which variables to include.
# 2. Aggregating the collected statistics
# 
# It has the capacity to impute missing values in each random subset,
# but this is especially time-consuming. After multiple tests it was also 
# determined that this was not necessary, and we recommend it be used on 
# complete dataset (imputed outside of this function) for speed.
search_procedure_long <- function(df, 
                                  outcome, 
                                  to_remove, 
                                  max_col_na_pct, 
                                  max_row_na_num, 
                                  bootstrap_pct,
                                  bad_rows, 
                                  n_iterations,
                                  lambda_exponent,
                                  col_bootstrap_pct=1, 
                                  cache_imputations=FALSE,
                                  return_imputations=FALSE,
                                  adaptive=F
){
    
    timestamp()
    
    df <- df[, colMeans(is.na(df)) < max_col_na_pct]
    df <- df[rowSums(is.na(df)) < max_row_na_num, ]
    
    # This has to be done here, before the random subsetting
    N <- nrow(df)
    ix <- (1:N)[!(1:N %in% bad_rows)]
    
    to_remove <- to_remove[!(to_remove %in% outcome)]
    to_remove <- c(to_remove,
                   c("country")) # error, otherwise; interprets as factor
    
    if (any(which(names(df) %in% to_remove))) {
        cleaned <- df[ix, -which(names(df) %in% to_remove)]
    } else {
        cleaned <- df[ix, ]
    }
    
    names(cleaned) <- make.names(names(cleaned))
    mf <- list()
    
    ###
    # 1. makes a NEW imputed data set, and 2. takes a bootstrap sample of it
    ###
    
    set.seed(5)
    single_imputation <- missForest(cleaned)$ximp
    outcome_ix <- which(names(cleaned)==outcome)
    cleaned <- cleaned[, c(outcome_ix, (1:ncol(cleaned))[-outcome_ix])] # put the outcome FIRST
    
    for (i in 1:n_iterations) {
        print(paste0("Imputation ", i, "/", n_iterations)) 
        set.seed(i)
        if (cache_imputations) {
            mf[[i]] <- single_imputation
        } else {
            mf[[i]] <- missForest(cleaned)$ximp 
        }
    }
    
    original_imputations <- mf # a copy to return with the results
    
    for (i in 1:n_iterations) {
        set.seed(i)                  
        mf[[i]] <- mf[[i]][sample(1:nrow(cleaned),      round(nrow(cleaned) * bootstrap_pct)-1),
                           c(1, sample(2:ncol(cleaned), round(ncol(cleaned) * col_bootstrap_pct)-1))] # 2:ncol to avoid the outcome var
    }
    
    ###
    # 1. For the GIVEN lambda, runs models on each data set, and keeps the non-zero vars
    # 2. Runs those in an lm, and saves the estimates
    ###
    all_results <- data.frame(names(cleaned))
    names(all_results) <- "variables"
    dynamic_lambda <- lambda_exponent
    for (j in 1:n_iterations) {
        
        if (!(outcome %in% names(mf[[j]]))) { next } 
        
        g <- glmnet(x = mf[[j]][, -(which(names(mf[[j]]) %in% outcome))],
                    y = mf[[j]][,  outcome], 
                    alpha=1,
                    lambda=exp(-dynamic_lambda))
        
        coefs <- coef(g)
        best <- rownames(coefs)[as.vector(abs(coefs) > 0)][-1]
        
        if (adaptive) {
            if (length(best) > 14) {
                dynamic_lambda <- dynamic_lambda -.1
            }
            if (length(best) < 13){
                dynamic_lambda <- dynamic_lambda +.1
            }
        }
        final.j <- data.frame(mf[[j]][, best])
        final.j$outcome <- mf[[j]][, outcome] 
        m.j <- lm(outcome ~ ., data=final.j)
        results <- data.frame(summary(m.j)$coefficients[, "Estimate", drop=F]) #* 
        results$variables <- row.names(results)
        all_results <- merge(all_results, results, by="variables", all=T)
    }
    
    ###
    # Calculates statistics of the estimated parameters, since each was estimated repeatedly
    ###
    row.names(all_results) <- all_results$variables
    
    mmm <- data.frame(colMeans(cleaned, na.rm=T))
    names(mmm) <- 'mean'
    mmm$bmin <- apply(cleaned, 2, min, na.rm=T)
    mmm$bmax <- apply(cleaned, 2, max, na.rm=T)
    mmm$variable <- rownames(mmm)
    
    all_results$variables <- NULL
    fc2 <- data.frame(rowMeans(all_results, na.rm=T))
    names(fc2) <- "b"
    fc2$frequency <- 1-rowMeans(is.na(all_results)) # remove the names    
    fc2 <- fc2[rowMeans(!is.na(all_results)) > 0,]
    fc2$variable <- rownames(fc2)
    
    fc2 <- merge(fc2, mmm, by='variable', all.x=T, all.y=F)
    fc2$bx <- fc2$b * fc2$mean
    # fc2$best  <- ifelse(fc2$b > 0, fc2$max, fc2$min)
    # fc2$worst <- ifelse(fc2$b < 0, fc2$max, fc2$min)
    fc2[is.na(fc2)] <- fc2$b[fc2$variable=="(Intercept)"]
    
    fc2 <- fc2[order(fc2$frequency, decreasing=T), ]
    fc2 <- fc2[, c("variable", "frequency", "bx", "b", "mean", "bmin", "bmax")]
    
    timestamp()
    
    if (return_imputations) {
        return(list(fc=fc2, imputations=original_imputations))
    } else {
        return(fc2)
    }
}


# A simple convenience, that runs search_procedure_long with all of the 
# parameter values set in advance.
search_procedure <- function(df, l, to_remove) {
    to_remove <- c(to_remove, c("Log.GDP.per.capita", # log is unjustified + incomparable
                                "Positive.affect",    # excluded by whr
                                "Negative.affect",    # excluded by whr
                                "year",               # below all not used in search
                                "year_effect",
                                "country_year",
                                "country"))
    set.seed(5)
    sp <- search_procedure_long(
        df,
        outcome="Life.Ladder", 
        to_remove=to_remove,
        max_col_na_pct=0.25,
        max_row_na_num=10000,
        bootstrap_pct=.95,
        col_bootstrap_pct = 1,
        bad_rows=c(),
        n_iterations=25,
        lambda_exponent=l,
        cache_imputations = T,
        return_imputations = T,
        adaptive=F
    )
    return(sp)
}


##################
# THE MAIN MODEL #
##################
# All search code is included in the the .rmd


# 0.49 => 0.52 -- pretty good.