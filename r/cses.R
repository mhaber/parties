(library("readr"))
(library("SAScii"))
(library("stringr"))
(library("dplyr"))
(library("tidyr"))

  
################################################
  ## convert raw data from dat/dct to R data frame
  
  cseszip <- unzip("data/csesmod1.zip", exdir="data/zip")
  datfile <- unzip("data/cm1_dat.txt")
  cses1 <- readr::read_csv("data/cm1_dat.txt")
  unlink(c(datfile))
  
  cseszip <- unzip("data/cses2.zip", exdir="data/zip"
  datfile <- unzip("data/cses2_rawdata.txt")
  cses2 <- readr::read_csv(datfile)
  unlink(c(datfile))
  
  cseszip <- unzip("data/cses3.zip", exdir="data/zip")
  datfile <- unzip("data/cses3.dat")
  sasfile <- unzip("data/cses3.sas")
  sastxt <- readLines(sasfile, encoding = "latin1")
  sastxt <- iconv(sastxt, "latin1", "UTF-8")
  writeLines(sastxt, sasfile)
  fwfrules <- SAScii::parse.SAScii(sasfile)
  fwfrules <- fwfrules[!is.na(fwfrules$varname), ]
  col_positions <- fwf_widths(fwfrules$width, col_names = fwfrules$varname)
  cses3 <- read_fwf(datfile, col_positions)
  unlink(c(datfile, sasfile))
  
  cseszip <- unzip("data/cses4.zip", exdir="data/zip"
  datfile <- unzip("data/cses4.dat")
  sasfile <- unzip("data/cses4.sas")
  sastxt <- readLines(sasfile, encoding = "latin1")
  sastxt <- iconv(sastxt, "latin1", "UTF-8")
  writeLines(sastxt, sasfile)
  fwfrules <- SAScii::parse.SAScii(sasfile)
  fwfrules <- fwfrules[!is.na(fwfrules$varname), ]
  col_positions <- fwf_widths(fwfrules$width, col_names = fwfrules$varname)
  cses4 <- read_fwf(datfile, col_positions)
  unlink(c(datfile, sasfile))
  
  #saveRDS(cses1, file.path(cacheDir, "cses1.rds"))
  #saveRDS(cses2, file.path(cacheDir, "cses2.rds"))
  #saveRDS(cses3, file.path(cacheDir, "cses3.rds"))
  #saveRDS(cses4, file.path(cacheDir, "cses4.rds"))
  
  
  ############################################
  ## extract country lr scores and vote shares
  
  extractCountryData <- function(data, idCol, polWghtCol, lrPrtyCols, vtShrLwrHouseCols, vtShrPresCols,
                                 vtShrUprHouseCols, lrPrtyExprtCols) {
    suppressPackageStartupMessages(suppressPackageStartupMessages(library("dplyr")))
    
    # subset to only the cols we need
    allCols <- c(idCol, polWghtCol, lrPrtyCols, vtShrLwrHouseCols, vtShrPresCols,
                 vtShrUprHouseCols, lrPrtyExprtCols)
    data <- data[, names(data) %in% c(allCols)]
    
    # set lr scores (lrPrtyCols and lrPrtyExprtCols) values > 10 to NA
    allScoreColNames <- c(lrPrtyCols, lrPrtyExprtCols)
    data[allScoreColNames] <- sapply(data[allScoreColNames], function(x) { ifelse(x > 10, NA, x) } )
    data <- data %>% mutate_each_(funs(as.numeric), allScoreColNames)
    
    # set vote shares (vtShrLwrHouseCols, vtShrPresCols, vtShrUprHouseCols) values > 100 to NA
    allShareColNames <- c(vtShrLwrHouseCols, vtShrPresCols, vtShrUprHouseCols)
    data[allShareColNames] <- sapply(data[allShareColNames], function(x) { ifelse(x > 100, NA, x) } )
    data <- data %>% mutate_each_(funs(as.numeric), allShareColNames)
    
    # make sure political weight is numeric also
    data <- data %>% mutate_each_(funs(as.numeric), polWghtCol)
    
    # combine Belgian regions from CSES1 into one group
    if (class(data[[1]]) == "factor") { levels(data[[1]])[levels(data[[1]]) %in% c("BELF1999", "BELW1999")] <- "BEL_1999"
    } else { data[data[[1]] %in% c("BELF1999", "BELW1999"), 1] <- "BEL_1999" }
    
    # combine German mail-back and telephone surveys from CSES2 into one group
    if (class(data[[1]]) == "factor") { levels(data[[1]])[levels(data[[1]]) %in% c("DEU22002", "DEU12002")] <- "DEU_2002"
    } else { data[data[[1]] %in% c("DEU22002", "DEU12002"), 1] <- "DEU_2002" }
    
    # lr scores
    lrPrty <- data %>% group_by_(idCol) %>%
      select_(.dots = c(lrPrtyCols, polWghtCol)) %>%
      rename_(polWghtCol = polWghtCol) %>%
      summarise_each(funs(ifelse(all(is.na(.)), as.numeric(NA), weighted.mean(., polWghtCol, na.rm=T)))) %>%
      select(-polWghtCol)
    # remove rows that are all NA
    lrPrty <- lrPrty[rowSums(is.na(lrPrty[,-1])) != ncol(lrPrty[,-1]), ]
    
    # vote shares lower house
    vtShrLwrHouse <- data %>% group_by_(idCol) %>%
      select(one_of(vtShrLwrHouseCols)) %>%
      distinct
    
    # vote shares president
    vtShrPres <- data %>% group_by_(idCol) %>%
      select(one_of(vtShrPresCols)) %>%
      distinct
    
    # vote shares upper house
    vtShrUprHouse <- data %>% group_by_(idCol) %>%
      select(one_of(vtShrUprHouseCols)) %>%
      distinct
    
    # pick a vote share set that is not all NA in order of preference
    vtShares <- vtShrLwrHouse
    
    allNaIdxs <- rowSums(is.na(vtShares[, -1])) == ncol(vtShares[, -1])
    vtShares[allNaIdxs, ] <- vtShrPres[allNaIdxs, ]
    
    allNaIdxs <- rowSums(is.na(vtShares[, -1])) == ncol(vtShares[, -1])
    vtShares[allNaIdxs, ] <- vtShrUprHouse[allNaIdxs, ]
    
    # remove rows that are all NA
    vtShares <- vtShares[rowSums(is.na(vtShares[,-1])) != ncol(vtShares[,-1]), ]
    
    # lr scores by experts
    lrPrtyExprt <- data %>% group_by_(idCol) %>%
      select(one_of(lrPrtyExprtCols)) %>%
      distinct
    # remove rows that are all NA
    lrPrtyExprt <- lrPrtyExprt[rowSums(is.na(lrPrtyExprt[,-1])) != ncol(lrPrtyExprt[,-1]), ]
    
    list(lrPrty = lrPrty, lrPrtyExprt = lrPrtyExprt, vtShares = vtShares,
         vtShrLwrHouse = vtShrLwrHouse, vtShrPres = vtShrPres,
         vtShrUprHouse = vtShrUprHouse)
  }
  
  
  #cses1 <- readRDS("dataCollection/cache/cses1.rds")
  
  idCol <- "A1004"
  polWghtCol <- "A1010_3"
  lrPrtyCols <- grep("^A3032", names(cses1), value = T)
  vtShrLwrHouseCols <- grep("^A5005", names(cses1), value = T)
  vtShrPresCols <- grep("^A5009", names(cses1), value = T)
  vtShrUprHouseCols <- grep("^A5007", names(cses1), value = T)
  lrPrtyExprtCols <- grep("^A5004", names(cses1), value = T)
  
  w1 <- extractCountryData(cses1, idCol, polWghtCol, lrPrtyCols, vtShrLwrHouseCols, vtShrPresCols, vtShrUprHouseCols, lrPrtyExprtCols)
  
  
  #cses2 <- readRDS("dataCollection/cache/cses2.rds")
  
  idCol <- "B1004"
  polWghtCol <- "B1010_3"
  lrPrtyCols <- grep("^B3038", names(cses2), value = T)
  vtShrLwrHouseCols <- grep("^B5001", names(cses2), value = T)
  vtShrPresCols <- grep("^B5005", names(cses2), value = T)
  vtShrUprHouseCols <- grep("^B5003", names(cses2), value = T)
  lrPrtyExprtCols <- grep("^B5018", names(cses2), value = T)
  
  w2 <- extractCountryData(cses2, idCol, polWghtCol, lrPrtyCols, vtShrLwrHouseCols, vtShrPresCols, vtShrUprHouseCols, lrPrtyExprtCols)
  
  
  #cses3 <- readRDS("dataCollection/cache/cses3.rds")
  
  idCol <- "C1004"
  polWghtCol <- "C1010_3"
  lrPrtyCols <- grep("^C3011", names(cses3), value = T)
  vtShrLwrHouseCols <- grep("^C5001", names(cses3), value = T)
  vtShrPresCols <- grep("^C5005", names(cses3), value = T)
  vtShrUprHouseCols <- grep("^C5003", names(cses3), value = T)
  lrPrtyExprtCols <- grep("^C5017", names(cses3), value = T)
  
  w3 <- extractCountryData(cses3, idCol, polWghtCol, lrPrtyCols, vtShrLwrHouseCols, vtShrPresCols, vtShrUprHouseCols, lrPrtyExprtCols)
  
  
  #cses4 <- readRDS("dataCollection/cache/cses4.rds")
  # remove certain countries as was with the version from BIGv1?
  
  idCol <- "D1004"
  polWghtCol <- "D1010_3"
  lrPrtyCols <- grep("^D3013", names(cses4), value = T)
  vtShrLwrHouseCols <- grep("^D5001", names(cses4), value = T)
  vtShrPresCols <- grep("^D5005", names(cses4), value = T)
  vtShrUprHouseCols <- grep("^D5003", names(cses4), value = T)
  lrPrtyExprtCols <- grep("^D5017", names(cses4), value = T)
  
  w4 <- extractCountryData(cses4, idCol, polWghtCol, lrPrtyCols, vtShrLwrHouseCols, vtShrPresCols, vtShrUprHouseCols, lrPrtyExprtCols)
  
  
  
  ############################################
  ## run polarizations
  
  #source("dataCollection/cses/new polarization function.R")
  
  polarizations <- function(sharesFrame) {
    suppressPackageStartupMessages(library("dplyr"))
    library("tidyr")
    
    names(sharesFrame$lrPrty)[1] <- "survey"
    names(sharesFrame$lrPrtyExprt)[1] <- "survey"
    names(sharesFrame$vtShares)[1] <- "survey"
    
    maxCols <- min(ncol(sharesFrame$lrPrty), ncol(sharesFrame$vtShares))
    joined <- dplyr::inner_join(sharesFrame$lrPrty[, 1:maxCols], sharesFrame$vtShares[, 1:maxCols])
    lrIdxs <- 1:(maxCols - 1)
    vtshIdxs <- maxCols:(2 * maxCols - 2)
    # only keep rows that have at least one matching lr and vtshr scroe for a party
    joined <- joined[rowSums(!is.na(joined[, vtshIdxs+1]) & !is.na(joined[, lrIdxs+1])) != 0, ]
    tjoined <- data.frame(t(joined[, -1]))
    names(tjoined) <- joined[[1]]
    joined$erPolar <- sapply(tjoined, function(x) Esteban.Ray(x[lrIdxs], x[vtshIdxs]))
    joined$stPolar <- sapply(tjoined, function(x) polarization(x[lrIdxs], x[vtshIdxs] / 100))
    ctzn <- joined %>% select_("survey", "erPolar", "stPolar")
    
    maxCols <- min(ncol(sharesFrame$lrPrtyExprt), ncol(sharesFrame$vtShares))
    joined <- dplyr::inner_join(sharesFrame$lrPrtyExprt[, 1:maxCols], sharesFrame$vtShares[, 1:maxCols])
    lrIdxs <- 1:(maxCols - 1)
    vtshIdxs <- maxCols:(2 * maxCols - 2)
    # only keep rows that have at least one matching lr and vtshr scroe for a party
    joined <- joined[rowSums(!is.na(joined[, vtshIdxs+1]) & !is.na(joined[, lrIdxs+1])) != 0, ]
    tjoined <- data.frame(t(joined[, -1]))
    names(tjoined) <- joined[[1]]
    joined$erExprtPolar <- sapply(tjoined, function(x) Esteban.Ray(x[lrIdxs], x[vtshIdxs]))
    joined$stExprtPolar <- sapply(tjoined, function(x) polarization(x[lrIdxs], x[vtshIdxs] / 100))
    exprt <- joined %>% select_("survey", "erExprtPolar", "stExprtPolar")
    
    joined <- dplyr::full_join(ctzn, exprt)
    joined <- joined %>% tidyr::separate(survey, into = c("iso3c", "year"), sep = "_")
    joined %>% select(iso3c, year, erPolar, stPolar, erExprtPolar, stExprtPolar)
  }
  
  out1 <- polarizations(w1)
  out2 <- polarizations(w2)
  out3 <- polarizations(w3)
  out4 <- polarizations(w4)
  
  out <- full_join(out3, out4) %>% full_join(out3) %>% full_join(out4)
  
  # add iso3n
  out <- out %>% dplyr::mutate(iso3n = iso3n(iso3c, type = "iso3c")) %>%
    dplyr::select(iso3n, everything())
  
  out <- out %>% gather("var", "value", -iso3c, -iso3n, -year, na.rm = T) %>%
    unite("var.year", var, year, sep = "_cses.") %>%
    spread(var.year, value) %>%
    rename(iso3c_cses = iso3c) %>%
    select(iso3n, everything())
  
  dplyr::tbl_df(out)
}