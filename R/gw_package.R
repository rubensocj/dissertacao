#' Function for importing packages
#'
#' Try to load a package, if it is not installed, perform installation
#' and loads in workspace
#'
#' @param x package names to be imported (string vector)
load_packages <- function(x) {
  for (pkg in x) {
    if(!require(pkg, character.only = TRUE)){
      install.packages(pkg, )
      library(pkg)
    }
  }
}

#' Function for import groundwater level files from RIMAS/CPRM website
#'
#' Imports a text file downloaded directly from RIMAS/CPRM website
#' with gorundwater level information from monitoring and converts
#' into a data.frame object with two column: Data and Nivel
#'
#' @param file path to text file (string)
#'
#' @return groundwater levels and date (data.frame)
import_RIMAS <- function(file) {
  require("dplyr")

  # file <- file.gwl[1]
  # import as text
  x <- base::readLines(file, encoding = "latin1")
  x <- x[-which(x==";;;;;;")]
  x <- x[-which(x=="")]

  # header (n) e data (x)
  n <- "N"#úmero de sequencia do nível d'agua;Data da medição;Hora da medição;Nível da água;Vazão;Bombeando;"
  for (i in 1:length(x)) {
    if (base::substr(x[i],1,1)==n) {
      kn <- i
      break
    }
  }
  n <- x[kn]
  x <- x[(kn+1):length(x)]

  # separe by ";"
  sx <- base::strsplit(x, split = ";")
  nx <- base::unlist(strsplit(n, split = ";"))

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- nx
  len <- nrow(xm)

  # date column correct
  xm <- xm %>% dplyr::mutate(Data = as.Date(xm[,2], "%d/%m/%Y"))

  # level column correct
  xm <- xm %>% dplyr::mutate(Nivel = as.numeric(gsub(",",".",xm[,4])))
  xm <- xm %>% dplyr::group_by(Data) %>% dplyr::mutate(Nivel = mean(Nivel))

  # date regularization in a new data frame
  dti <- xm$Data[1]
  dtf <- xm$Data[len]
  df <- data.frame(Data = seq(as.Date(dti), as.Date(dtf), by = 1))
  # df <- suppressMessages(left_join(df, select(xm, Nivel), by = "Data"))
  df <- suppressMessages(df %>% dplyr::left_join(xm))
  df <- df %>% dplyr::select(Data, Nivel)
  # names(df) <- c("Data", "Nivel")

  # day, month and year columns
  df <- df %>% dplyr::mutate(Ano = as.numeric(substr(Data, 1, 4)), .before = Nivel)
  df <- df %>% dplyr::mutate(Mes = as.numeric(substr(Data, 6, 7)), .before = Nivel)
  df <- df %>% dplyr::mutate(Dia = as.numeric(substr(Data, 9, 10)), .before = Nivel)

  return(df)
}

converttoDataframe_RIMAS <- function(x) {
  datas <- lapply(x, function(y) {
    return(as.Date(cbind(y$Data[1],y$Data[nrow(y)])))})
  datas <- data.frame(t(rbind.data.frame(datas)))
  colnames(datas) <- c("Start","End")
  datas$Start <- as.Date(datas$Start); datas$End <- as.Date(datas$End)
  start <- min(datas$Start); end <- max(datas$End)
  df <- data.frame(Data = seq(start, end, by = 1))
  for(i in 1:length(x)) {
    xdf <- x[[i]]
    df <- left_join(df, xdf, by = c("Data" = "Data"))
    df <- df %>% select(-one_of(c("Ano","Mes","Dia")))
    colnames(df)[ncol(df)] <- names(x)[i]
  }
  df <- df %>% mutate(
    Dia = as.numeric(substr(as.character(df$Data),9,10)),
    Mes = as.numeric(substr(as.character(df$Data),6,7)),
    Ano = as.numeric(substr(as.character(df$Data),1,4)), .after = Data)
  return(df)
}

#' Function for import ranfall files from FUNCEME website
#'
#' Imports a text file downloaded directly from FUNCEME main website
#' with rainfall information of pluviometric station and converts into
#' a data.frame with two columns: Data and Chuva
#'
#' @param file path to text file (string)
#'
#' @return a list containing a data.frame with rainfall and date,
#' and latitude and longitude as decimal geographic coordinates
import_FUNCEME <- function(file) {
  require("dplyr"); require("tidyr")

  # import text
  x <- readLines(file, encoding = "latin1")
  x <- x[-which(x=="")]

  # header (n) e data (x)
  n <- as.character("Posto")
  for (i in 1:length(x)) {
    if (substr(x[i],1,5)==n) {
      kn <- i
      break
    }
  }
  n <- x[kn]
  x <- x[(kn+1):length(x)]

  # separate by ","
  sx <- strsplit(x, split = ",")
  nx <- unlist(strsplit(n, split = ","))

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- nx
  len <- nrow(xm)

  # get geographic coordinates
  lat <- as.numeric(unique(xm$Latitude))
  lon <- as.numeric(unique(xm$Longitude))

  # converts to longitudinal data format
  pluvio <- tidyr::gather(xm[,-c(1,2,3,4)], dia, precip, Dia1:Dia31)
  names(pluvio) <- c("Ano", "Mes", "Dia", "Precip")
  pluvio <- pluvio %>% mutate(Dia = as.numeric(substr(Dia, 4, 5)))
  pluvio <- pluvio %>% mutate(Mes = as.numeric(Mes))
  pluvio <- pluvio %>% mutate(Ano = as.numeric(Ano))
  pluvio <- pluvio %>% arrange(Ano,Mes)
  pluvio <- pluvio %>% mutate(Data = paste0(sprintf("%02d", as.numeric(Dia)),"/",sprintf("%02d", as.numeric(Mes)),"/",Ano), .before = Ano)
  pluvio <- pluvio %>% mutate(Data = as.Date(Data, "%d/%m/%Y"))
  pluvio <- pluvio[!(pluvio$Precip == "*"),]
  pluvio$Precip[pluvio$Precip == "-"] <- NA
  pluvio <- pluvio %>% mutate(Precip = as.numeric(Precip))

  return(list(df = pluvio, latitude = lat, longitude = lon))
}

import_FUNCEME2 <- function(file) {
  require("dplyr"); require("tidyr")

  # file <- paste0("./man/chuva/mv2021/","298.txt")

  # import text
  x <- readLines(file, encoding = "latin1")

  # separate by ","
  sx <- strsplit(x, split = ";")

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- xm[1,]
  xm <- xm[-1,]
  len <- nrow(xm)

  # get geographic coordinates
  lat <- as.numeric(unique(xm$Latitude))
  lon <- as.numeric(unique(xm$Longitude))

  # data regularization in new data.frame
  pluvio <- data.frame(cbind(Ano=xm$Anos,Mes=xm$Meses,Precip=xm$Total))
  pluvio <- pluvio %>% mutate(AnoMes=as.Date(paste0(Ano,"/",Mes,"/1")), .before = Precip)

  dti <- paste0(xm$Anos[1],"/",xm$Meses[1],"/1")
  dtf <- paste0(xm$Anos[len],"/",xm$Meses[len],"/1")
  df <- data.frame(Data = seq(as.Date(dti), as.Date(dtf), by = "month"))
  df <- df %>% mutate(Ano = as.numeric(format(Data, "%Y")), Mes = as.numeric(format(Data, "%m"))); head(df)
  df <- suppressMessages(left_join(df, pluvio, by = c("Data"="AnoMes")))
  df <- df[,-c(1,4,5)]
  names(df) <- c("Ano", "Mes", "Precip")

  return(list(df = df, latitude = lat, longitude = lon))
}

import_FUNCEME3 <- function(file) {
  require("dplyr"); require("tidyr")

  # file <- paste0("./man/chuva/mv2021/","1.txt")

  # import text
  x <- readLines(file, encoding = "latin1")

  # separate by ","
  sx <- strsplit(x, split = ";")

  # create data frame
  xm <- data.frame(matrix(unlist(sx), nrow = length(sx), byrow = T))
  names(xm) <- xm[1,]
  xm <- xm[-1,]
  len <- nrow(xm)

  # get geographic coordinates
  lat <- as.numeric(unique(xm$Latitude))
  lon <- as.numeric(unique(xm$Longitude))

  # data regularization in new data.frame
  pluvio <- data.frame(cbind(Ano=xm$Anos,Mes=xm$Meses,Precip=xm$Total))
  # pluvio <- pluvio %>% mutate(AnoMes=as.Date(paste0(Ano,"-",Mes,"-1")), .before = Precip)

  # converts to longitudinal data format
  pluvio <- tidyr::gather(xm[,-c(1,2,3,4,7)], dia, precip, Dia1:Dia31)
  names(pluvio) <- c("Ano", "Mes", "Dia", "Precip")
  pluvio <- pluvio %>% mutate(Dia = as.numeric(substr(Dia, 4, 5)))
  pluvio <- pluvio %>% mutate(Mes = as.numeric(Mes))
  pluvio <- pluvio %>% mutate(Ano = as.numeric(Ano))
  pluvio <- pluvio %>% arrange(Ano,Mes)
  pluvio <- pluvio %>% mutate(Data = paste0(sprintf("%02d", as.numeric(Dia)),"/",sprintf("%02d", as.numeric(Mes)),"/",Ano), .before = Ano)
  pluvio <- pluvio %>% mutate(Data = as.Date(Data, "%d/%m/%Y"))
  pluvio <- pluvio[!(pluvio$Precip == "888.0"),]
  pluvio$Precip[pluvio$Precip == "999.0"] <- NA
  pluvio <- pluvio %>% mutate(Precip = as.numeric(Precip))

  # dti <- paste0(xm$Anos[1],"/",xm$Meses[1],"/1")
  # dtf <- paste0(xm$Anos[len],"/",xm$Meses[len],"/1")
  # df <- data.frame(Data = seq(as.Date(dti), as.Date(dtf), by = "month"))
  # df <- df %>% mutate(Ano = as.numeric(format(Data, "%Y")), Mes = as.numeric(format(Data, "%m"))); head(df)
  # df <- suppressMessages(left_join(df, pluvio, by = c("Data"="AnoMes")))
  # df <- df[,-c(1,4,5)]
  # names(df) <- c("Ano", "Mes", "Precip")

  return(list(df = pluvio, latitude = lat, longitude = lon))
}

#' Function for time series MinMax normalization
#'
#' @param x: array or ts object to be normalized
#' @param inf: lower boundary of normalization range
#' @param sup: upper boundary of normalization range
#'
#' @return normalized time series
norm_minmax <- function(x, inf, sup) {
  return(((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) * (sup - inf) + inf)
}
norm_standard <- function(x) {
  return((x-mean(x))/sd(x))
}

#' Function for time series MinMax de-normalization
#'
#' @param x: array or ts object to be de-normalized
#' @param src: original data before previous normalization (array or ts object)
#' @param inf: lower boundary of normalization range
#' @param sup: upper boundary of normalization range
#'
#' @return de-normalized time series
denorm_minmax = function(x, src, inf, sup) {
  return(min(src, na.rm = TRUE) + (max(src, na.rm = TRUE) - min(src, na.rm = TRUE)) * ((x - inf)/(sup - inf)))
}
denorm_standard <- function(x, src) {
  return(x*sd(src) + mean(src))
}
denorm_diff <- function(x, src, diff) {
  return(x+src[diff:length(src)-diff])
}
denorm_diff2 <- function(x, src, diff) {
  res <- c()
  res[1] <- src[1]
  for(i in 1:(length(src)-1)) {
    res[i+1] <- sum(c(src[diff], x[diff:i]))
  }
  return(res)
}

#' Function for lagged time series matrix
#'
#' @param x: array ou ts object
#' @param lag: number of lag values e.g. y_{t-lag}
#'
#' @return lag matrix (data.frame)
lag_matrix = function(x, lag) {
  ps <- tsp(x)
  yt <- x

  mt <- cbind(yt)

  for (i in 1:lag) {
    mt <- cbind(mt, stats::lag(yt, -i))
  }
  colnames(mt) <- c("yt", paste0("yt_",c(1:lag)))

  df <- data.frame(mt[-seq(length(x)+1, length.out = lag),])
  return(df)
}

#' Funcao para construir matriz indicadora de falhas para
#' serie temporal multivariada (valor observado = 1, falha = 0).
#'
#' @param data: um data.frame contendo uma serie temporal multivariada
#' @param plot: se TRUE, plota o mapa de calor das falhas no conjunto
#' @param tck.space: intervalo entre as marcas no rótulo do eixo horizontal
#' @param cex.plot: tamanho do texto
#' @param legend.x: posição da legenda
#' @param legend.y: posição da legenda
#' @param ...: argumentos adicionais passados para a funcao heatmap
#'
#' @return uma matriz
na_matrix <- function(data, plot = TRUE, tck.space = 1, cex.plot = 1,
                      legend.x = 1, legend.y = 1, ...) {
  col <- ncol(data)#length(data[1,])
  row <- nrow(data)#length(data[,1])
  na_matrix = matrix(ncol = col, nrow = row, dimnames = list(rownames(data),colnames(data)))

  for (j in 1:col) {
    for (i in 1:row) {
      if (!is.na(data[i,j])) {
        na_matrix[i,j] = 1 # obs
      } else {
        na_matrix[i,j] = 0 # mis
      }
    }
  }

  if(plot) {
    # set par
    fontes <- cex.plot
    par(xpd = TRUE, lwd = fontes, mar = c(1,4,0,1) + 0.1)

    # set labels
    labs.col <- rep("", row)
    tck.col <- seq(from = 1, to = row, by = tck.space)
    labs.col[tck.col] <- tck.col

    perc.row <- 100*apply(data, MARGIN = 2, na_perc.default)
    labs.row <- paste0(colnames(data), " (", sprintf("%0.1f", perc.row), "%)")
    cat("# Missing value statistics in data set\n",
      paste0("Mean: ", sprintf("%0.1f", mean(perc.row)), "%"), "\n",
      paste0("Min: ", sprintf("%0.1f", min(perc.row)), "% (", colnames(data)[which.min(perc.row)], ")"), "\n",
      paste0("Max: ", sprintf("%0.1f", max(perc.row)), "% (", colnames(data)[which.max(perc.row)], ")"), "\n")

    # plot
    heatmap(t(na_matrix), Colv = NA, Rowv = NA, scale="none",
            col = c("black","gray80"),
            labCol = labs.col, labRow = labs.row,
            cexRow = fontes, cexCol = fontes, cex.lab = fontes,
            margins = c(3,3), xlab = "", ylab = "", ...)

    # legend
    legend(x = legend.x, y = legend.y, legend = c("Ausente","Observado"),
           cex = fontes, fill = c("black","gray80"),
           bty = "n", horiz = F)

  }

  return(na_matrix)
}

na_stats <- function(x) {
  require("imputeTS")

  sumx <- imputeTS::statsNA(x, print_only = FALSE)
  aux <- cbind(
    sumx$df_distribution_na_gaps[sumx$df_distribution_na_gaps>0],
    which(sumx$df_distribution_na_gaps>0))

  nna <- sum(is.na(x))
  interv <- split(x, ceiling(seq_along(x)/(length(x)/4)))
  # interv <- seq(1, length(x), round(length(x)/4))

  return(cbind(
    "Length"=sumx$length_series,
    "Missing data"=round(100*sumx$number_NAs/sumx$length_series,1),
    "Gaps"=sumx$number_na_gaps,
    "Longest gap"=sumx$longest_na_gap,
    "%1stQtle"=round(100*sum(is.na(interv$`1`))/nna,1),
    "%2ndQtle"=round(100*sum(is.na(interv$`2`))/nna,1),
    "%3rdQtle"=round(100*sum(is.na(interv$`3`))/nna,1),
    "%4thQtle"=round(100*sum(is.na(interv$`4`))/nna,1),
    # "%1stQtle"=round(100*sum(is.na(x[interv[1]:interv[2]]))/nna,2),
    # "%2ndQtle"=round(100*sum(is.na(x[(interv[2]+1):interv[3]]))/nna,2),
    # "%3rdQtle"=round(100*sum(is.na(x[(interv[3]+1):interv[4]]))/nna,2),
    # "%4thQtle"=round(100*sum(is.na(x[(interv[4]+1):interv[5]]))/nna,2),
    "%gaps<=10"=round(100*sum(aux[,1][aux[,2]<=10])/sumx$number_na_gaps,1),
    # "10<%gaps<=30"=round(100*sum(aux[,1][aux[,2]>10 & aux[,2]<=30])/sumx$number_na_gaps,1),
    "10<%gaps<=100"=round(100*sum(aux[,1][aux[,2]>10 & aux[,2]<=100])/sumx$number_na_gaps,1),
    "%gaps>100"=round(100*sum(aux[,1][aux[,2]>100])/sumx$number_na_gaps,1)))
}

#' Function for cubic spline regression with equally spaced knots
#'
#' @param x: time series
#' @param plot: show decomposition plot
#'
#' @return Time series and model
csr <- function(x, plot = TRUE) {

  # x <- window(tsAirgapComplete, start = c(1949,1), end=c(1958,1))
  # x <- wineind
  n <- length(x)
  f <- round(frequency(x))
  info <- tsp(x)

  # Check if time series is seasonal
  if (f <= 1) {
    stop("Time series is not seasonal")
  }

  # auxiliary predictors
  t <- 1:n
  aux <- list()
  aux[["x"]] <- x
  aux[["t"]] <- t
  aux[["t.sqr"]] <- t^2
  aux[["t.cub"]] <- t^3

  # knot number
  # k-1 knots, where k = int(log(length/frequency))
  period <- n%/%f
  k <- round(log(period))

  # knot position
  knots <- round(seq(1,n,by = n/k))

  # regressors
  for(i in 2:k) {
    ki <- knots[[i]]
    aux[[paste0("t",ki)]] <- (t-ki)^3
    aux[[paste0("t",ki)]][aux[[paste0("t",ki)]]<0] <- 0
  }
  df <- as.data.frame(aux)

  # model fitting
  fit <- lm(x~., data = df)
  # print(summary(fit))

  # interpolation
  pred <- predict(fit, newdata = df)

  if (plot) {
    plot(t, x, type = "l")
    lines(t, pred, col="blue", lwd=2)

    for(i in 2:k) {
      abline(v = knots[i], lty = 2)
    }
  }

  tsp(pred) <- info
  return(list("fitted" = pred, "mod" = fit, "data" = df))
}

#' Function for a seasonal and trend time series model
#' with trend estimated by cubic spline regression
#' with equally spaced knots and dummy variables for seasons
#'
#' @param x: time series
#' @param plot: show decomposition plot
#'
#' @return Time series and model
na_tsr <- function(x) {
  # x <- -tsAirgap
  k <- frequency(x)
  n <- length(x)
  t <- 1:n
  info <- tsp(x)

  missindx <- is.na(x)
  na.index <- which(is.na(x))

  df.loess <- data.frame(cbind(x,t))
  loess.mod <- loess(x~t, data = df.loess)
  trend <- predict(loess.mod, newdata = df.loess)
  # csr.mod <- csr(x, plot = F)
  # trend <- csr.mod$fitted

  # seasonal dummy variable
  season_dummy <- forecast::seasonaldummy(x)

  # recharge
  chunks <- split(t, sort(t%%k))
  recharge <- rep(1, n)
  for (i in 1:length(chunks)) {
    indices <- chunks[[i]]
    ts_temp <- stats::ts(x[indices])

    diff.r <- as.numeric(diff(ts_temp))
    diff.r <- diff.r[!is.na(diff.r)]
    recharge[indices] <- rep(sum(diff.r[diff.r>0]), k)
    # recharge[indices] <- rep(diff(range(ts_temp, na.rm = TRUE)), k)

    # min.r <- +Inf; min.j <- 1
    # max.j <- which.max(ts_temp); max.r <- ts_temp[max.j]
    # for (j in 1:max.j) {
    #   if (!is.na(ts_temp[j]) && ts_temp[j] < min.r) {
    #     min.r <- ts_temp[j]
    #     min.j <- j
    #   }
    # }
    # recharge2[indices] <- rep((max.r-min.r), k)
  }

  # regressors
  df <- data.frame(cbind(x, trend, season_dummy*recharge))

  # train with complete case
  df.na <- na.omit(df)
  df.pred <- df[na.index, ]

  # model fitting
  fit <- lm(x~., data = df.na)

  # prediction
  pred <- predict(fit, newdata = df.pred)

  # imputation
  imp <- x
  imp[na.index] <- pred

  return(imp)
}
#' Missing data imputation of Groundwater Level time series
#' based on a Trend and Seasonal time series model
#' taking into consideration the system Recharge.
#' The trend component is estimated by LOESS regression
#' and the seasonal dummy variables are weighted
#' according to the total groundwater recharge
#'
#' @param x: time series with missing values
#' @param ...: additional parameters for LOESS regression
#'
#' @return complete time series with imputed values
na_gwltsr <- function(x, s = rep(1, length(x)), ...) {
  # x <- -tsAirgap
  # x <- gg[[1]]
  # x <- ls.gwl$P2300022135

  # mod <- StructTS(x, type = "trend")
  # mod.fit <- tsSmooth(mod)

  k <- frequency(x)
  n <- length(x)
  t <- 1:n
  info <- tsp(x)

  missindx <- is.na(x)
  na.index <- which(is.na(x))

  # data.frame for LOESS regression
  df.loess <- data.frame(cbind(x,t))

  # sum of squared error of LOESS residuals
  SSE <- function(y){
    loess.mod <- loess(x ~ t, data = df.loess, span = y)
    res <- loess.mod$residuals
    sse <- sum(res^2)
    return(sse)
  }

  # optimize "span" parameter by minimizing SSE
  span.min <- sum(missindx)/n+0.2
  opt.span <- optimize(SSE, lower = span.min, upper = 1)

  # optimized LOESS
  loess.mod <- loess(x~t, data = df.loess, span = opt.span$minimum)
  # loess.mod <- loess(x~t, data = df.loess, span = 0.75)
  trend <- predict(loess.mod, newdata = df.loess)

  # plot(as.numeric(x),x=t,"l")
  # lines(trend, x=t, col="green")

  # remove trend
  # no_trend <- x - trend

  # seasonal dummy variable
  season_dummy <- forecast::seasonaldummy(x)

  # Wet and Dry seasons
  # W <- c(1:5,12); D <- c(6:11)
  # season_year <- stats::cycle(x)
  # season_year[season_year %in% W] <- "W"
  # season_year[season_year %in% D] <- "D"
  # season_year <- as.factor(season_year)

  # recharge
  season <- floor(time(x))
  chunks <- split(t,season)
  recharge <- rep(0, n)
  # x_temp <- imputeTS::na_kalman(x)
  x_temp <- x
  plot(x_temp)
  for (i in 1:length(chunks)) {
    indices <- chunks[[i]]
    ts_temp <- stats::ts(x_temp[indices])

    diff.r <- as.numeric(diff(ts_temp))
    diff.r <- diff.r[!is.na(diff.r)]
    ss <- ifelse(sum(diff.r[diff.r>0]) != 0, sum(diff.r[diff.r>0]), 0)
    recharge[indices] <- rep(ss, length(indices))
  }

  # Explanatory variables
  # df <- data.frame(cbind(x, trend, recharge*s*season_dummy))
  df <- data.frame(cbind(x, trend, recharge*s, season_dummy))
  # df <- data.frame(cbind(x, trend, recharge*season_dummy))

  # train with complete case
  df.na <- na.omit(df)
  df.pred <- df[na.index, ]

  # model fitting
  fit <- lm(x~., data = df.na)

  # prediction
  pred <- predict(fit, newdata = df.pred)

  # imputation
  imp <- x
  imp[na.index] <- pred
  # plot(imp)
  # plot(fit$fitted.values)
  # rmse(imp[na.index],-tsAirgapComplete[na.index])
  # rmse(na_seadec(x)[na.index],-tsAirgapComplete[na.index])

  return(imp)
}

na_cstsr <- function(x) {
  x <- tsAirgap

  cstsr_mod <- cstsrm(x, plot = FALSE)
  df <- cstsr_mod$data



}

#' Function for user-defined decomposition
#'
#' Estimate trend using cubic spline regression
#'
#' Supports additive and multiplicative decomposition
#'
#' @param data: time series
#' @param type: decomposition type c("additive","multiplicative")
#' @param plot: show decomposition plot
#'
#' @return Time series
decomposition <- function(data, type = "additive", plot = TRUE) {

  n <- length(data)
  f <- frequency(data)
  periods <- n%/%f
  id <- seq(1, n, by = f) - 1

  # trend series estimated by cubic spline regression
  trend <- csr(data)$fitted

  # compute seasonal and remainder components
  # seasonal series estimated by averaging or computing median of
  # the de-trended values for each period
  if (type == "additive") {

    ## additive decomposition
    detrend <- data - trend

    m_per <- numeric(f)
    for (i in 1:f) {
      #m_per[i] <- mean(detrend[id + i], na.rm = TRUE)
      m_per[i] <- quantile(detrend[id + i], probs = 0.5, na.rm = TRUE)
    }

    seasonal <- ts(rep(m_per, periods + 1)[seq(n)],
                   start = start(detrend),
                   frequency = f)

    remainder <- data - trend - seasonal

  } else if (type == "multiplicative") {

    ## multiplicative decomposition
    detrend <- data / trend

    m_per <- numeric(f)
    for (i in 1:f) {
      #m_per[i] <- mean(detrend[id + i], na.rm = TRUE)
      m_per[i] <- quantile(detrend[id + i], probs = 0.5, na.rm = TRUE)
    }

    seasonal <- ts(rep(m_per, periods + 1)[seq(n)],
                   start = start(detrend),
                   frequency = f)

    remainder <- data / (trend * seasonal)
  }

  # plot
  if(plot) {
    plot(cbind(data, seasonal, trend, remainder),
         main = "",
         yax.flip = TRUE)
  }

  df <- data.frame("seasonal" = seasonal, "trend" = trend, "remainder" = remainder)
  return(df)
}

#' Function for univariate time series imputation with
#' linear interpolation on seasonally adjusted series decomposed
#' with cubic spline regression trend
#'
#' Estimate trend with cubic spline regression
#'
#' Imputate missing values on seasonally adusted series
#'
#' @param data: time series
#' @param type: decomposition type c("additive","multiplicative")
#' @param ...: Further graphical parameters
#'
#' @return Time series
na_lisacsr <- function(data, type = "additive", ...) {

  n <- length(data)
  t <- 1:n
  na_id <- is.na(data)
  obs_id <- t[!na_id]
  info <- tsp(data)

  # decomposition with cubic spline regression trend
  dec <- decomposition(data, type, ...)

  # perform imputation on seasonally adjusted series
  # recompose imputed series
  if (type == "additive") {
    no_season <- dec$remainder + dec$trend
    pred <- approx(obs_id, no_season[obs_id], t, rule = 2)$y
    rec <- dec$seasonal + pred

  } else if (type == "multiplicative") {
    no_season <- dec$remainder * dec$trend
    pred <- approx(obs_id, no_season[obs_id], t, rule = 2)$y
    rec <- dec$seasonal * pred
  }

  # imputed values back to original data
  data[na_id] <- rec[na_id]

  tsp(data) <- info
  return(data)
}

#' Function for plotting imputation
#'
#' @param src: original time series with missing values
#' @param imputed: time series obtained from some imputation procedure
#' @param ...: Further graphical parameters
na_plot <- function(src, imputed, ...) {
  t <- c(1:length(src))
  plot(t, imputed, type = "l", col = "red", ...)
  lines(t, src, type = "l", col = "black")
}

#' Function for identifying opyimum number of cluster in k-mean method
#'
#'
optimal_cluster <- function(data, start = 1, end = nrow(data)-1, ...) {
#data <- tdf
#start <- 1
#end <- 15
  # initialize
  opt_cl <- kmeans(data, start)
  min_tss <- opt_cl$betweenss/opt_cl$totss
  #min_tss <- opt_cl$tot.withinss
  trace <- c()
  trace[1] <- min_tss

  # search for different k values
  for (i in (start+1):(end)) {

    cl <- kmeans(data, i)
    trace[i] <- cl$betweenss/cl$totss
    print(trace[i])

    if (trace[i] > min_tss) {
      opt_cl <- cl
      min_tss <- trace[i]
    }
  }

  # plot
  plot(c(start:(end)),trace, type = "b", xlab="Number of clusters k", ylab="Total Within Sum of Square")

  return(opt_cl)
}


#' Function for polynomial interpolation
#'
#' @param data: um data.frame contendo uma série temporal multivariada
#' @param plot: se TRUE, plota o mapa de calor das falhas no conjunto
#' @param ...: argumentos adicionais passados para a funcao heatmap
#'
#' @return uma matriz
poly_fit <- function(x, n = 3, spline = FALSE, plot = TRUE) {
  q <- seq(from = 1, to = length(x), by = 1)

  if(spline) {
    n <- NULL
    pred <- stats::spline(x, n = length(x)/10, xout = q)$y
  } else {
    model <- lm(x ~ poly(q, n))
    pred <- predict(model, data.frame(x = q))
  }

  if(plot) {
    plot(q, x, col = "deepskyblue4", xlab = 'Tempo', ylab = 'Serie')
    lines(q, pred, col = "green", lwd = 3)
  }

  return(pred)
}


# Teste SVR imputation ----
na_svr_multivariate <- function(x) {
  #tx <- t(x)
  #dtx <- dist(tx)
  #clx <- hclust(dtx)
  #plot(clx)

  #clt <- cutree(clx, k = 2)

  #cl1 <- x %>% select(names(clt[clt==1]))

  ccx <- cor(x, use = "complete.obs")
  View(ccx)

  col <- names(sort(ccx[,1], decreasing = TRUE)[1:5])
  clt <- x %>% select(col)

  #x <- as.data.frame(ts_estudo)

  na_id <- complete.cases(clt)

  x_comp <- x[!na_id,]
  x_miss <- x[na_id,]

}

# Groundwater ----

#' Monta uma tabela com estat?sticas sobre dados ausentes no conjunto de dados
#'
#' @param dados: objeto do tipo lista contendo todas as s?ries temporais
#' @return stNAPoco: objeto do tipo matriz
#'
estatistica_NA = function(dados) {
  # usa pacote imputeTS
  library("imputeTS")

  # estat?sticas sobre as s?ries e os valores ausentes
  stNAPoco = c(rep(NA,length(dados)))
  stNAPoco = matrix(ncol = 3,
                nrow = length(dados),
                dimnames = list(colnames(dados), c("Comprimento","NAs","% NAs")))

  for (i in 1:length(dados)) {
    stNA = imputeTS::statsNA(ts(dados[[i]]), print_only = FALSE)
    stNAPoco[i,] = c(stNA$length_series, stNA$number_NAs, stNA$percentage_NAs)
    stNA = NULL
  }

  return(stNAPoco)
}

#' Faz imputa??o de dados ausentes com SVR interpola??o
#' e plota grafico com visualizacao
#'
#' @param serie: objeto do tipo ts a ser imputado
#' @return stNAPoco: objeto do tipo matriz
#'
na_plot_svr = function(serie) {
  # usa pacote e1071
  #library("e1071")

  # recebe a serie
  # monta array t
  # monta data.frame t x yt
  # monta data.frame sem valores NA
  yt = serie
  t = c(1:length(yt))
  df_na = data.frame(cbind(t,yt))
  df = na.omit(df_na)

  # constroi modelo SVR
  svrmod = svm(yt ~ t,
               df,
               kernel = "radial",
               degree = 3,
               coef0 = 0,
               cost = 1)
  # faz previsao
  # usa valores de t :: yt = f(t)
  # calcula o valor correpondente aos NA
  prev = predict(svrmod, t)

  # monta data.frame final contendo:
  # t = tempo
  # yt = serie original
  # yt_prev = serie original com imputa??o de NAs
  # prev = previsao do modelo SVR
  # yt_na = valores imputados
  # yt_bin = binario indicando onde houve imputa??o
  # yt_lin - linhas verticais indicando distribui??o de NAs
  yt_prev = yt
  yt_na = rep(NA, length = length(yt))
  yt_bin = rep(0, length = length(yt))
  yt_lin = rep(NA, length = length(yt))
  for (i in 1:length(yt)) {
    if (is.na(yt[i])) {
      yt_prev[i] = prev[i]
      yt_na[i] = prev[i]
      yt_bin[i] = 1

      # linhas verticais
      # distribui??o NAs
      if (i>1 && !is.na(yt[i-1])) {
        yt_lin[i] = i
      }
    } else {
      if (i>1 && is.na(yt[i-1])) {
        yt_lin[i-1] = i-1
      }
    }
  }
  df_prev = data.frame(cbind(t,yt,yt_prev,prev,yt_na,yt_bin,yt_lin))

  View(df_prev)
  # plota
  par(cex.axis = 0.8,
      cex.lab = 1,
      font.lab=2)
  plot(df_prev$yt_prev,
       type = "o",
       pch = 20,
       cex = 0.7,
       col = rgb(df_prev$yt_bin,0,0),
       xlab = "Tempo",
       ylab = "S?rie")
  abline(v=df_prev$yt_lin, col = "gray83", lwd = 1)
  legend("topleft", legend=c("S?rie original", "Pontos inseridos via SVR", "Distribui??o de dados NA"),
        col=c("black", "red", "gray83"), lty = 1, lwd = c(2,2,2), bty = "white", cex = 0.8)

  return(df_prev)
}

#' Faz imputa??o de dados ausentes com SVR autorregressivo
#' e plota graficos com visualizacao
#'
#' @param serie: objeto do tipo ts a ser imputado
#' @param termos_ar: indice do valor passado (lagged) ex.: y_{t-index}
#' @return stNAPoco: objeto do tipo matriz
#'
na_plot_svr_ar = function(serie, termos_ar) {
  # usa pacote e1071
  #library("e1071")

  # recebe a serie
  # monta matriz autorregressiva
  yt = serie
  df_na = lag_matrix(yt, termos_ar)
  df = na.omit(df_na)


  # constroi modelo SVR
  svrmod = svm(yt ~ .,
               df,
               kernel = "radial",
               degree = 3,
               coef0 = 0,
               cost = 1)

  # monta data.frame final cotendo:
  # t = tempo
  # yt = serie original
  # yt_prev = serie original com imputa??o de NAs
  # yt_na = valores imputados
  # yt_bin = binario indicando onde houve imputa??o
  # yt_lin - linhas verticais indicando distribui??o de NAs
  yt_prev = df_na$yt
  yt_na = rep(NA, length = length(df_na$yt))
  yt_bin = rep(0, length = length(df_na$yt))
  yt_lin = rep(NA, length = length(df_na$yt))

  # data.frame auxiliar com valores autorregressivos
  # como entradas no modelo para calculo do valor a ser imputado
  df_aux = data.frame(matrix(nrow = 1, ncol = termos_ar)); names(df_aux) = names(df)[-length(df)]

  for (i in 1:length(df_na$yt)) {
    if (is.na(df_na$yt[i])) {
      df_aux[1,] = try(yt_prev[(i-1):(i-termos_ar)])
      yt_prev[i] = predict(svrmod, df_aux)
      #yt_prev[i] = prev[i]

      yt_na[i] = yt_prev[i]
      #yt_na[i] = prev[i]
      yt_bin[i] = 1

      # linhas verticais
      # distribui??o NAs
      if (!is.na(df_na$yt[i-1])) {
        yt_lin[i] = i
      }
    } else {
      if (i>1 && is.na(df_na$yt[i-1])) {
        yt_lin[i-1] = i-1
      }
    }
  }
  df_prev = data.frame(cbind(yt,
                             yt_prev = c(yt[1:termos_ar], yt_prev),
                             yt_na = c(rep(NA, termos_ar), yt_na),
                             yt_bin = c(rep(0, termos_ar), yt_bin),
                             yt_lin = c(rep(NA, termos_ar), (yt_lin+termos_ar))))

  View(df_prev)
  # plota
  par(cex.axis = 0.8,
      cex.lab = 1,
      font.lab=2)
  plot(df_prev$yt_prev,
       type = "o",
       pch = 20,
       cex = 0.7,
       col = rgb(df_prev$yt_bin,0,0),
       xlab = "Tempo",
       ylab = "S?rie")
  abline(v=df_prev$yt_lin, col = "gray83", lwd = 1)
  legend("topleft", legend=c("S?rie original", "Pontos inseridos via SVR", "Distribui??o de dados NA"),
         col=c("black", "red", "gray83"), lty = 1, lwd = c(2,2,2), bty = "white", cex = 0.8)

  return(df_prev)
}

#' Faz imputa??o de dados ausentes com SVR
#'
#' @param data_frame: conjunto de dados
#' @param indep_var: string com o nome da variavel independente do dataframe
#' @return stNAPoco: objeto do tipo matriz
#'
na_svr = function(data_frame, indep_var) {
  df_na = data.frame(data_frame)
  df = na.omit(df_na)

  # constroi modelo SVR
  svrmod = svm(as.formula(paste(indep_var,"~.", sep="")),
               df,
               kernel = "radial",
               degree = 3,
               coef0 = 0,
               cost = 1)

  # data.frame auxiliar
  df_aux = data.frame(matrix(nrow = 1, ncol = (ncol(data_frame)-1)));
  names(df_aux) = names(df)[colnames(df)!=indep_var]

  # serie original para receber imputa??o
  prev = df_na[[indep_var]]

  for (i in 1:length(df_na[[indep_var]])) {
    if (is.na(df_na[[indep_var]][i])) {
      tryCatch(
        expr = {
          df_aux[1,] = try(df_na[i,][colnames(df)!=indep_var])
          prev[i] = predict(svrmod, df_aux)
        },
        error = function(e){
          prev[i] = NA
        },
        warning = function(w){},
        finally = {}
      )
    }
  }

  return(as.data.frame(cbind(df_na, Imputation = prev)))
}

# Forecasting ----

#' Time series forecasting via SVR
#' optimised gamma, cost and epsilon of RBF kernel via GA
#'
#' @param series ts object to be forecasted
#' @param ... aditional parameters to be passed for GA optimisation
#' @return a list with the forecasted series and optimised SVR parameters
#'
svr.optim <- function(series, fitness = "ga.fit.svr.ar.radial", v.size = 0.2, cv.type = "most recent", ...) {

  yt = series

  # optimises SVR model
  ga = ga.optimisation.svrar(fit.fun = get(fitness), series = yt, v.size = v.size, cv.type = cv.type, ...)

  ar.terms = round(ga@solution[4])
  print(ga@solution)
  print(ga@fitnessValue)

  # rebuild the model

  # autorregressive matrix
  df = lag_matrix(yt, ar.terms)

  # cross validation
  training = cross.validation(df, t.size, v.size, cv.type)

  # data sets
  df.train = training$treino # treino + validation
  df.training = training$treinamento # treino sem valida??o
  df.validation = training$valida # valida??o
  #df.test = training$teste # test (unused on imputation simulation)

  # training without validation
  svrmod = svm(formula = yt ~ .,
               data = df.training,
               type = "eps-regression",
               kernel = "radial",
               gamma = ga@solution[1],
               cost = ga@solution[2],
               epsilon = ga@solution[3])

  # forecast
  prev = predict(svrmod, df)

  return(list(series = prev, ga.solution = ga@solution))
}

#' @title SVR Time Index Based Seasonally Decomposed GA Optimised Missing Value Imputation
#'
#' @description Removes the seasonal component from the time series,
#' performs imputation on the deseasonalized series by Support Vector
#' Regression over the time index with Radial kernel optimised by
#' Genetic Algorithm and afterwards adds the seasonal component again.
#'
#' @param data ts object to be imputed
#' @param ... aditional parameters to be passed for GA optimisation
#'
#' @return List with the imputed series and optimised SVR parameters
#'
#' @author Rubens Cunha
na.svrt.seadec.optim = function(data, fitness = "ga.fit.svr.radial", ...) {

  missindx <- is.na(data)

  ##
  ## 2. Imputation Code
  ##

  # Interpolate NAs, to get complete series, because findFRequency and later on stl does not work with NAs
  temp <- na_interpolation(data)

  # temp (see above) is a interpolated version of data since stl does not work with NAs
  stl <- stats::stl(temp, robust = TRUE, s.window = "periodic")#s.window = 11)
  # just take trend component + irregular  (remove seasonality)
  ts_no_seasonality <- stl$time.series[, 2] + stl$time.series[, 3]

  # Fill in NAs again
  ts_no_seasonality[missindx] <- NA

  # Perform imputation on data without seasonality
  ts_no_seasonalityimputed <- na.svrt.optim(ts_no_seasonality, fitness, ...)

  # add seasonality
  ts_imputed <- ts_no_seasonalityimputed$series + stl$time.series[, 1]

  # Merge imputed values back into original time series
  data[missindx] <- ts_imputed[missindx]

  return(list(series = data, ga.solution = ts_no_seasonalityimputed$ga.solution))
}

# Imputation ----

#' Time series imputation via SVR on time index
#' optimised gamma, cost and epsilon of RBF kernel via GA
#'
#' @param series ts object to be imputed
#' @param ... aditional parameters to be passed for GA optimisation
#' @return a list with the imputed series and optimised SVR parameters
#'
na.svrt.optim <- function(series, fitness = "ga.fit.svr.radial", ...) {

  miss.index = is.na(series)

  # get series
  # build array t
  # build data.frame t x yt
  # build data.frame without NA
  yt = series
  t = c(1:length(yt))
  df_na = data.frame(cbind(t,yt))
  df = na.omit(df_na)

  # optimises SVR model
  ga = ga.optimisation.svr(fit.fun = get(fitness), data = df, ...)
  print(ga@solution)

  if (fitness == "ga.fit.svr.radial") {
    svrmod = svm(yt ~ t, df,
                  type = "eps-regression",
                  kernel = "radial",
                  gamma = ga@solution[1],
                  cost = ga@solution[2],
                  epsilon = ga@solution[3])
  } else if (fitness == "ga.fit.svr.sigmoid") {
    svrmod = svm(yt ~ t, df,
                 type = "eps-regression",
                 kernel = "sigmoid",
                 gamma = ga@solution[1],
                 cost = ga@solution[2],
                 epsilon = ga@solution[3],
                 coef0 = ga@solution[4])
  } else if (fitness == "ga.fit.svr.polynomial") {
    svrmod = svm(yt ~ t, df,
                 type = "eps-regression",
                 kernel = "polynomial",
                 gamma = ga@solution[1],
                 cost = ga@solution[2],
                 epsilon = ga@solution[3],
                 coef0 = ga@solution[4],
                 degree = ga@solution[5])
  }

  # predict, imputs
  # use values t :: yt = f(t)
  # compute the NA corresponding values
  prev = predict(svrmod, t[miss.index])

  # series with imputed values
  yt_prev = yt
  yt_prev[miss.index] = prev[miss.index]

  return(list(series = yt_prev, ga.solution = ga@solution))
}

#' @title SVR Time Index Based Seasonally Decomposed GA Optimised Missing Value Imputation
#'
#' @description Removes the seasonal component from the time series,
#' performs imputation on the deseasonalized series by Support Vector
#' Regression over the time index with Radial kernel optimised by
#' Genetic Algorithm and afterwards adds the seasonal component again.
#'
#' @param data ts object to be imputed
#' @param ... aditional parameters to be passed for GA optimisation
#'
#' @return List with the imputed series and optimised SVR parameters
#'
#' @author Rubens Cunha
na.svrt.seadec.optim = function(data, fitness = "ga.fit.svr.radial", ...) {

  missindx <- is.na(data)

  ##
  ## 2. Imputation Code
  ##

  # Interpolate NAs, to get complete series, because findFRequency and later on stl does not work with NAs
  temp <- na_interpolation(data)

  # temp (see above) is a interpolated version of data since stl does not work with NAs
  stl <- stats::stl(temp, robust = TRUE, s.window = "periodic")#s.window = 11)
  # just take trend component + irregular  (remove seasonality)
  ts_no_seasonality <- stl$time.series[, 2] + stl$time.series[, 3]

  # Fill in NAs again
  ts_no_seasonality[missindx] <- NA

  # Perform imputation on data without seasonality
  ts_no_seasonalityimputed <- na.svrt.optim(ts_no_seasonality, fitness, ...)

  # add seasonality
  ts_imputed <- ts_no_seasonalityimputed$series + stl$time.series[, 1]

  # Merge imputed values back into original time series
  data[missindx] <- ts_imputed[missindx]

  return(list(series = data, ga.solution = ts_no_seasonalityimputed$ga.solution))
}


#' Time series imputation via SVR on autorregressive terms
#' optimised gamma, cost and epsilon of RBF kernel via GA
#'
#' @param serie: ts object to be imputed
#' @param ar.terms: order of ar model (lagged values) e.g.: y_{t-index}
#' @param ...: aditional parameters to be passed for GA optimisation
#' @return a list with the imputed series and optimised SVR parameters
#'
na.svrar.optim = function(series, ar.terms, fitness = "ga.fit.svr.radial", ...) {

  #series = tsAirgap
  #ar.terms = 1
  #t.size = .8
  #v.size = .25

  miss.index = is.na(series)

  # cross validation
  #lim.imp = ar.terms + 2
  #na.series = series[!miss.index]
  #imput.interval = na.series[lim.imp:(length(na.series)-lim.imp+1)]

  #train.rate = t.size
  #train.size = trunc(train.rate * length(imput.interval))
  #train.smp = sort(sample(length(imput.interval), train.size))
  #train.inv = c(1:length(imput.interval))[-train.smp]

  # training + validation
  #train.interval = imput.interval[train.smp]

  #val.rate = v.size
  #val.size = trunc(val.rate * length(train.interval))
  #val.smp = sort(sample(length(train.interval), val.size))
  #val.inv = c(1:length(train.interval))[-val.smp]

  # validation set
  #validation.data = train.interval[val.smp]

  # training set
  #training.data = train.interval[val.inv]

  # test set
  #test.data = imput.interval[train.inv]

  # series for forecast and backcast
  forecast = series
  backcast = rev(series)
  data = c("forecast", "backcast")
  imputed = list(values = c(), best.solution = c(),
                 ga.solution.forecast = c(), ga.solution.backcast = c(),
                 ga.fit.value.forecast = "", ga.fit.value.backcast = c())

  #lag_matrix = list(forecast = "", backcast = c())
  #for (k in data) {
  #  print(k)
  #  yt = get(k)

  #  # build autorregressive matrix
  #  df.na = lag_matrix(yt, ar.terms)
  #  df = na.omit(df.na)

  #  lag_matrix[[k]] = df
  #}

  #aux.lag_matrix = data.frame(forecast = c(rep(NA,ar.terms), lag_matrix$forecast$yt), backcast = c(rev(lag_matrix$backcast$yt),rep(NA,ar.terms)))
  #test.data = na.omit(aux.lag_matrix$forecast==aux.lag_matrix$backcast)

  # intersection
  #aux.test = data.frame(index = c(1:length(test.data)), test = test.data)
  #join.index = aux.test$index[aux.test$test == TRUE]
  #join.size = length(join.index)

  # training + validation
  #train.rate = t.size
  #train.size = trunc(train.rate * join.size)
  #train.interval = sort(sample(join.index, train.size))

  # test set
  #test.interval = join.index[-train.interval]

  #forecast.data = data.frame(index = c(test.data, rep(FALSE, ar.terms)), forecast = lag_matrix$forecast$yt)
  #forecast.test.data = forecast.data[test.interval, ]
  #forecast.test.data = lag_matrix$forecast[as.numeric(row.names(forecast.test.data)), ]

  #backcast.data = data.frame(index = c(rep(FALSE, ar.terms), test.data), backcast = rev(lag_matrix$backcast$yt))
  #backcast.test.data = backcast.data[(test.interval + ar.terms), ]
  #backcast.test.data = lag_matrix$backcast[as.numeric(row.names(backcast.test.data)), ]

  for (j in data) {
    print(j)
    yt = get(j)

    # build autorregressive matrix
    df.na = lag_matrix(yt, ar.terms)
    df = na.omit(df.na)
    #df = lag_matrix[[j]]

    # optimises SVR model
    ga = ga.optimisation.svr(fit.fun = get(fitness), data = df, ...)
    print(ga@solution)
    print(ga@fitnessValue)

    imputed[[paste("ga.solution.",j,sep="")]] = ga@solution
    imputed[[paste("ga.fit.value.",j,sep="")]] = ga@fitnessValue
  }

  # get the best imputation forecast or backcast
  optim.imput = ifelse(test = { (imputed$ga.fit.value.forecast >= imputed$ga.fit.value.backcast) },
                       yes = {"forecast"},
                       no = {"backcast"})
  optim.param = imputed[[paste("ga.solution.",j,sep="")]]
  print(optim.imput)

  # imputation
  yt = get(optim.imput)
  df.na = lag_matrix(yt, ar.terms)
  df = na.omit(df.na)

  svrmod = svm(yt ~ ., df,
               type = "eps-regression",
               kernel = "radial",
               gamma = optim.param[1],
               cost = optim.param[2],
               epsilon = optim.param[3])

  # auxiliary data.frame with autorregressive values
  df.aux = data.frame(matrix(nrow = 1, ncol = ar.terms)); names(df.aux) = names(df)[-length(df)]

  # original series to receive imputations
  prev = df.na$yt
  for (i in 1:length(df.na$yt)) {
    if (is.na(df.na$yt[i])) {
      tryCatch(
        expr = {
          df.aux[1,] = try(prev[(i-1):(i-ar.terms)])
          prev[i] = predict(svrmod, df.aux)
        },
        error = function(e){},
        warning = function(w){},
        finally = {})
    }
  }

  # result into a list
  imputed$values = c(yt[1:ar.terms], prev)
  imputed$best.solution = optim.param

  # imputed series
  series[miss.index] = imputed$values[miss.index]
  ga.solution = paste(round(as.numeric(imputed$ga.solution.forecast),3), collapse = ", ")

  #series[miss.index] = rowMeans(cbind(imputed$forecast[miss.index], rev(imputed$backcast[miss.index])))
  #ga.solution = paste(paste(round(as.numeric(imputed$ga.solution.forecast),3), collapse = ", "),
  #                    paste(round(as.numeric(imputed$ga.solution.backcast),3), collapse = ", "), collapse = " ")

  return(list(series = series, ga.solution = ga.solution))
}

na.svrar.test.optim = function(series, fitness = "ga.fit.svr.ar.radial", ...) {

  miss.index = is.na(series)

  # series for forecast and backcast
  forecast = series
  #backcast = rev(series)
  #data = c("forecast", "backcast")
  data = c("forecast")
  imputed = list(values = c(), best.solution = c(),
                 ga.solution.forecast = c(), ga.solution.backcast = c(),
                 ga.fit.value.forecast = "", ga.fit.value.backcast = "")

  for (j in data) {
    yt = get(j)

    # optimises SVR model
    ga = ga.optimisation.svrar(fit.fun = get(fitness), series = yt, ...)

    ar.terms = round(ga@solution[4])
    print(ga@solution)
    print(ga@fitnessValue)

    # build autorregressive matrix
    df.na = lag_matrix(yt, ar.terms)
    df = na.omit(df.na)

    svrmod = svm(yt ~ ., df,
                 type = "eps-regression",
                 kernel = "radial",
                 gamma = ga@solution[1],
                 cost = ga@solution[2],
                 epsilon = ga@solution[3])

    # auxiliary data.frame with autorregressive values
    df.aux = data.frame(matrix(nrow = 1, ncol = ar.terms)); names(df.aux) = names(df)[-length(df)]

    # original series to receive imputations
    prev = df.na$yt
    for (i in 1:length(df.na$yt)) {
      if (is.na(df.na$yt[i])) {
        tryCatch(
          expr = {
            df.aux[1,] = try(prev[(i-1):(i-ar.terms)])
            prev[i] = predict(svrmod, df.aux)
          },
          error = function(e){},
          warning = function(w){},
          finally = {})
      }
    }

    # result into a list
    imputed$values = c(yt[1:ar.terms], prev)

    # imputed series
    series[miss.index] = imputed$values[miss.index]
    ga.solution = paste(round(as.numeric(imputed$ga.solution.forecast),3), collapse = ", ")
  }

  return(list(series = series, ga.solution = ga.solution))
}

na.svrar.mixed.optim = function(series, ar.terms, fitness = "ga.fit.svr.radial", ...) {

  miss.index = is.na(series)

  # series for forecast and backcast
  forecast = series
  backcast = rev(series)
  data = c("forecast", "backcast")
  #data = c("forecast")
  imputed = list(forecast = c(), backcast = c(), ga.solution.forecast = "", ga.solution.backcast = "")

  for (j in data) {
    print(j)
    yt = get(j)

    # build optimized autorregressive matrix
    df.na = lag_matrix(yt, ar.terms)
    df = na.omit(df.na)

    # optimises SVR model
    ga = ga.optimisation.svr(fit.fun = get(fitness), data = df, ...)
    print(ga@solution)
    print(ga@fitnessValue)

    svrmod = svm(yt ~ ., df,
                 type = "eps-regression",
                 kernel = "radial",
                 gamma = ga@solution[1],
                 cost = ga@solution[2],
                 epsilon = ga@solution[3])

    # auxiliary data.frame with autorregressive values
    df.aux = data.frame(matrix(nrow = 1, ncol = ar.terms)); names(df.aux) = names(df)[-length(df)]

    # original series to receive imputations
    prev = df.na$yt
    for (i in 1:length(df.na$yt)) {
      if (is.na(df.na$yt[i])) {
        tryCatch(
          expr = {
            df.aux[1,] = try(prev[(i-1):(i-ar.terms)])
            prev[i] = predict(svrmod, df.aux)
          },
          error = function(e){},
          warning = function(w){},
          finally = {})
      }
    }

    # result into a list
    imputed[[j]] = c(yt[1:ar.terms], prev)
    imputed[[paste("ga.solution.",j,sep="")]] = ga@solution
  }

  miss.series = series[miss.index]
  miss.size = length(miss.series)

  if ((miss.size %% 2) == 0) {
    for (k in 1:(miss.size / 2)) {
      miss.series[k] = imputed$forecast[miss.index][k]
    }
    for (k in (miss.size / 2):miss.size) {
      miss.series[k] = rev(imputed$backcast[miss.index])[k]
    }
  } else {
    median = ceiling((miss.size / 2))
    for (k in 1:(median - 1)) {
      miss.series[k] = imputed$forecast[miss.index][k]
    }
    for (k in (median + 1):miss.size) {
      miss.series[k] = rev(imputed$backcast[miss.index])[k]
    }
    miss.series[median] = mean(imputed$forecast[miss.index][median],
                               rev(imputed$backcast[miss.index])[median])
  }

  #series[miss.index] = imputed$forecast[miss.index]
  series[miss.index] = miss.series
  ga.solution = paste(round(as.numeric(imputed$ga.solution.forecast),3), collapse = ", ")
  #series[miss.index] = rowMeans(cbind(imputed$forecast[miss.index], rev(imputed$backcast[miss.index])))
  #ga.solution = paste(paste(round(as.numeric(imputed$ga.solution.forecast),3), collapse = ", "),
  #                    paste(round(as.numeric(imputed$ga.solution.backcast),3), collapse = ", "), collapse = " ")

  return(list(series = series, ga.solution = ga.solution))
}

na.svrar.mean.optim = function(series, ar.terms, fitness = "ga.fit.svr.radial", ...) {

  miss.index = is.na(series)

  # series for forecast and backcast
  forecast = series
  backcast = rev(series)
  data = c("forecast", "backcast")
  #data = c("forecast")
  imputed = list(forecast = c(), backcast = c(), ga.solution.forecast = "", ga.solution.backcast = "")

  for (j in data) {
    print(j)
    yt = get(j)

    # build optimized autorregressive matrix
    df.na = lag_matrix(yt, ar.terms)
    df = na.omit(df.na)

    # optimises SVR model
    ga = ga.optimisation.svr(fit.fun = get(fitness), data = df, ...)
    print(ga@solution)
    print(ga@fitnessValue)

    svrmod = svm(yt ~ ., df,
                 type = "eps-regression",
                 kernel = "radial",
                 gamma = ga@solution[1],
                 cost = ga@solution[2],
                 epsilon = ga@solution[3])

    # auxiliary data.frame with autorregressive values
    df.aux = data.frame(matrix(nrow = 1, ncol = ar.terms)); names(df.aux) = names(df)[-length(df)]

    # original series to receive imputations
    prev = df.na$yt
    for (i in 1:length(df.na$yt)) {
      if (is.na(df.na$yt[i])) {
        tryCatch(
          expr = {
            df.aux[1,] = try(prev[(i-1):(i-ar.terms)])
            prev[i] = predict(svrmod, df.aux)
          },
          error = function(e){},
          warning = function(w){},
          finally = {})
      }
    }

    # result into a list
    imputed[[j]] = c(yt[1:ar.terms], prev)
    imputed[[paste("ga.solution.",j,sep="")]] = ga@solution
  }

  #series[miss.index] = imputed$forecast[miss.index]
  #ga.solution = paste(round(as.numeric(imputed$ga.solution.forecast),3), collapse = ", ")
  series[miss.index] = rowMeans(cbind(imputed$forecast[miss.index], rev(imputed$backcast[miss.index])))
  ga.solution = paste(paste(round(as.numeric(imputed$ga.solution.forecast),3), collapse = ", "),
                      paste(round(as.numeric(imputed$ga.solution.backcast),3), collapse = ", "), collapse = " ")

  return(list(series = series, ga.solution = ga.solution))
}

#' Faz imputa??o de dados ausentes com SVR autorregressivo
#' em series temporais univariadas
#'
#' @param serie: objeto do tipo ts a ser imputado
#' @param termos_ar: indice do valor passado (lagged) ex.: y_{t-index}
#' @param type: forecast ou backcast
#' @return stNAPoco: objeto do tipo matriz
#'
na_svr_ar = function(serie, termos_ar, type) {
  if (type == "forecast") {
    yt = serie
  } else if (type == "backcast") {
    yt = rev(serie)
  } else {
    stop("type argument must be eithe 'forecast' or 'backcast'")
  }

  # recebe a serie
  # monta matriz autorregressiva
  #yt = serie
  df_na = lag_matrix(yt, termos_ar)
  df = na.omit(df_na)

  # constroi modelo SVR
  svrmod = svm(yt ~ .,
               df,
               kernel = "radial",
               degree = 3,
               coef0 = 0,
               cost = 1)

  # data.frame auxiliar com valores autorregressivos
  # como entradas no modelo para calculo do valor a ser imputado
  df_aux = data.frame(matrix(nrow = 1, ncol = termos_ar)); names(df_aux) = names(df)[-length(df)]

  # serie original para receber imputa??o
  prev = df_na$yt

  for (i in 1:length(df_na$yt)) {
    if (is.na(df_na$yt[i])) {
      tryCatch(
        expr = {
          # Your code...
          # goes here...
          # ...
          df_aux[1,] = try(prev[(i-1):(i-termos_ar)])
          prev[i] = predict(svrmod, df_aux)
        },
        error = function(e){
          # (Optional)
          # Do this if an error is caught...
          prev[i] = NA
        },
        warning = function(w){
          # (Optional)
          # Do this if an warning is caught...
        },
        finally = {
          # (Optional)
          # Do this at the end before quitting the tryCatch structure...
        }
      )

      #df_aux[1,] = try(prev[(i-1):(i-termos_ar)])
      #prev[i] = predict(svrmod, df_aux)
    }
  }

  if (type == "forecast") {
    return(c(yt[1:termos_ar], prev))
  } else {
    return(rev(c(yt[1:termos_ar], prev)))
  }
}

#' Separa um data.frame de entrada em um conjunto de treino e um de testes
#'
#' @param dados: data.frame contendo a ser separado
#' @param t.size: porcentagem dos dados a ser destinada ao conjunto de treino
#' @param v.size: porcentagem dos dados de treino a ser destinada ao conjunto de validacao
#' @param cv.type: cross-validation type for validation data set: "random" or "recent values"
#' @return lista contendo os dois data.frames, de treino e de teste
#'
cross.validation = function(dados, t.size, v.size, cv.type = "random") {

  tam = t.size
  tam_treino = trunc(tam * length(dados[,1]))
  dados_treino = dados[1:tam_treino,]
  dados_teste = dados[(tam_treino+1):length(dados[,1]),]

  val = 1-v.size
  tam_valida = trunc(val * length(dados_treino[,1]))

  if(cv.type == "random") {
    smp = sort(sample(nrow(dados_treino), tam_valida))
    inv = c(1:nrow(dados_treino))[-smp]

    dados_treinamento = dados_treino[smp, ]
    ifelse(test = {val == 1},
           yes = {dados_valida = dados_treinamento},
           no = {dados_valida = dados_treino[inv, ]})
  }

  else if (cv.type == "most recent") {
    dados_treinamento = dados_treino[1:tam_valida,]
    ifelse(test = {val == 1},
           yes = {dados_valida = dados_treinamento},
           no = {dados_valida = dados_treino[-(1:tam_valida),]})
  }

  return(list(treinamento = dados_treinamento,
              valida = dados_valida,
              treino = dados_treino, # valida + treinamento
              teste = dados_teste))
}

# GA optimisation ----

#' Fitness function for a GA optimisation of a SVR model with radial basis kernel by the RMSE
#'
#' @param par: array of length 3 with gamma, cost and epsilon parameters for a radial basis kernel svr
#' @param data: data.frame within a 'yt' column
#' @param t.size: percentage of data to be in the training set
#' @param v.size: percentage of the training data to be in the validation set
#' @param cv.type: cross-validation type: "random" or "recent values"
#' @return RMSE fitness value
#'
ga.fit.svr.radial = function(par, data, t.size, v.size, cv.type = "random") {
  training = cross.validation(data, t.size, v.size, cv.type)

  df.train = training$treino # conjunto de treinamento
  df.training = training$treinamento # treino sem valida??o
  df.validation = training$valida # valida??o

  df.test = training$teste

  # treino sem valida??o
  svrmod = svm(formula = yt~.,
               data = df.training,
               type = "eps-regression",
               kernel = "radial",
               gamma = par[1],
               cost = par[2],
               epsilon = par[3])

  # avalia no valida??o
  prev = predict(svrmod, df.validation)

  return(-rmse(previsto = prev, original = df.validation$yt))
}

ga.fit.svr.ar.radial = function(par, training.series, validation.length, cv.type = "most recent") {

  # autorregressive terms
  df.lag = lag_matrix(serie = training.series, terms = round(par[4]))
  df = na.omit(df.lag)

  # cross validation
  #training = cross.validation(df, 1, validation.length, cv.type)

  # data sets
  #df.train = training$treino # treino + validation
  #df.training = training$treinamento # treino sem valida??o
  #df.validation = training$valida # valida??o
  df.training = head(df, (nrow(df) - validation.length))
  df.validation = tail(df, validation.length)
  #df.test = training$teste # test (unused on imputation simulation)

  # treino sem valida??o
  svrmod = svm(formula = yt ~ .,
               data = df.training,
               type = "eps-regression",
               kernel = "radial",
               gamma = par[1],
               cost = par[2],
               epsilon = par[3])

  # avalia no valida??o
  prev = predict(svrmod, df.validation)

  return(-rmse(previsto = prev, original = df.validation$yt))
}

#' Fitness function for a GA optimisation of a SVR model with radial basis kernel by the RMSE
#'
#' @param fit.fun: fitness function for a GA optimisation
#' @param data: data.frame within a 'yt' column
#' @param t.size: percentage of data to be in the training set
#' @param v.size: percentage of the training data to be in the validation set
#' @param cv.type: cross-validatiom type
#' @param min: lower
#' @param max: upper
#' @return GA object
#'
ga.optimisation.svr = function(fit.fun, data, t.size = 0.8, v.size = 0.25, min = c(gamma = 0.0001, cost = 0.0001, epsilon = 0.0001), max = c(gamma = 1, cost = 1, epsilon = 0.5), ...) {
  GA = ga(type = "real-valued",
          fitness = fit.fun, data, t.size, v.size,
          lower = min,
          upper = max,
          names = names(min), ...)

  return(GA)
}

ga.optimisation.svr.ar = function(fit.fun = get("ga.fit.svr.ar.radial"), training.series, validation.length, min = c(gamma = 0.0001, cost = 0.0001, epsilon = 0.0001, terms = 1), max = c(gamma = 1, cost = 1, epsilon = 0.5, terms = 13), ...) {
  GA = ga(type = "real-valued",
          fitness = fit.fun, training.series, validation.length,
          lower = min,
          upper = max,
          names = names(min), ...)

  print(GA@solution)
  return(GA)
}


#' @title Missing Value Imputation by Interpolation
#'
#' @description Uses either linear, spline or stineman interpolation
#' to replace missing values.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#'  object in which missing values shall be replaced
#'
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"linear" - for linear interpolation using \link{approx} } (default choice)
#'    \item{"spline" - for spline interpolation using \link{spline}}
#'    \item{"stine" - for Stineman interpolation using \link[stinepack]{stinterp}}
#'    }
#'
#' @param maxgap Maximum number of successive NAs to still perform imputation on.
#'  Default setting is to replace all NAs without restrictions. With this
#'  option set, consecutive NAs runs, that are longer than 'maxgap' will
#'  be left NA. This option mostly makes sense if you want to
#'  treat long runs of NA afterwards separately.
#'
#' @param ... Additional parameters to be passed through to \link{approx} or
#'  \link{spline} interpolation functions
#'
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#'  object (dependent on given input at parameter x)
#'
#' @details Missing values get replaced by values of a \link{approx}, \link{spline}
#' or \link[stinepack]{stinterp} interpolation.
#'
#' @author Steffen Moritz
#'
#' @seealso  \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Prerequisite: Create Time series with missing values
#' x <- ts(c(2, 3, 4, 5, 6, NA, 7, 8))
#'
#' # Example 1: Perform linear interpolation
#' na_interpolation(x)
#'
#' # Example 2: Perform spline interpolation
#' na_interpolation(x, option = "spline")
#'
#' # Example 3: Perform stine interpolation
#' na_interpolation(x, option = "stine")
#'
#' # Example 4: Same as example 1, just written with pipe operator
#' x %>% na_interpolation()
#'
#' # Example 5: Same as example 2, just written with pipe operator
#' x %>% na_interpolation(option = "spline")
#' @references Johannesson, Tomas, et al. (2015). "Package stinepack".
#' @importFrom stats ts approx spline
#' @importFrom stinepack stinterp
#' @importFrom magrittr %>%
#' @export

na_interpolation_edit <- function(x, option = "linear", maxgap = Inf, ...) {
  data <- x

  #----------------------------------------------------------
  # Mulivariate Input
  # The next 20 lines are just for checking and handling multivariate input.
  #----------------------------------------------------------

  # Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    # Go through columns and impute them by calling this function with univariate input
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {
        next
      }
      # if imputing a column does not work - mostly because it is not numeric - the column is left unchanged
      tryCatch(data[, i] <- na_interpolation(data[, i], option, maxgap), error = function(cond) {
        warning(paste("imputeTS: No imputation performed for column", i, "because of this", cond), call. = FALSE)
      })
    }
    return(data)
  }


  #----------------------------------------------------------
  # Univariate Input
  # All relveant imputation / pre- postprocessing  code is within this part
  #----------------------------------------------------------

  else {
    missindx <- is.na(data)

    ##
    ## 1. Input Check and Transformation
    ##


    # 1.1 Check if NAs are present
    if (!anyNA(data)) {
      return(data)
    }

    # 1.2 special handling data types
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }

    # 1.3 Check for algorithm specific minimum amount of non-NA values
    if (sum(!missindx) < 2) {
      stop("Input data needs at least 2 non-NA data point for applying na_interpolation")
    }

    # 1.4 Checks and corrections for wrong data dimension

    # Check if input dimensionality is not as expected
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x")
    }

    # Altering multivariate objects with 1 column (which are essentially
    # univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }

    # 1.5 Check if input is numeric
    if (!is.numeric(data)) {
      stop("Input x is not numeric")
    }

    ##
    ## End Input Check
    ##


    ##
    ## 2. Imputation Code
    ##

    n <- length(data)

    allindx <- 1:n
    indx <- allindx[!missindx]

    data_vec <- as.vector(data)

    if (option == "linear") {
      interp <- stats::approx(indx, data_vec[indx], 1:n, rule = 2, ...)$y
    }
    else if (option == "spline") {
      interp <- stats::spline(indx, data_vec[indx], n = n, ...)$y
    }
    else if (option == "stine") {
      interp <- stinepack::stinterp(indx, data_vec[indx], 1:n, ...)$y
      # avoid NAs at the beginning and end of series // same behavior like
      # for approx with rule = 2.
      if (any(is.na(interp))) {
        interp <- na_locf(interp, na_remaining = "rev")
      }
    }
    else if (option == "polynom") {
      z <- polynom::poly.calc(indx, data_vec[indx])
      pr <- as.function(z)
      interp <- pr(1:n)
    }
    else {
      stop("Wrong parameter 'option' given. Value must be either 'linear', 'spline' or 'stine'.")
    }

    # Merge interpolated values back into original time series
    data[missindx] <- interp[missindx]

    ##
    ## End Imputation Code
    ##


    ##
    ## 3. Post Processing
    ##

    # 3.1 Check for Maxgap option

    # If maxgap = Inf then do nothing and when maxgap is lower than 0
    if (is.finite(maxgap) && maxgap >= 0) {

      # Get logical vector of the time series via is.na() and then get the
      # run-length encoding of it. The run-length encoding describes how long
      # the runs of FALSE and TRUE are
      rlencoding <- rle(is.na(x))

      # Runs smaller than maxgap (which shall still be imputed) are set FALSE
      rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE

      # The original vector is being reconstructed by reverse.rls, only now the
      # longer runs are replaced now in the logical vector derived from is.na()
      # in the beginning all former NAs that are > maxgap are also FALSE
      en <- inverse.rle(rlencoding)

      # Set all positions in the imputed series with gaps > maxgap to NA
      # (info from en vector)
      data[en == TRUE] <- NA
    }

    ##
    ## End Post Processing
    ##


    ##
    ## 4. Final Output Formatting
    ##

    # Give back the object originally supplied to the function
    # (necessary for multivariate input with only 1 column)
    if (!is.null(dim(x)[2])) {
      x[, 1] <- data
      return(x)
    }

    ##
    ## End Final Output Formatting
    ##

    return(data)
  }
}

na_kalman_edit <- function(x, ...) {
  require("imputeTS")
  y <- as.numeric(x)

  imp <- na_kalman(y, ...)

  return(imp)
}

#' @title Seasonally Splitted Missing Value Imputation
#'
#' @description Splits the times series into seasons and afterwards performs
#' imputation separately for each of the resulting time series datasets
#' (each containing the data for one specific season).
#
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#' @param algorithm Algorithm to be used after splits.
#' Accepts the following input:
#' \itemize{
#'    \item{"interpolation" - Imputation by Interpolation} (default choice)
#'    \item{"locf" - Imputation by Last Observation Carried Forward}
#'    \item{"mean" - Imputation by Mean Value}
#'    \item{"random" - Imputation by Random Sample}
#'    \item{"kalman" - Imputation by Kalman Smoothing and State Space Models}
#'    \item{"ma" - Imputation by Weighted Moving Average}
#'    }
#'
#' @param find_frequency If TRUE the algorithm will try to estimate the frequency
#' of the time-series automatically.
#'
#' @param maxgap Maximum number of successive NAs to still perform imputation on.
#'  Default setting is to replace all NAs without restrictions. With this
#'  option set, consecutive NAs runs, that are longer than 'maxgap' will
#'  be left NA. This option mostly makes sense if you want to
#'  treat long runs of NA afterwards separately.
#'
#' @param ... Additional parameters for these algorithms that can be
#' passed through. Look at \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_locf}}, \code{\link[imputeTS]{na_random}},
#' \code{\link[imputeTS]{na_mean}} for parameter options.
#'
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object (dependent on given input at parameter x)
#'
#' @author Steffen Moritz
#'
#' @seealso  \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}
#'
#' @examples
#' # Example 1: Perform seasonal splitted imputation using algorithm = "interpolation"
#' na_seasplit(tsAirgap, algorithm = "interpolation")
#'
#' # Example 2: Perform seasonal splitted imputation using algorithm = "mean"
#' na_seasplit(tsAirgap, algorithm = "mean")
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsAirgap %>% na_seasplit(algorithm = "interpolation")
#'
#' @importFrom stats frequency ts
#' @importFrom magrittr %>%
#' @export
#' @name na_seasplit

na_seasplit_edit <- function(x, algorithm = "interpolation", find_frequency = FALSE, maxgap = Inf, ...) {
  data <- x



  #----------------------------------------------------------
  # Mulivariate Input
  # The next 20 lines are just for checking and handling multivariate input.
  #----------------------------------------------------------

  # Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    # Go through columns and impute them by calling this function with univariate input
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {
        next
      }
      # if imputing a column does not work - mostly because it is not numeric - the column is left unchanged
      tryCatch(data[, i] <- na_seasplit(data[, i], algorithm, find_frequency, maxgap, ...), error = function(cond) {
        warning(paste("imputeTS: No imputation performed for column", i, "because of this", cond), call. = FALSE)
      })
    }
    return(data)
  }


  #----------------------------------------------------------
  # Univariate Input
  # All relveant imputation / pre- postprocessing  code is within this part
  #----------------------------------------------------------

  else {
    missindx <- is.na(data)

    ##
    ## 1. Input Check and Transformation
    ##


    # 1.1 Check if NAs are present
    if (!anyNA(data)) {
      return(data)
    }

    # 1.2 special handling data types
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }

    # 1.3 Check for algorithm specific minimum amount of non-NA values
    if (sum(!missindx) < 3) {
      stop("Input data needs at least 3 non-NA data point for applying na_seasplit")
    }

    # 1.4 Checks and corrections for wrong data dimension

    # Check if input dimensionality is not as expected
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x")
    }

    # Altering multivariate objects with 1 column (which are essentially
    # univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }

    # 1.5 Check if input is numeric
    if (!is.numeric(data)) {
      stop("Input x is not numeric")
    }


    # 1.6 Checks and corrections for time series frequency

    # Try to findFrequency
    if (find_frequency == TRUE) {
      t <- as.vector(data)
      freq <- forecast::findfrequency(na_interpolation(t))
      if (freq > 1) {
        data <- ts(t, frequency = freq)
      }
    }

    if (stats::frequency(data) == 1) {
      warning("No seasonality information for dataset could be found, going on without decomposition.
              Setting find_frequency=TRUE might be an option.")
      data <- apply_base_algorithm(data, algorithm = algorithm, ...)
      return(data)
    }

    if (length(data) < stats::frequency(data) * 2) {
      warning("More than 2 complete periods needed to perform a seasonal split. The algorithm will go on without seasonal split.")
      data <- apply_base_algorithm(data, algorithm = algorithm, ...)
      return(data)
    }

    ##
    ## End Input Check and Transformation
    ##


    ##
    ## 2. Imputation Code
    ##

    for (i in 1:stats::frequency(data)) {

      # get indices for one season
      indices <- seq(from = i, to = length(data), by = stats::frequency(data))

      # Create time series just with one season
      ts_temp <- stats::ts(data[indices])

      # Apply algorithm on this season
      ts_temp <- apply_base_algorithm(ts_temp, algorithm = algorithm, ...)

      # Write result back into original time series
      data[indices] <- as.vector(ts_temp)
    }

    ##
    ## End Imputation Code
    ##


    ##
    ## 3. Post Processing
    ##

    # 3.1 Check for Maxgap option

    # If maxgap = Inf then do nothing and when maxgap is lower than 0
    if (is.finite(maxgap) && maxgap >= 0) {

      # Get logical vector of the time series via is.na() and then get the
      # run-length encoding of it. The run-length encoding describes how long
      # the runs of FALSE and TRUE are
      rlencoding <- rle(is.na(x))

      # Runs smaller than maxgap (which shall still be imputed) are set FALSE
      rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE

      # The original vector is being reconstructed by reverse.rls, only now the
      # longer runs are replaced now in the logical vector derived from is.na()
      # in the beginning all former NAs that are > maxgap are also FALSE
      en <- inverse.rle(rlencoding)

      # Set all positions in the imputed series with gaps > maxgap to NA
      # (info from en vector)
      data[en == TRUE] <- NA
    }

    ##
    ## End Post Processing
    ##


    ##
    ## 4. Final Output Formatting
    ##

    # Give back the object originally supplied to the function
    # (necessary for multivariate input with only 1 column)
    if (!is.null(dim(x)[2])) {
      x[, 1] <- data
      return(x)
    }

    ##
    ## End Final Output Formatting
    ##

    return(data)
  }
}

plot_errors_edit <- function(dataIn, plotType = c('boxplot')){
  if(!plotType %in% c('boxplot', 'bar', 'line'))
    stop('plotType must be boxplot, bar, or line')

  # boxplot
  if(plotType == 'boxplot'){

    toplo <- attr(dataIn, 'errall')
    toplo <- melt(toplo)
    percs <- dataIn$MissingPercent
    toplo$L2 <- factor(toplo$L2, levels = unique(toplo$L2), labels = percs)
    names(toplo) <- c('Medida de erro', 'Dados ausentes (%)', 'Métodos')

    p <- ggplot(toplo, aes(x = `Percent of missing observations`, y = `Error value`)) +
      ggtitle(dataIn$Parameter) +
      geom_boxplot(aes(fill = Methods)) +
      theme_bw()

    return(p)

  }

  # data for line or bar
  # dataIn <- bbench
  toplo <- data.frame(dataIn[-1])
  toplo <- melt(toplo, id.var = 'MissingPercent')
  toplo$MissingPercent <- factor(toplo$MissingPercent)
  # error.mt <- dataIn$Parameter
  # names(toplo) <- c('Dados ausentes (%)', 'Métodos', error.mt)

  # barplot
  if(plotType == 'bar'){

    p <- ggplot(toplo, aes(x = `Percent of missing observations`, y = `Error value`)) +
      ggtitle(dataIn$Parameter) +
      geom_bar(aes(fill = Methods), stat = 'identity', position = 'dodge') +
      theme_bw()

    return(p)

  }

  # line plot
  if(plotType == 'line'){

    p <- ggplot(toplo, aes(x = MissingPercent, y = value, group = variable)) +
      ggtitle("") +
      labs(title = "",
           x = "Ausência de dados (%)",
           y = paste0(toupper(dataIn$Parameter)," (m)"),
           fill = "Método") +
      geom_line() +
      geom_point(aes(fill = variable), shape = 21, size = 5, alpha = 0.75) +
      theme_bw() +
      scale_color_brewer(type = 'qual', palette = 'Paired', aesthetics = "fill")

    return(p)
  }
}

check_component_strength <- function(x, ...) {
  ts.stl = stl(x, s.window = "periodic", robust = FALSE, ...)

  ts.sea = ts.stl$time.series[,1] # trend
  ts.tre = ts.stl$time.series[,2] # seasonality
  ts.rem = ts.stl$time.series[,3] # remainder

  # measuring strength of trend and seasonality
  f.tre = max(0,1-var(ts.rem)/var(ts.tre + ts.rem)) # seasonally adjusted
  f.sea = max(0,1-var(ts.rem)/var(ts.sea + ts.rem)) # detrended

  return(list(trend = f.tre, seasonal = f.sea))
}

getOservedIntervals <- function(x) {
  # x <- c(1,NA,3,4,NA,6,7,8,NA,8,NA,NA,9)
  mi <- !is.na(x); tr <- c(); ss <- 0; pi <- 0; pe <- 0
  for(i in 1:length(x)){
    if(mi[i]) { # obs
      if(i==1) {
        tr[i] <- ss
      } else if(i>1 && mi[i-1]) {
        tr[i] <- ss
      } else if(i>1 && !mi[i-1]) {
        ss <- ss+1
        tr[i] <- ss
      }
    } else { # mis
      if(i==1) {
        tr[i] <- NA
      } else if(i>1) {
        tr[i] <- NA
      }
    }
  }; rbind(x,mi,tr)

  intervals <- unique(na.omit(tr)); pos.i <- c(); pos.f <- c()
  for(i in 1:length(intervals)) {
    pos.i[i] <- which(tr==intervals[i])[1]
    pos.f[i] <- which(tr==intervals[i])[length(which(tr==intervals[i]))]
  }

  tbls <- table(tr)
  df <- as.data.frame(t(rbind(as.numeric(names(tbls)),as.numeric(tbls),pos.i,pos.f)))
  df <- df[order(df$V2, decreasing = T), ]
  colnames(df) <- c("Interval","Count","Start","End")

  return(list("intervals"=df, "lenghtlongest"=max(df$Count)))
}

# MESOR ----
getMinimalVariancePredictor = function(residuals, forecasts, target, from, to){
  # print(paste("*************", "MINIMAL VARIANCE COMBINATION", "*************"))
  modellingTime = Sys.time()#proc.time()[[3]]
  nSingleModels = ncol(forecasts)
  covMatrix=cov(residuals[from:to, ], use = "pairwise.complete.obs")
  invCovMatrix = tryCatch({
    # expr = inv(covMatrix)},
    expr = base::solve(covMatrix)},
    error = function(e){
      message("Error: inv(covMatrix). Approaching via ginv(covMatrix)");
      return(ginv(covMatrix))
    }
    #,finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
  )

  #print(covMatrix%*%invCovMatrix)
  weigths = numeric(nSingleModels)
  for(i in 1:nSingleModels){
    weigths[i] = sum(invCovMatrix[i,])
  }
  sw=sum(weigths)
  weigths = weigths/sw
  modellingTime = Sys.time() - modellingTime#proc.time()[[3]] - modellingTime#time in seconds
  forecastingTime = Sys.time()#proc.time()[[3]]
  mvForecasts = weigths[1]*forecasts[,1]
  for(i in 2:nSingleModels){
    mvForecasts = mvForecasts + weigths[i]*forecasts[,i]
  }
  forecastingTime = Sys.time() - forecastingTime#proc.time()[[3]] - forecastingTime
  modellingTime = as.numeric(modellingTime)
  forecastingTime = as.numeric(forecastingTime)
  weigths = as.list(weigths)
  names(weigths) = colnames(forecasts)
  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = forecastingTime
  optimal$forecasts = mvForecasts
  optimal$weigths = weigths
  optimal$cov = covMatrix

  optimal$coefficients = weigths
  optimal$residuals = mvForecasts - target
  optimal$model = forecasts
  return(optimal)
}

get_cMvNewForecasts = function(ModelsObjs, singleForecasts){
  forecastingTime = Sys.time()#proc.time()[[3]]
  singleForecasts = singleForecasts
  cMVobj = ModelsObjs#$opt.c.MvObj
  w = cMVobj$weigths
  nModels = length(w)
  nmModels = names(w)
  forecasts = 0
  for(i in 1:nModels){
    nmModel_i = nmModels[i]
    forecasts = forecasts + w[[nmModel_i]]*singleForecasts[,which(colnames(singleForecasts)==nmModel_i)]
  }
  forecastingTime = Sys.time() - forecastingTime#proc.time()[[3]] - forecastingTime
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = forecasts
  return (ret)
}

getSimpleAverageForecasts = function(forecasts){
  t1 <- Sys.time()
  pred <- apply(forecasts, 1, mean)
  duration <- as.numeric(Sys.time() - t1)

  return(list('forecasts' = pred, 'forecasting.time' = duration))
}
getSimpleMedianForecasts = function(forecasts){#forecasts=single.forecasts
  t1 <- Sys.time()
  pred <- apply(forecasts, 1, quantile, 0.5)
  duration <- as.numeric(Sys.time() - t1)

  return(list('forecasts' = pred, 'forecasting.time' = duration))
}
getMPD_viaInformationCriterion = function(e){
  require("rmutil")
  require("fGarch")
  require("copula")
  require("moments")
  require("MASS")

  ret = list()
  ret$residuals = e
  #e = e[!is.na(e)]
  n = length(e); k = log(n)

  margins = NULL; logLiks = NULL; nPars = NULL; BICs = NULL; parsEstimates = list(); i=0

  #SYMMETRICAL PDFs
  margins = c(margins, "norm")
  i=i+1; parsEstimates[[i]] = list(mean=mean(e, na.rm = TRUE), sd = sd(e, na.rm = TRUE))
  logLiks = c(logLiks, sum(dnorm(x = e, mean = parsEstimates[[i]]$mean, sd = parsEstimates[[i]]$sd, log=TRUE), na.rm = TRUE))

  margins = c(margins, "cauchy")
  location = median(e, na.rm = TRUE)
  q = quantile(e, probs = c(.25, .75), na.rm = TRUE)
  scale = as.numeric(q[2]-q[1])/2
  i=i+1; parsEstimates[[i]] =  list(location=location, scale = scale)
  logLiks = c(logLiks, sum(dcauchy(x = e, location = parsEstimates[[i]]$location, scale = parsEstimates[[i]]$scale, log=TRUE), na.rm = TRUE))

  margins = c(margins, "laplace")
  m = median(e, na.rm = TRUE)
  s = sd(e, na.rm = TRUE)/sqrt(2)
  i=i+1; parsEstimates[[i]] =  list(m = m, s = s)
  logLiks = c(logLiks, sum(dlaplace(y = e, m = parsEstimates[[i]]$m, s = parsEstimates[[i]]$s, log=TRUE), na.rm = TRUE))

  #ASYMMETRICAL PDFs

  margins = c(margins, "snorm")
  fit = snormFit(e[which(!is.na(e))])
  mean = fit$par[1]; sd = fit$par[2]; xi = fit$par[3]
  i=i+1; parsEstimates[[i]] = list(mean=mean, sd = sd, xi = xi)
  logLiks = c(logLiks, sum(dsnorm(x = e, mean = parsEstimates[[i]]$mean, sd = parsEstimates[[i]]$sd, xi = parsEstimates[[i]]$xi, log=TRUE), na.rm = TRUE))

  # TAKING THE BEST BIC FIT
  minBIC = Inf; minBIC_index = NULL; nFits = length(margins)
  for(i in 1:nFits){
    nPars[i] = length(parsEstimates[[i]])
    BICs[i] = -2*logLiks[i] + k*nPars[i]
    if(BICs[i] < minBIC){
      minBIC = BICs[i]; minBIC_index = i
    }
  }
  ret$nPars = nPars[minBIC_index]
  ret$BIC = BICs[minBIC_index]
  ret$margin = margins[minBIC_index]
  ret$parEstimates = parsEstimates[[minBIC_index]]
  ret$loglik = logLiks[minBIC_index]

  # rocj: get BIC values from all models
  names(BICs) <- margins
  ret$trash <- BICs

  #TAKING THE BEST BIC FIT CDF RESIDUALS POINTS
  if(ret$margin=="norm"){
    ret$CDF = function(x){return(pnorm(x, mean=ret$parEstimates[[1]], sd=ret$parEstimates[[2]]))}
    ret$pdf = function(x){return(dnorm(x = x, mean = ret$parEstimates[[1]], sd = ret$parEstimates[[2]]))}
  }
  else if(ret$margin=="cauchy"){
    ret$CDF = function(x){return(pcauchy(x, location=ret$parEstimates[[1]], scale=ret$parEstimates[[2]]))}
    ret$pdf = function(x){return(dcauchy(x = x, location=ret$parEstimates[[1]], scale=ret$parEstimates[[2]]))}
  }
  else if(ret$margin=="laplace"){
    ret$CDF = function(x){return(plaplace(x, m=ret$parEstimates[[1]], s=ret$parEstimates[[2]]))}
    ret$pdf = function(x){return(dlaplace(x, m=ret$parEstimates[[1]], s=ret$parEstimates[[2]]))}
  }
  else if(ret$margin=="snorm"){
    ret$CDF = function(x){return(psnorm(x, mean=mean, sd = sd, xi = xi))}
    ret$pdf = function(x){return(dsnorm(x = x, mean = ret$parEstimates[[1]], sd = ret$parEstimates[[2]], xi = ret$parEstimates[[3]]))}
  }
  return(ret)
}

getCopulaBasedPredictor = function(targets = target.all.norm,
                                   forecasts=single.forecasts,
                                   from=(n+1), to=(n+v)){
  # targets <- y
  # forecasts <- cbind(mod.arima$fitted, mod.ets$fitted)

  # print(paste("*************", seriesName, ": COPULA-BASED COMBINATION", "*************"))
  modellingTime = Sys.time()
  nSingleModels = ncol(forecasts)#nSingleModels=3
  residuals = targets - forecasts
  #pairsFigure(residuals[from:to,], seriesName)#paste(seriesName, "withOutliers", sep="."))
  copulaFamily = list(Empirical = "Empirical", Archimedean= "Archimedean", Elliptical = "Elliptical", Extreme_Value = "Extreme_Value")
  copulaDists = list(); copulaDists.initPar = list(); copulaDists.nPar = list();

  copulaDists[[copulaFamily$Empirical]]=        c("rankCacoullos")#, "cvtCacoullos", "Cacoullos")
  copulaDists.initPar[[copulaFamily$Empirical]]=c(1)
  copulaDists.nPar[[copulaFamily$Empirical]]=   rep(1, length(copulaDists.initPar$Empirical))

  copulaDists[[copulaFamily$Archimedean]]=        c("clayton", "frank", "gumbel", "joe")#, "amh"
  copulaDists.initPar[[copulaFamily$Archimedean]]=c(1,         1,        2,        2)#   .5,
  copulaDists.nPar[[copulaFamily$Archimedean]]=   rep(1, length(copulaDists.initPar$Archimedean))

  copulaDists[[copulaFamily$Elliptical]]=         c("normal")#, "t")
  copulaDists.initPar[[copulaFamily$Elliptical]]= c(.5)#,       .5)
  copulaDists.nPar[[copulaFamily$Elliptical]]=   rep((nSingleModels*(nSingleModels-1)/2), length(copulaDists.initPar$Elliptical))

  copulaDists[[copulaFamily$Extreme_Value]]= NULL#c("tawn", "huslerReiss", "galambos", "tev")
  copulaDists.initPar[[copulaFamily$Extreme_Value]]= c(.5,       .5)
  copulaDists.nPar[[copulaFamily$Extreme_Value]]=   rep(2, length(copulaDists.initPar$Extreme_Value))

  c.copulaDists = c(copulaDists$Empirical, copulaDists$Archimedean, copulaDists$Elliptical, copulaDists$Extreme_Value)
  c.copulaDists.initPar = c(copulaDists.initPar$Empirical, copulaDists.initPar$Archimedean, copulaDists.initPar$Elliptical, copulaDists.initPar$Extreme_Value)
  c.copulaDists.nPar = c(copulaDists.nPar$Empirical, copulaDists.nPar$Archimedean, copulaDists.nPar$Elliptical, copulaDists.nPar$Extreme_Value)
  c.family = c(rep(copulaFamily$Empirical, length(copulaDists$Empirical))
               , rep(copulaFamily$Archimedean, length(copulaDists$Archimedean))
               , rep(copulaFamily$Elliptical, length(copulaDists$Elliptical))
               , rep(copulaFamily$Extreme_Value, length(copulaDists$Extreme_Value)))

  modelLabels = dimnames(forecasts)[[2]]
  margins = NULL; paramMargins = list(); CDFs = NULL; MPDs = list(); Errors = NULL
  outliersIndexes = NULL
  NA_indexes = NULL
  # from <- 1; to <- length(y)
  for(i in 1:nSingleModels){
    # i <- 2
    modelLabel = modelLabels[i]
    e = residuals[from:to,i]
    # NA_indexes_i = which(is.na(e))
    # outliersIndexes_i = getOutliersIndexes(e)
    # e[outliersIndexes_i] = NA
    # outliersIndexes = union(outliersIndexes, outliersIndexes_i)
    # NA_indexes = union(NA_indexes, c(NA_indexes_i, outliersIndexes_i))
    MPD = getMPD_viaInformationCriterion(e)
    #histFigure(data=e, MPD, modelName=modelLabel, seriesName)
    margins = c(margins, MPD$margin)
    paramMargins[[i]] = MPD$parEstimates
    CDFs = cbind(CDFs, MPD$CDF(e))
    Errors = cbind(Errors, MPD$residuals)
    MPDs[[modelLabel]] = MPD
  }
  # if(length(NA_indexes)>0){#eventual outliers are also removed (they were set NA in the for(...))
  #   Errors = Errors[-NA_indexes, ]
  #   CDFs = CDFs[-NA_indexes, ]
  # }
  # if(length(outliersIndexes)>0){
  #   # dimnames(Errors) = list(NULL, modelLabels)
  #   Errors = Errors[-outliersIndexes, ]
  #   CDFs = CDFs[-outliersIndexes, ]
  #   # pairsFigure(as.data.frame(Errors), paste(seriesName, "withoutOutliers", sep="."))
  # }

  getOptimalMvdc = function(Errors, CDFs, margins, paramMargins){
    nSingleModels = dim(Errors)[2]
    n = dim(Errors)[1]
    mvdc_ = NULL
    BIC = Inf
    bestCopulaDist = NULL
    nCopulaDists = length(c.copulaDists)
    for(i in 1:nCopulaDists){#i=6
      loglike = -Inf; nPar = Inf
      copulaDist = c.copulaDists[i]
      nPars = c.copulaDists.nPar[i]
      initPars = rep(c.copulaDists.initPar[i], nPars)
      family = c.family[i]
      copula = NULL;
      if (family==copulaFamily$Empirical) {}
      else {
        # trash = function(){
        # }
        if (family == copulaFamily$Elliptical) {
          copula = ellipCopula(
            copulaDist,
            param = initPars,
            dim = nSingleModels,
            dispstr = "un")#, df = ((n-1)^nSingleModels))
        } else if (family==copulaFamily$Archimedean) {
          copula = archmCopula(
            copulaDist,
            param = initPars,
            dim = nSingleModels)
        } else if (family==copulaFamily$Extreme_Value) {
          copula = evCopula(
            copulaDist,
            param = initPars,
            dim = nSingleModels)
        }
        mvdc_i = mvdc(copula = copula, margins = margins, paramMargins = paramMargins)
        fitIfm = tryCatch(
          fitCopula(mvdc_i@copula, CDFs, method="ml")
          , error = function(e){return(NULL)})#, finally=function(){return(NULL)})
        #properties_fitIfm = summary(fitIfm); #IfmCoefficients = properties_fitIfm$coefficients[1]
        if(!is.null(fitIfm)){
          IfmCoefficients = coef(fitIfm)
          if(family==copulaFamily$Elliptical){
            copula = ellipCopula(copulaDist, param=IfmCoefficients, dim=nSingleModels, dispstr="un")#, df = ((n-1)^nSingleModels))
          } else if(family==copulaFamily$Archimedean){
            copula = archmCopula(copulaDist, param=IfmCoefficients, dim=nSingleModels)
          } else if(family==copulaFamily$Extreme_Value){
            copula = evCopula(copulaDist, param=IfmCoefficients, dim=nSingleModels)
          }
          mvdc_i = mvdc(copula = copula, margins = margins, paramMargins = paramMargins)
          loglike = fitIfm@loglik
          nPar = length(IfmCoefficients)
        }
        #     }
      }
      BIC_i = -2*(loglike) + nPar*log(n)
      if((BIC_i < BIC) & is.finite(BIC_i)){
        BIC = BIC_i
        mvdc_ = mvdc_i
        bestCopulaDist = copulaDist
      }
    }
    ret = list(bestMvdc= mvdc_, copulaDist = bestCopulaDist)
    return(ret)
  }
  mvdc = getOptimalMvdc(Errors, CDFs, margins, paramMargins)
  modellingTime = Sys.time() - modellingTime#time in seconds

  uCombined = numeric(length(targets))
  all.data = cbind(targets, forecasts)
  bounds = c(min(all.data, na.rm = TRUE), max(all.data, na.rm = TRUE))
  getCBForecast = function(mvdc, forecasts_t, bounds){
    bestEstimate = NA
    # if(length(which(is.na(forecasts_t)))>0){
    #   return (bestEstimate)
    # }
    # getOptimumViaEvenlySpacedPoints = function(){
      nPoints = 1000
      x = seq(from=bounds[1], to=bounds[2],length.out=nPoints)
      nModels = length(forecasts_t)
      correctedEst = NULL
      for(i in 1:nModels){
        modelLabel_i = names(forecasts_t)[i]#modelLabels[i]
        errors = x - rep(forecasts_t[[modelLabel_i]], nPoints)
        # errors = x - rep(forecasts_t[[i]], nPoints)
        correctedEst = cbind(correctedEst, errors)
      }
      dimnames(correctedEst) = list(round(x, 4), modelLabels)
      # dimnames(correctedEst) = list(round(x, 4), colnames(forecasts_t))
      #maybe, there is a problem with my call to dMvdc(.)....
      fx = dMvdc(correctedEst, mvdc, log=TRUE)#;plot(x, fx, type="l")
      s = which.max(fx)
      bestEstimate = x[s]
      # return(bestEstimate)
    # }
    # bestEstimate = getOptimumViaEvenlySpacedPoints()
    return(bestEstimate)
  }
  n_v_m = length(targets)
  forecastingTime = Sys.time()
  for(t in 1:(n_v_m)){
    uCombined[t] = getCBForecast(mvdc$bestMvdc, forecasts_t= forecasts[t,], bounds)
  }
  forecastingTime = Sys.time() - forecastingTime

  modellingTime = as.numeric(modellingTime)
  forecastingTime = as.numeric(forecastingTime)

  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = forecastingTime
  optimal$model = list(MVDC = mvdc$bestMvdc, MPDs = MPDs, combinedModelsNames = modelLabels)
  optimal$forecasts = uCombined
  optimal$modelingBounds = bounds
  optimal$getCBForecast = getCBForecast
  return(optimal)
}

get_cCbNewForecasts = function(ModelsObjs, singleForecasts) {
  # ModelsObjs <- mod.ccp
  # singleForecasts <- cbind(fct.arima,fct.ets)
  opt_cCb = ModelsObjs
  # cANN one-step forecast  (fixed model and updating time series)
  # forecastingTime = proc.time()[[3]]
  newDataMatrix = singleForecasts
  # series.norm = getNormalizedSeries(series = newDataMatrix
  #                                   , min = ModelsObjs$superMinimum
  #                                   , max = ModelsObjs$superMaximum)
  forecastingTime = Sys.time()

  # forecasts.series.norm = opt_cCb$getCBForecast(
  #   mvdc = opt_cCb$model$MVDC, forecasts_t=newDataMatrix
  #   , bounds = opt_cCb$modelingBounds)

  forecasts.series.norm <- c()
  for(i in 1:nrow(newDataMatrix)) {
    forecasts.series.norm[i] <- opt_cCb$getCBForecast(
        mvdc = opt_cCb$model$MVDC, forecasts_t=newDataMatrix[i,]
        , bounds = opt_cCb$modelingBounds)
  }

  # forecasts = getDenormalizedSeries(dataset.norm = forecasts.series.norm
  #                                   , min = ModelsObjs$superMinimum
  #                                   , max = ModelsObjs$superMaximum)
  forecastingTime = Sys.time() - forecastingTime
  # forecastsData = cbind(new_u = forecasts, newDataMatrix)
  # forecastsData = as.data.frame(t(forecastsData))#;View(forecastsData)

  ###
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = forecasts.series.norm
  # ret$forecastsData = forecastsData
  return (ret)
}

#' Diagnosis of a Normal model according to different hypothesis tests
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Norm_test <-function(x){
  t1 <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x)) # KS
  t2 <- lillie.test(x)                     # Lilliefors
  t3 <- shapiro.test(x)                    # Shapiro-Wilk
  t4 <- ad.test(x)                         # Anderson-Darling
  testes <- c(t1$method, t2$method, t3$method, t4$method)
  valorp <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value)
  resultados <- cbind(valorp)
  rownames(resultados) <- testes
  print(resultados, digits = 4)
}

#' QQ-plot with enveloping
#'
#' @param x
#' @param conf
#' @param seed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
qqplot_env <- function(x, conf = 0.95, seed = 1, ...){      ## conf = Coeficiente de confian?a
  par(cex.axis = 0.6)
  set.seed(seed)
  n <- length(x)
  nsim <- 100                         ## N?mero de simula??es
  dadossim <- matrix(rnorm(n*nsim, mean = mean(x), sd = sd(x)), nrow = n)
  dadossim <- apply(dadossim,2,sort)
  infsup <- apply(dadossim,1,quantile, probs = c((1 - conf) / 2,(1 + conf) / 2))
  xbsim <- rowMeans(dadossim)
  faixay <- range(x, dadossim)
  qq0 <- qqnorm(
    x,
    # xlab = "Quantis teóricos N(0,1)", ylab = "Quantis amostrais",
    pch = 20, ylim = faixay, main="", ...)
  eixox <- sort(qq0$x)
  lines(eixox, xbsim, lwd=1)
  lines(eixox, infsup[1,], col = "red")
  lines(eixox, infsup[2,], col = "red")
}

#' Run a complete diagnosis of the residuals in a data frame.
#' For each column, the corresponding residuals are investigated under
#' different models, such as Normal, Laplace, Cauchy and Skew-normal.
#' Optionally, it plots a pair plots with histogram of redsiduals
#'
#' @param x
#' @param plot
#' @param path
#'
#' @return
#' @export
#'
#' @examples
getMPD_ResidualCheck <- function(x, plot = TRUE, path) {
  nc <- ncol(x)
  fontes <- 0.8

  if (plot) {
    jpeg(filename = path,
         height = 15, width = 15, units = 'cm',
         res=300)
    par(mfrow = c(nc,nc),
        mar = c(0.5,0.5,0.5,0.5),
        mgp = c(2,0.05,0),
        lwd = fontes,
        tck = -0.015,  cex.axis = 0.6,
        oma = c(0.3, 0.3, 0.3, 0.3) + 0.2)
  }

  mpd <- list()
  for (i in 1:nc) {
    ts.target <- x[ ,i]
    df.pred <- x;
    df.pred[ ,i] <- NA
    df.res <- data.frame(df.pred - ts.target)

    nr <- ncol(df.res)
    for (j in 1:nr) {
      if (j == i) {
        plot(1, type = "n", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1), xaxt = 'n', yaxt = 'n')
        text(0.5, 0.65, colnames(x)[i], pos = 1)

      } else {
        e <- df.res[ ,j]

        # get marginal probability density via information criterion (BIC)
        empd <- getMPD_viaInformationCriterion(e)
        mpd[[colnames(x)[i]]][[colnames(df.res)[j]]] <- empd

        if (j < i) {
          # plot(NULL, type = "n", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1), xaxt = 'n', yaxt = 'n', bty = 'n')

          # plot MPD and BIC
          qqplot_env(e, conf = .99, lwd = fontes)
          # plot(1, type = "n", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1), xaxt = 'n', yaxt = 'n')
          # text(0.5, 0.75, empd$margin, cex = fontes, pos = 1)
          # text(0.5, 0.65,
          #      paste0("(",paste(
          #        unlist(format(round(as.numeric(empd$parEstimates), 2), nsmall = 2)), collapse = ", "),")"),
          #      cex = fontes-0.1, pos = 1)
          # text(0.5, 0.55,
          #      paste0("BIC = ", format(round(as.numeric(empd$BIC), 2), nsmall = 2)),
          #      cex = fontes-0.1, pos = 1)

        } else if (j > i) {
          # theorical density plot
          quantiles <- seq(min(e), max(e), length = 100)
          f <- empd$pdf(quantiles)

          # plot histogram
          he <- hist(e, plot = FALSE)
          hist(e, prob = TRUE, ylim = c(0,(max(he$density)+max(he$density)*0.25)),
               main = '', xlab = '', ylab = '', lwd = fontes)

          param <- paste0("(",paste(unlist(format(round(as.numeric(empd$parEstimates), 1), nsmall = 1)), collapse = ", "),")")
          legend("topright",
                 legend = c(empd$margin,
                            format(round(as.numeric(empd$parEstimates[1]), 1), nsmall = 1),
                            format(round(as.numeric(empd$parEstimates[2]), 1), nsmall = 1)),
                 cex = fontes-0.1, bty = "n", yjust = 0.5)
          # text("topright", empd$margin, cex = fontes, line = 0)
          # text("topright",
          #       paste0("(",paste(
          #         unlist(format(round(as.numeric(empd$parEstimates), 2), nsmall = 2)), collapse = ", "),")"),
          #       cex = fontes-0.1, line = 0)

          box(lwd = fontes)

          # plot line
          lines(quantiles, f, col = "red", lwd = fontes+0.1)
        }
      }
    }
  }
  if(plot) {
    dev.off()
  }

  return(mpd)
}

#' Diagnosis of a Normal model fitted to residuals of a data frame.
#' For each column, the corresponding residuals are fitted to Normal
#' model and a dignosis analysis is performed
#'
#' @param x data frame
#'
#' @return
#' @export
#'
#' @examples
getMPD_GaussianResidualCheck <- function(x) {
  require(moments)
  nc <- ncol(x); mpd <- list(); ls.bic <- list(); ls.skw <- list(); ls.kur <- list(); ls.qqn <- list(); ls.bics <- list()
  for (i in 1:nc) {
    ts.target <- x[ ,i]
    df.pred <- x;
    df.pred[ ,i] <- NA
    df.res <- data.frame(df.pred - ts.target)

    nr <- ncol(df.res)
    r.bic <- NULL; r.skw <- NULL; r.kur <- NULL; r.qqn <- NULL
    for (j in 1:nr) {
      if(j!=i) {
        e <- df.res[ ,j]

        # get gaussian marginal probability density via information criterion (BIC)
        empd <- getMPD_Gaussian(e)
        mpd[[colnames(x)[i]]][[colnames(df.res)[j]]] <- empd

        r.bic <- c(r.bic, empd$BIC)
        r.skw <- c(r.skw, moments::skewness(abs(e)))
        r.kur <- c(r.kur, moments::kurtosis(abs(e)))

        qq <- qqnorm(e, plot = FALSE)
        r.qqn <- c(r.qqn, summary(lm(y~x,qq))$r.squared)

        # get MPD via BIC
        ls.bics[[paste0(i)]][[paste0(j)]] <- getMPD_viaInformationCriterion(e)$trash
      }
    }
    ls.bic[[colnames(x)[i]]] <- r.bic
    ls.skw[[colnames(x)[i]]] <- r.skw
    ls.kur[[colnames(x)[i]]] <- r.kur
    ls.qqn[[colnames(x)[i]]] <- r.qqn
  }
  return(list(MOD=mpd,BIC=ls.bic,Skewness=ls.skw,Kurtosis=ls.kur,"R2.QQ-plot"=ls.qqn,BICs=ls.bics))
}

#' Diagnosis of a Normal model fitted to a giver time series
#'
#' @param e
#'
#' @return
#' @export
#'
#' @examples
getMPD_Gaussian <- function(e) {
  ret = list()
  ret$residuals = e
  #e = e[!is.na(e)]
  n = length(e); k = log(n)

  margins = NULL; logLiks = NULL; nPars = NULL; BICs = NULL; parsEstimates = list(); i=0

  #SYMMETRICAL PDFs
  margins = c(margins, "norm")
  i=i+1; parsEstimates[[i]] = list(mean=mean(e, na.rm = TRUE), sd = sd(e, na.rm = TRUE))
  logLiks = c(logLiks, sum(dnorm(x = e, mean = parsEstimates[[i]]$mean, sd = parsEstimates[[i]]$sd, log=TRUE), na.rm = TRUE))

  # TAKING THE BEST BIC FIT
  minBIC = Inf; minBIC_index = NULL; nFits = length(margins)
  for(i in 1:nFits){
    nPars[i] = length(parsEstimates[[i]])
    BICs[i] = -2*logLiks[i] + k*nPars[i]
    if(BICs[i] < minBIC){
      minBIC = BICs[i]; minBIC_index = i
    }
  }
  ret$nPars = nPars[minBIC_index]
  ret$BIC = BICs[minBIC_index]
  ret$margin = margins[minBIC_index]
  ret$parEstimates = parsEstimates[[minBIC_index]]
  ret$loglik = logLiks[minBIC_index]

  #TAKING THE BEST BIC FIT CDF RESIDUALS POINTS
  if(ret$margin=="norm"){
    ret$CDF = function(x){return(pnorm(x, mean=ret$parEstimates[[1]], sd=ret$parEstimates[[2]]))}
    ret$pdf = function(x){return(dnorm(x = x, mean = ret$parEstimates[[1]], sd = ret$parEstimates[[2]]))}
  }

  return(ret)
}

#' Apply a given function pairwise in a data frame
#'
#' @param data data frame
#' @param fun string name of a function to be called
#'
#' @return
#' @export
#'
#' @examples
pairwise_function <- function(data, fun) {
  return(sapply(data, function(x) {
    sapply(data, function(y) {
      do.call(fun, args = list(x,y))
    })
  }))
}

#' Sequential Multivariate Regression Imputation
#'
#' @param x: data frame
#' @param K: number of iterations
#'
#' @return a complete data frame obtained after imputation
na_sequential <- function(x, K = 5) {
  # data("sleep")
  # x <- sleep; K <- 5

  # mod <- match.arg(model)

  # order from less to higher number of missing values
  ord <- sort(na_perc(x))

  # U matrix: only complete variables
  U <- x[names(which(ord==0))]
  U_loop <- U

  # P matrix: only variables with missing values
  P <- x[names(which(ord!=0))]

  for (j in 1:K) {
    # j <- 1
    for (i in names(P)) {
      # i <- "Span"
      # get response variable
      y <- P[[i]]

      # missing values indexes
      na_index <- is.na(x[[i]])
      # sum(na_index) == as.numeric(ord[[i]])

      # UP matrix
      if (j == 1) {
        UP <- as.data.frame(cbind(y, U_loop))
        colnames(UP) <- c(i, colnames(U_loop))
      } else {
        UP <- U_loop

        # virtually remove values: MV receives a incomplete data.frame
        # Equivalent to complete case UP_cc on original algorithm
        UP[na_index, c(i)] <- NA
      }

      # complete case matrix
      # UP_cc <- UP[!na_index, ]
      # nrow(UP)-nrow(UP_cc) == sum(na_index)
      # UP_cc <- UP[!na_index, ]

      # Select model
      print(head(UP))
      mod <- na_mv(UP)

      # model y ~ .
      # form <- as.formula(paste0(as.character(i)," ~ ."))
      # mod <- lm(form, data = UP_cc)

      # predict
      # pred <- predict(mod, newdata = UP)

      # imputation on Y
      y[na_index] <- mod$imp
      # y[na_index] <- pred[na_index]

      # update U matrix for next iteration
      if (j == 1) {
        U_loop <- as.data.frame(cbind(y, U_loop))
        colnames(U_loop) <- colnames(UP)
      } else {
        U_loop[i] <- y
      }
    }
  }

  U_loop <- U_loop[ , colnames(x)] # reorder columns
  return(U_loop)
}

#' Multivariate imputation by Minimal Variance (MV) method
#'
#' @param x data frame with one target station to be imputed
#'
#' @return complete data frame
#' @export
#'
#' @examples
na_mv <- function(x, pos.only = TRUE) {

  # column with missing data
  col <- which(na_perc(x)>0)

  indexes.tre <- !is.na(x[, col]) # index for training: complete case
  indexes.tes <- is.na(x[, col]) # index for test: rows with missing values

  df.tre <- x[indexes.tre,] # training subset: complete case
  df.tes <- x[indexes.tes,] # test subset: rows with missing values

  # target
  ts.tre <- df.tre[,col]
  ts.tes <- df.tes[,col]

  # predictor (stations): other columns except target
  pred.tre <- df.tre[,-col]
  pred.tes <- df.tes[,-col]

  # deviation
  df.res <- ts.tre - pred.tre

  # minimal variance model
  mv.obj <- getMinimalVariancePredictor(df.res, pred.tre, ts.tre, 1, nrow(pred.tre))
  mv.pred <- get_cMvNewForecasts(ModelsObjs = mv.obj, singleForecasts = pred.tes)

  # imputed data.frame
  mv.imp <- mv.pred$forecasts

  if (pos.only) {
    mv.imp[mv.imp < 0] <- 0
  }

  x[indexes.tes, col] <- mv.imp

  return(list("x" = x, "imp" = mv.imp, "model" = mv.obj))
}

#' Imputation by Minimal Variance (MV) method optimized by selecting
#' best covariates by minimization of BIC
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
opt_na_mv <- function(x) {
  # data <- df.pcp
  # data[30:75,2] <- NA
  # x <- data

  # column with missing data
  col <- which(na_perc(x)!=0)
  nm.series <- names(col)

  # best estimators
  x_pair <- cor(x, use = "p")
  bst <- sort(x_pair[,colnames(x_pair) == nm.series], decreasing = T)[-1]

  min.res <- +Inf
  opt.mv.obj <- NULL
  opt.bic <- NULL
  for (i in 2:length(bst)) {
    # i <- 2
    idx.k <- which(colnames(x) %in% names(bst[1:i]))
    x.k <- x[ ,c(col,idx.k)]

    mv.k <- na_mv(x = x.k)

    bic.k <- my_BIC(mv.k$model)

    if (bic.k < min.res) {
      min.res <- bic.k
      opt.mv.obj <- mv.k
      opt.bic <- bic.k
    }
  }
  return(opt.mv.obj)
}

#' Imputation by Minimal Variance (MV) with STL decomposition - FAIL
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
na_mv_stl <- function(data) {
  require(stlplus)
  # data <- df.pcp
  # data[30:75,2] <- NA

  x <- data

  # column with missing data
  col <- which(apply(apply(x, 2, is.na), 2, sum)>0)
  nm.series <- colnames(x)[col]

  x.stl <- apply(x, 2, stlplus::stlplus, n.p = 12, s.window = "periodic")
  x.rem <- data.frame(simplify2array(lapply(x.stl, stlplus::remainder)))
  x.tre <- data.frame(simplify2array(lapply(x.stl, stlplus::trend)))
  x.sea <- data.frame(simplify2array(lapply(x.stl, stlplus::seasonal)))
  x.cmp <- x.tre+x.sea

  indexes.tre <- !is.na(x[, col]) # index for training
  indexes.tes <- is.na(x[, col]) # index for test

  df.tre <- x[indexes.tre,] # training subset
  df.tes <- x[indexes.tes,] # test subset

  df.tre.rem <- x.rem[indexes.tre,] # training subset remainder
  df.tes.rem <- x.rem[indexes.tes,] # test subset remainder

  # target
  ts.tre <- df.tre[,col]
  ts.tes <- df.tes[,col]

  ts.tre.rem <- df.tre.rem[,col]
  ts.tes.rem <- df.tes.rem[,col]

  # predictor (stations): other columns except target
  pred.tre <- df.tre[,-col]
  pred.tes <- df.tes[,-col]

  pred.tre.rem <- df.tre.rem[,-col]
  pred.tes.rem <- df.tes.rem[,-col]

  # deviation
  df.res <- ts.tre - pred.tre

  # minimal variance model
  mv.obj <- getMinimalVariancePredictor(pred.tre.rem, pred.tre, 1, nrow(pred.tre))
  mv.pred <- get_cMvNewForecasts(ModelsObjs = mv.obj,
                                 singleForecasts = pred.tes)
  # imputed data.frame
  x[indexes.tes, col] <- mv.pred$forecasts

  return(x)
}

#' Total of missing values in each column of a data frame
#'
#' @param x data frame with missing values
#'
#' @return
#' @export
#'
#' @examples
na_count <- function(x) {
  return(apply(apply(x, 2, is.na), 2, sum))
}

#' Percentage of missing values in a time series
#'
#' @param x ts with missing values
#'
#' @return
#' @export
#'
#' @examples
na_perc.default <- function(x) {
  return(sum(is.na(x))/length(x))
}
na_perc <- function(x) {
  return(if (match(class(x), c("data.frame"))) {
    apply(x, 2, na_perc.default)
  } else {
    na_perc.default(x)
  })
}

#' Function for lag matrix from a time series
#'
#' @param x ts object
#' @param lags vector with index of lags to be computed. Use positive
#' (negative) values for get past (future) lags
#'
#' @return data frame
#' @export
#'
#' @examples
lag_matrix2 <- function(x, lags) {
  cll <- paste0("cbind(x, ", paste0("stats::lag(x, -", lags,")", collapse = ", "), ")")
  df <- eval(parse(text = cll))
  colnames(df) <- c("x", paste0("x.",lags))
  return(df)
}

compose_lags_combn <- function(lags.max) {
  require("arrangements")
  lags.option <- list()
  t1 <- Sys.time()
  for(i in 1:lags.max) {
    # lags.option[[i]] <- t(combn(lags.max, i))
    lags.option[[i]] <- arrangements::combinations(lags.max, i)
  }
  t2 <- Sys.time()
  lags.option <- lapply(lags.option, function(k) split(k, seq(nrow(k))))
  lags.option <- unlist(lags.option, recursive = FALSE)

  # Slower computation time, equals to sum(choose(lags.max, i))
  # lags.tot <- lags.tot + choose(lags.max, i)
  lags.tot <- length(lags.option)
  return(list(lags.option = lags.option, lags.tot = lags.tot,
              elapsed.time = (t2-t1)))
}

compose_lags <- function(y, p, P) {
  m <- max(round(frequency(y)), 1L)
  if (P > 0) {
    lags <- sort(unique(c(1:p, m * (1:P))))
  }
  else {
    lags <- 1:p
  }
  return(lags)
}
