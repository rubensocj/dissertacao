# 15/03/2022
ga_SVR <- function(y, p, P, gamma = c(1e-03, 1e-00),
                   cost = c(1e-00, 1e+03), epsilon = c(1e-03, 1e-01), training,
                   xreg = NULL, ...) {

  opt.SVR <<- list()
  opt.SVR$mse <<- Inf

  # GA fitness function
  ga_SVR_fitness <- function(par, y, training, xreg) {

    # Selects lag candidates
    lags <- compose_lags(y, round(par[1]), round(par[2]))

    # SVR model
    mod <- GWL_ts_model(
      y = y,
      type = "svm",
      lags = lags,
      gamma = par[3],
      cost = par[4],
      epsilon = par[5],
      from = 1,
      to = training
    )

    # Validation
    prev <- GWL_ts_forecast(mod, h = length(mod$validation$y))$forecasts
    mse.SVR <- mean((mod$validation$y - prev)^2)
    # ts.plot(cbind(prev,mod$validation$y), ylim = rev(range(y)),
    #         col = c('red','black'))

    if(mse.SVR < opt.SVR$mse){
      opt.SVR$mse <<- mse.SVR
      opt.SVR$model <<- mod
      opt.SVR$p <<- round(par[1])
      opt.SVR$P <<- round(par[2])
      opt.SVR$gamma <<- par[3]
      opt.SVR$cost <<- par[4]
      opt.SVR$epsilon <<- par[5]
    }

    # MSE
    return(-mse.SVR)
  }

  # GA optimization
  t1 <- Sys.time()
  expr = ga.svr <- GA::ga(
    type = "real-valued",
    fitness = ga_SVR_fitness, y, training, xreg,
    lower = c(1, 1, gamma[1], cost[1], epsilon[1]),
    upper = c(p, P, gamma[2], cost[2], epsilon[2]),
    names = c('p', 'P', 'gamma', 'cost', 'epsilon'),
    ...
  )
  duration <- as.numeric(Sys.time() - t1, units = "secs")

  # Update the y object in the optimum model
  opt.SVR$model$training <<- y
  opt.SVR$validation <<- c()
  opt.SVR$ga <<- ga.svr
  opt.SVR$fitted <<- opt.SVR$model$fitted
  opt.SVR$optimization.time <<- duration

  return(opt.SVR)
}

# Combination with SVR
ga_cSVR <- function(y, gamma = c(1e-03, 1e-00),
                    cost = c(1e-00, 1e+03), epsilon = c(1e-03, 1e-01),
                    training, xreg = NULL, ...) {

  opt.cSVR <<- list()
  opt.cSVR$mse <<- Inf

  # GA fitness function
  ga_cSVR_fitness <- function(par, y, training, xreg) {

    # SVR model
    mod <- GWL_ts_mixed_model(
      y = y,
      type = "svm",
      gamma = par[1],
      cost = par[2],
      epsilon = par[3],
      xreg = xreg,
      from = 1,
      to = training
    )

    # Validation
    prev <- stats::predict(mod$model, newdata = mod$validation)
    mse.cSVR <- mean((mod$validation$y - prev)^2)

    if(mse.cSVR < opt.cSVR$mse){
      opt.cSVR$mse <<- mse.cSVR
      opt.cSVR$model <<- mod
      opt.cSVR$gamma <<- par[1]
      opt.cSVR$cost <<- par[2]
      opt.cSVR$epsilon <<- par[3]
    }

    # MSE
    return(-mse.cSVR)
    # return(-mean((mod$validation$y - prev)^2))
  }

  # GA optimization
  t1 <- Sys.time()
  ga.csvr <- GA::ga(
    type = "real-valued",
    fitness = ga_cSVR_fitness, y, training, xreg,
    lower = c(gamma[1], cost[1], epsilon[1]),
    upper = c(gamma[2], cost[2], epsilon[2]),
    names = c('gamma', 'cost', 'epsilon'),
    ...
  )
  duration <- as.numeric(Sys.time() - t1, units = "secs")

  # Update the y object in the optimum model
  opt.cSVR$ga <<- ga.csvr
  opt.cSVR$fitted <<- opt.cSVR$model$fitted
  opt.cSVR$optimization.time <<- duration

  return(opt.cSVR)
}

# NEURAL NETWORK
ga_ANN <- function(y, p, P, hid.max, layer.max, training,
                  # learningrate = c(1e-06, 1e-04),
                  xreg = NULL, ...) {

  if(length(y)==training) {stop('Training subset size can not be equals to the length of y.')}

  opt.ANN <<- list()
  opt.ANN$mse <<- Inf

  # GA fitness function
  ga_ANN_fitness <- function(par, y, training, act.fun.labels, xreg){

    # Selects lag candidates
    lags <- compose_lags(y, round(par[1]), round(par[2]))

    # Compose model array
    hidden <- rep(round(par[3]), round(par[4]))

    # Randomly selects one of the learning algorithms
    # alg <- alg.labels[round(par[5])]
    alg <- 'rprop+'

    # Learning rate used for traditional backpropagation
    # if(alg == "backprop") {
    #   lea <- par[6]
    # } else {
    #   lea <- NULL
    # }
    lea <- NULL

    # Randomly selects one of the activation function candidates
    afu <- act.fun.labels[round(par[5])]

    # NEURALNET model
    mod <- GWL_ts_model(
      y = y,
      type = 'neuralnet',
      lags = lags,
      hidden = hidden,
      act.fun = afu,
      algorithm = alg,
      learningrate = lea,
      from = 1,
      to = training)

    # Validation
    prev <- GWL_ts_forecast(mod, h = length(mod$validation$y))$forecasts
    mse.ANN <- mean((mod$validation$y - prev)^2)

    if(mse.ANN < opt.ANN$mse){
      opt.ANN$mse <<- mse.ANN
      opt.ANN$model <<- mod
      opt.ANN$p <<- round(par[1])
      opt.ANN$P <<- round(par[2])
      opt.ANN$hidden <<- hidden
      opt.ANN$actfun <<- afu
      opt.ANN$algorithm <<- alg
      opt.ANN$learningrate <<- lea
    }

    # MSE
    return(-mse.ANN)
  }

  # alg.labels = c('backprop', 'rprop+', 'rprop-', 'sag', 'slr')
  act.fun.labels = c('logistic','tanh')

  # GA optimization
  t1 <- Sys.time()
  ga.ann <- GA::ga(
    type = "real-valued",
    fitness = ga_ANN_fitness, y, training, act.fun.labels, xreg,
    lower = c(1, 1, 1, 1, 1),
    upper = c(p, P, hid.max, layer.max, length(act.fun.labels)),
    names = c('p', 'P', 'nodes', 'layers',
              # 'algorithm', 'learningrate',
              'act.fun'),
    ...
  )
  duration <- as.numeric(Sys.time() - t1, units = 'secs')

  # Update the y object in the optimum model
  opt.ANN$model$training <<- y
  opt.ANN$validation <<- c()
  opt.ANN$ga <<- ga.ann
  opt.ANN$fitted <<- opt.ANN$model$fitted
  opt.ANN$optimization.time <<- duration

  return(opt.ANN)
}

# NEURAL NETWORK
ga_cANN <- function(y, hid.max, layer.max, training,
                   # learningrate = c(1e-06, 1e-04),
                   xreg = NULL, ...) {

  opt.cANN <<- list()
  opt.cANN$mse <<- Inf

  # GA fitness function
  ga_cANN_fitness <- function(par, y, training, act.fun.labels, xreg) {

    # Compose model array
    hidden <- rep(round(par[1]), round(par[2]))

    # Randomly selects one of the learning algorithms
    # alg <- alg.labels[round(par[3])]
    alg <- 'rprop+'

    # Learning rate used for traditional backpropagation
    # if(alg == "backprop") {
    #   lea <- par[4]
    # } else {
    #   lea <- NULL
    # }
    lea <- NULL

    # Randomly selects one of the activation function candidates
    afu <- act.fun.labels[round(par[3])]

    # NEURALNET model
    mod <- GWL_ts_mixed_model(
      y = y,
      type = "neuralnet",
      hidden = hidden,
      act.fun = afu,
      algorithm = alg,
      learningrate = lea,
      xreg = xreg,
      from = 1,
      to = training)

    # Validation
    prev <- stats::predict(mod$model, newdata = mod$validation)
    mse.cANN <- mean((mod$validation$y - prev)^2)

    if(mse.cANN < opt.cANN$mse){
      opt.cANN$mse <<- mse.cANN
      opt.cANN$model <<- mod
      opt.cANN$hidden <<- hidden
      opt.cANN$actfun <<- afu
      opt.cANN$algorithm <<- alg
      opt.cANN$learningrate <<- lea
    }

    # MSE
    return(-mse.cANN)
  }

  # alg.labels = c('backprop', 'rprop+', 'rprop-', 'sag', 'slr')
  act.fun.labels = c('logistic','tanh')

  # GA optimization
  t1 <- Sys.time()
  ga.cann <- GA::ga(
    type = "real-valued",
    fitness = ga_cANN_fitness, y, training, act.fun.labels, xreg,
    lower = c(1, 1, 1),
    upper = c(hid.max, layer.max, length(act.fun.labels)),
    names = c('nodes', 'layers', 'act.fun'),
    ...
  )
  duration <- as.numeric(Sys.time() - t1, units = "secs")

  # Update the y object in the optimum model
  opt.cANN$ga <<- ga.cann
  opt.cANN$fitted <<- opt.cANN$model$fitted
  opt.cANN$optimization.time <<- duration

  return(opt.cANN)
}

# ELM
ga_ELM <- function(y, p, P, hid.max = 50, training, xreg = NULL, ...) {

  opt.ELM <<- list()
  opt.ELM$mse <<- Inf

  # GA fitness function
  ga_ELM_fitness <- function(par, y, act.fun.labels, training, xreg) {

    # Selects lag candidates
    lags <- compose_lags(y, round(par[1]), round(par[2]))

    # Randomly selects one of the activation function candidates
    afu <- act.fun.labels[round(par[4])]

    # ELM model
    mod <- GWL_ts_model(
      y = y,
      type = "elm",
      lags = lags,
      hidden = round(par[3]),
      act.fun = afu,
      from = 1,
      to = training)

    # Validation
    prev <- GWL_ts_forecast(mod, h = length(mod$validation$y))$forecasts
    mse.ELM <- mean((mod$validation$y - prev)^2)

    if(mse.ELM < opt.ELM$mse){
      opt.ELM$mse <<- mse.ELM
      opt.ELM$model <<- mod
      opt.ELM$p <<- round(par[1])
      opt.ELM$P <<- round(par[2])
      opt.ELM$hidden <<- round(par[3])
      opt.ELM$actfun <<- afu
    }

    # MSE
    return(-mse.ELM)
  }

  # Compose activation function label candidates
  act.fun.labels <- c('sig','sin','radbas','hardlim','hardlims','satlins',
                      'tansig','tribas','relu','purelin')
  # act.fun.labels <- c('sig','sin','radbas','tansig')
  # act.fun.labels <- c('sig','tansig')

  # GA optimization
  t1 <- Sys.time()
  ga.elm <- GA::ga(
    type = "real-valued",
    fitness = ga_ELM_fitness, y, act.fun.labels, training, xreg,
    lower = c(1, 1, 1, 1),
    upper = c(p, P, hid.max, length(act.fun.labels)),
    names = c('p', 'P', 'hidden', 'act.fun'),
    ...
  )
  duration <- as.numeric(Sys.time() - t1, units = "secs")

  # Update the y object in the optimum model
  opt.ELM$model$training <<- y
  opt.ELM$validation <<- c()
  opt.ELM$ga <<- ga.elm
  opt.ELM$fitted <<- opt.ELM$model$fitted
  opt.ELM$optimization.time <<- duration

  return(opt.ELM)
}

# LSTM
ga_LSTM <- function(y, p, P, unit.max, training, epochs.max,
                    learningrate = c(1e-06, 1e-01), xreg = NULL, ...) {

  if(length(y)==training) {stop('Training subset size can not be equals to the length of y.')}

  opt.LSTM <<- list()
  opt.LSTM$mse <<- Inf

  # GA fitness function
  ga_LSTM_fitness <- function(par, y, training, xreg) {

    # Selects lag candidates
    lags <- compose_lags(y, round(par[1]), round(par[2]))

    # Compose model array
    # hidden <- rep(round(par[3]), round(par[4]))
    hidden <- round(par[3])

    # Learning rate for ADAM optimizer
    lea <- par[4]

    # Number of Epochs
    epo <- round(par[5])

    # LSTM model
    mod <- GWL_ts_model(
      y = y,
      type = 'lstm',
      lags = lags,
      hidden = hidden,
      epochs = epo,
      batch_size = 1,
      learningrate = lea,
      from = 1,
      to = training)

    # Validation
    prev <- GWL_ts_forecast(mod, h = length(mod$validation$y))$forecasts
    mse.LSTM <- mean((mod$validation$y - prev)^2)

    cat(paste0('LSTM: ', round(mod$training.time, 3), 's, ',
               round(mse.LSTM,6),'\n'))

    if(mse.LSTM < opt.LSTM$mse){
      opt.LSTM$mse <<- mse.LSTM
      opt.LSTM$model <<- mod
      opt.LSTM$p <<- round(par[1])
      opt.LSTM$P <<- round(par[2])
      opt.LSTM$hidden <<- hidden
      opt.LSTM$learningrate <<- lea
      opt.LSTM$epochs <<- epo
    }

    # MSE
    return(-mse.LSTM)
  }

  # GA optimization
  t1 <- Sys.time()
  ga.lstm <- GA::ga(
    type = "real-valued",
    fitness = ga_LSTM_fitness, y, training, xreg,
    lower = c(1, 1, 1, learningrate[1], 1),
    # upper = c(p, P, unit.max, layer.max, learningrate[2], epochs.max),
    upper = c(p, P, unit.max, learningrate[2], epochs.max),
    names = c('p', 'P', 'units', 'learningrate', 'epochs'),
    ...
  )
  duration <- as.numeric(Sys.time() - t1, units = "secs")

  # Update the y object in the optimum model
  opt.LSTM$model$training <<- y
  opt.LSTM$validation <<- c()
  opt.LSTM$ga <<- ga.lstm
  opt.LSTM$fitted <<- opt.LSTM$model$fitted
  opt.LSTM$optimization.time <<- duration

  return(opt.LSTM)
}

# GRU
ga_GRU <- function(y, p, P, unit.max, training, epochs.max,
                    learningrate = c(1e-06, 1e-01), xreg = NULL, ...) {

  if(length(y)==training) {stop('Training subset size can not be equals to the length of y.')}

  opt.GRU <<- list()
  opt.GRU$mse <<- Inf

  # GA fitness function
  ga_GRU_fitness <- function(par, y, training, xreg) {

    # Selects lag candidates
    lags <- compose_lags(y, round(par[1]), round(par[2]))

    # Compose model array
    hidden <- round(par[3])

    # Learning rate for ADAM optimizer
    lea <- par[4]

    # Number of Epochs
    epo <- round(par[5])

    # GRU model
    mod <- GWL_ts_model(
      y = y,
      type = 'gru',
      lags = lags,
      hidden = hidden,
      epochs = epo,
      batch_size = 1,
      learningrate = lea,
      from = 1,
      to = training)

    # Validation
    prev <- GWL_ts_forecast(mod, h = length(mod$validation$y))$forecasts
    mse.GRU <- mean((mod$validation$y - prev)^2)

    cat(paste0('GRU: ', round(mod$training.time, 3), 's, ',
               round(mse.GRU,6),'\n'))

    if(mse.GRU < opt.GRU$mse){
      opt.GRU$mse <<- mse.GRU
      opt.GRU$model <<- mod
      opt.GRU$p <<- round(par[1])
      opt.GRU$P <<- round(par[2])
      opt.GRU$hidden <<- hidden
      opt.GRU$learningrate <<- lea
      opt.GRU$epochs <<- epo
    }

    # MSE
    return(-mse.GRU)
  }

  # GA optimization
  t1 <- Sys.time()
  ga.gru <- GA::ga(
    type = "real-valued",
    fitness = ga_GRU_fitness, y, training, xreg,
    lower = c(1, 1, 1, learningrate[1], 1),
    upper = c(p, P, unit.max, learningrate[2], epochs.max),
    names = c('p', 'P', 'units', 'learningrate', 'epochs'),
    ...
  )
  duration <- as.numeric(Sys.time() - t1, units = "secs")

  # Update the y object in the optimum model
  opt.GRU$model$training <<- y
  opt.GRU$validation <<- c()
  opt.GRU$ga <<- ga.gru
  opt.GRU$fitted <<- opt.GRU$model$fitted
  opt.GRU$optimization.time <<- duration

  return(opt.GRU)
}

GWL_ts_model <- function(y, type = c('arima','ets','elm','neuralnet','svm',
                                     'lstm','gru','mlp','nnet'),
                         lags = 1:frequency(y), hidden = NULL, act.fun = NULL,
                         units = NULL, epochs = NULL, batch_size = 1,
                         gamma = NULL, cost = NULL, epsilon = NULL,
                         algorithm = NULL, learningrate = NULL,
                         from = 1, to = length(y), xreg = NULL, ...) {

  df <- lag_matrix2(y, lags)
  colnames(df)[1] <- "y"
  # df <- data.frame(df)

  y.tra <- ts(y[from:to], frequency = frequency(y))

  df.tra <- data.frame(df[from:to, ])
  # df.tra <- df.tra[1:length(y), ]
  # if(!is.null(xreg)) df.tra <- data.frame(cbind(df.tra, xreg))
  df.tra <- na.omit(df.tra)

  if(to!=length(y)) {
    df.val <- data.frame(df[(to+1):length(y), ])
    y.val <- y[(to+1):length(y)]
  } else {
    df.val <- NULL
    y.val <- NULL
  }
  df <- data.frame(na.omit(df))

  history <- NULL

  # Match selected model
  type <- match.arg(type)

  # Chooses arguments according to model type
  switch (
    type,

    ## ARIMA
    arima = {
      t1 <- Sys.time()
      mod <- forecast::auto.arima(y = y, allowdrift = FALSE, ...)
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      pred <- mod$fitted
    },

    ## ETS
    ets = {
      t1 <- Sys.time()
      mod <- forecast::ets(y = y)
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      pred <- mod$fitted
    },

    ## Extreme Learning Machine
    elm = {
      t1 <- Sys.time()
      mod <- elmNNRcpp::elm(
        formula = y~.,
        data = df.tra,
        nhid = hidden,
        actfun = act.fun,
        ...
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      # Fitted (New forecasts)
      pred <- as.numeric(stats::predict(mod, newdata = df))
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred),
                 frequency = frequency(y), end = end(y))
    },

    ## Neural Network
    neuralnet = {
      t1 <- Sys.time()
      mod <- neuralnet::neuralnet(
        formula = y~.,
        data = df.tra,
        hidden = hidden,
        act.fct = act.fun,
        algorithm = algorithm,
        learningrate = learningrate,
        linear.output = TRUE,
        err.fct = 'sse',
        stepmax = 1e+06,
        threshold = 1e-02,
        ...
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      # Fitted (New forecasts)
      pred <- as.numeric(stats::predict(mod, newdata = df))
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred),
                 frequency = frequency(y), end = end(y))

    },

    ## Support Vector Machine
    svm = {
      t1 <- Sys.time()
      mod <- e1071::svm(
        formula = y~.,
        data = df.tra,
        # scale = TRUE,
        scale = FALSE,
        type = 'eps-regression',
        kernel = "radial",
        gamma = gamma,
        cost = cost,
        epsilon = epsilon,
        ...
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      # Fitted (New forecasts)
      pred <- stats::predict(mod, newdata = df)
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred),
                 frequency = frequency(y), end = end(y))

    },

    ## Gated recurrent unit
    gru = {
      y_train = as.matrix(df.tra[, 1])
      x_train = as.matrix(df.tra[, -1])

      # Train set
      dim(x_train) <- c(dim(x_train), 1)

      # LSTM architecture
      mod <- keras::keras_model_sequential()
      mod %>%
          keras::layer_gru(
            units = as.integer(hidden[1]),
            input_shape = c(dim(x_train)[-1]))
      mod %>% keras::layer_dense(units = 1)

      mod %>% keras::compile(
        loss = 'mse',
        optimizer = optimizer_adam(learning_rate = learningrate),
        metrics = 'mae'
      )

      # early_stopping <- callback_early_stopping(
      #   monitor = 'val_loss', patience = 5)
      early_stopping <- callback_early_stopping(
        monitor = 'val_loss', patience = 2)
      # funcionou na 20 monitor = 'loss' patience = 5, sem validacao

      # x_val <- as.matrix(df.val[,-1])
      # y_val <- as.matrix(df.val[,1])
      # dim(x_val) <- c(dim(x_val), 1)
      # dim(y_val) <- c(dim(y_val), 1)

      t1 <- Sys.time()
      history <- mod %>% keras::fit(
        x = x_train,
        y = y_train,
        # callbacks = c(early_stopping),
        # validation_split = 0.25,
        # validation_data = list(x_val, y_val),
        epochs = epochs,
        batch_size = batch_size,
        verbose = 0,
        shuffle = FALSE
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      # Fitted (New forecasts)
      pred <- mod %>% predict(as.matrix(df[, -1]))#, batch_size = 1)
      pred <- pred[ ,1]
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred),
                 frequency = frequency(y), end = end(y))
    },

    ## Long Short-Term Memory
    lstm = {
      y_train = as.matrix(df.tra[, 1])
      x_train = as.matrix(df.tra[, -1])

      # Train set
      dim(x_train) <- c(dim(x_train), 1)

      # LSTM architecture
      mod <- keras::keras_model_sequential()

      if(length(hidden) > 1) {
        mod %>%
          keras::layer_lstm(
            units = as.integer(hidden[1]),
            input_shape = c(dim(x_train)[-1]),
            return_sequences = TRUE
          )

        for (i in 2:length(hidden)) {
          if (i != length(hidden)) {
            mod %>% keras::layer_lstm(
              units = as.integer(hidden[i]),
              return_sequences = TRUE
            )
          } else {
            mod %>% keras::layer_lstm(
              units = as.integer(hidden[i])
            )
          }
        }
      } else {
        mod %>%
          keras::layer_lstm(
            units = as.integer(hidden[1]),
            input_shape = c(dim(x_train)[-1])
          )
      }
      mod %>% keras::layer_dense(units = 1)

      mod %>% keras::compile(
        loss = 'mse',
        optimizer = optimizer_adam(learning_rate = learningrate),
        metrics = 'mae'
      )

      # early_stopping <- callback_early_stopping(
      #   monitor = 'val_loss', patience = 5)
      early_stopping <- callback_early_stopping(
        monitor = 'val_loss', patience = 2)
      # funcionou na 20 monitor = 'loss' patience = 5, sem validacao

      # x_val <- as.matrix(df.val[,-1])
      # y_val <- as.matrix(df.val[,1])
      # dim(x_val) <- c(dim(x_val), 1)
      # dim(y_val) <- c(dim(y_val), 1)

      t1 <- Sys.time()
      history <- mod %>% keras::fit(
        x = x_train,
        y = y_train,
        # callbacks = c(early_stopping),
        # validation_split = 0.25,
        # validation_data = list(x_val, y_val),
        epochs = epochs,
        batch_size = batch_size,
        verbose = 0,
        shuffle = FALSE
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      # Fitted (New forecasts)
      pred <- mod %>% predict(as.matrix(df[, -1]))#, batch_size = 1)
      pred <- pred[ ,1]
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred),
                 frequency = frequency(y), end = end(y))
    },

    ## Keras multilayer perceptron
    mlp = {
      y_train = as.matrix(df.tra[, 1])
      x_train = as.matrix(df.tra[, -1])

      # Train set
      dim(x_train) <- c(nrow(x_train), ncol(x_train), 1)

      mod <- keras_model_sequential()
      mod %>%
        layer_dense(
          units = 10,
          batch_input_shape = c(1, dim(x_train)[2]),
          activation = act.fun) %>%
        layer_dense(1)

      mod %>% compile(
        loss = 'mean_squared_error',
        optimizer = 'adam',
        metrics = c('accuracy')
      )

      t1 <- Sys.time()
      mod %>% fit(
        x = x_train,
        y = y_train,
        epochs = epochs,
        verbose = 0
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      pred <- mod %>% predict(x_train)
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred))
      tsp(pred) <- tsp(y)
    },

    ## Hyndman's NNETAR
    nnet = {
      t1 <- Sys.time()
      mod <- forecast::nnetar(
        y = y,
        p = length(which(lags%%frequency(y)!=0)),
        P = length(which(lags%%frequency(y)==0)),
        size = hidden,
        scale.inputs = FALSE
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      pred <- mod$fitted
      # df.val <- as.data.frame(y.val); colnames(df.val) = c("y")
      df.val <- y.val
    }
  )

  # train.y <- ts(y[from:to], frequency = frequency(y))

  return(list(model = mod, type = type, y = y, training = y.tra,
              validation = df.val, fitted = pred, lags = lags,
              covariates = colnames(df), actfun = act.fun, hidden = hidden,
              epochs = epochs, units = units, batch_size = batch_size,
              gamma = gamma, cost = cost, epsilon = epsilon, xreg = xreg,
              algorithm = algorithm, learningrate = learningrate,
              training.time = duration, history = history))
}

GWL_ts_mixed_model <- function(y, type = c('neuralnet','svm'), hidden = NULL,
                          gamma = NULL, cost = NULL, epsilon = NULL,
                          learningrate = NULL, act.fun = NULL, algorithm = NULL,
                          from = 1, to = length(y), xreg = NULL, ...) {

  df <- data.frame(cbind(y, xreg))
  colnames(df)[1] <- "y"
  colnames(df)[-1] <- colnames(xreg)
  df <- data.frame(df)

  y.tra <- ts(y[from:to], frequency = frequency(y))

  df.tra <- data.frame(df[from:to, ])
  # df.tra <- df.tra[1:length(y), ]
  df.tra <- na.omit(df.tra)

  if(to!=length(y)) {
    df.val <- data.frame(df[(to+1):length(y), ])
    y.val <- y[(to+1):length(y)]
  } else {
    df.val <- NULL
    y.val <- NULL
  }
  df <- na.omit(df)

  # Match selected model
  type <- match.arg(type)

  # Chooses arguments according to model type
  switch (
    type,

    ## Neural Network
    neuralnet = {
      t1 <- Sys.time()
      mod <- neuralnet::neuralnet(
        formula = y~.,
        data = df.tra,
        hidden = hidden,
        act.fct = act.fun,
        algorithm = algorithm,
        learningrate = learningrate,
        linear.output = TRUE,
        err.fct = 'sse',
        stepmax = 1e+06,
        ...
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      # Fitted (New forecasts)
      pred <- as.numeric(stats::predict(mod, newdata = df))
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred),
                 frequency = frequency(y), end = end(y))
    },

    ## Support Vector Machine
    svm = {
      t1 <- Sys.time()
      mod <- e1071::svm(
        formula = y~.,
        data = df.tra,
        # scale = TRUE,
        scale = FALSE,
        type = 'eps-regression',
        kernel = "radial",
        gamma = gamma,
        cost = cost,
        epsilon = epsilon,
        ...
      )
      duration <- as.numeric(Sys.time() - t1, units = "secs")

      # Fitted (New forecasts)
      pred <- stats::predict(mod, newdata = df)
      pred <- ts(c(rep(NA, (length(y)-length(pred))), pred),
                 frequency = frequency(y), end = end(y))
    }
  )

  train.y <- ts(y[from:to], frequency = frequency(y))

  return(list(model = mod, type = type, y = y, training = y.tra,
              validation = df.val, fitted = pred, hidden = hidden,
              algorithm = algorithm, actfun = act.fun,
              learningrate = learningrate, gamma = gamma, cost = cost,
              epsilon = epsilon, xreg = xreg, training.time = duration))
}

GWL_ts_forecast <- function(object, h = 12, xreg = NULL) {
  pred <- numeric(h)
  # if(!is.null(xreg)) xreg <- data.frame(xreg)
  mod <- object$model
  covariates <- object$covariates
  type <- object$type
  y <- object$training
  lags <- object$lags
  t1 <- Sys.time()

  # 11/05
  lags <- object$lags
  maxlag <- max(lags)
  flag <- rev(tail(y, n = maxlag))

  # Iterative 1-step forecast
  if (type == 'elm') { # NOVO
    for(i in 1:h) {
      # df <- matrix(data = rev(y)[lags], nrow = 1)
      # if(!is.null(xreg)) df <- data.frame(cbind(df, xreg[i, ]))
      # colnames(df) <- covariates[-1]

      newdata <- flag[lags]
      newdata <- data.frame(matrix(newdata, nrow = 1))
      colnames(newdata) <- covariates[-1]

      pred[i] <- predict(object = mod, newdata = newdata)
      flag <- c(pred[i], flag[-maxlag])

      # pred[i] <- elmNNRcpp::elm_predict(mod, newdata = df)
      # y <- ts(c(y, pred[i]), frequency = frequency(object$y))
    }
  } else if (type == 'neuralnet' || type == 'svm') {
    for(i in 1:h) {
      df <- data.frame(matrix(data = rev(y)[lags], nrow = 1))
      # print(colnames(df))
      # print(colnames(object$data))
      # if(!is.null(xreg)) df <- data.frame(cbind(df, xreg[i, ]))
      # colnames(df) <- colnames(mod$data)
      # colnames(df) <- covariates[-1]

      newdata <- flag[lags]
      newdata <- data.frame(matrix(newdata, nrow = 1))
      colnames(newdata) <- covariates[-1]

      pred[i] <- predict(object = mod, newdata = newdata)
      flag <- c(pred[i], flag[-maxlag])

      # pred[i] <- stats::predict(mod, newdata = df)
      # y <- ts(c(y, pred[i]), frequency = frequency(object$y))
    }
  } else if (type == 'lstm' || type == 'gru' || type == 'mlp') {
    for(i in 1:h) {
      # df <- matrix(data = rev(y)[lags], nrow = 1)
      # if(!is.null(xreg)) df <- data.frame(cbind(df, xreg[i, ]))

      newdata <- flag[lags]
      newdata <- matrix(newdata, nrow = 1)
      # colnames(newdata) <- covariates[-1]
      dim(newdata) <- c(1, ncol(newdata), 1)
      pred[i] <- mod %>% predict(newdata)
      flag <- c(pred[i], flag[-maxlag])

      # dim(df) <- c(1, ncol(df), 1)

      # pred[i] <- mod %>% predict(df)
      # y <- ts(c(y, pred[i]), frequency = frequency(object$y))
    }
  } else if (type == 'nnet') {
    pred <- forecast::forecast(mod, h = h)$mean
  } else if (type == 'arima' || type == 'ets') {
    pred <- forecast::forecast(mod, h = h)$mean
  }

  duration <- as.numeric(Sys.time() - t1, units = "secs")
  pred <- ts(pred, frequency = frequency(object$y))

  return(list('forecasts' = pred, 'forecasting.time' = duration))
}

# LIXO
# NEURAL NETWORK
# nnetar_ga <- function(y, p, P, hid.max = 50, training, xreg = NULL, ...) {
#
#   # GA fitness function
#   nnetar_ga_fitness <- function(par, y, training, xreg) {
#
#     # Selects lag candidates
#     lags <- compose_lags(y, round(par[1]), round(par[2]))
#
#     # NEURALNET model
#     mod <- GWL_ts_model(
#       y = y,
#       type = "nnet",
#       lags = lags,
#       hidden = round(par[3]),
#       from = 1,
#       to = training)
#
#     # Validation
#     prev <- GWL_ts_forecast(mod, h = length(mod$validation$y))$forecasts
#
#     # MSE
#     return(-mean((mod$validation$y - prev)^2))
#   }
#
#   # GA optimization
#   t1 <- Sys.time()
#   ga.nnetar <- GA::ga(
#     type = "real-valued",
#     fitness = nnetar_ga_fitness, y, training, xreg,
#     lower = c(1, 1, 1),
#     upper = c(p, P, hid.max),
#     names = c('p', 'P', 'hidden'),
#     ...
#   )
#   duration <- as.numeric(Sys.time() - t1, units = "secs")
#
#   # Optimum model
#   opt.p <- round(ga.nnetar@solution[1,1])
#   opt.P <- round(ga.nnetar@solution[1,2])
#   opt.hid <- round(ga.nnetar@solution[1,3])
#
#   opt.mod <- GWL_ts_model(
#     y = y,
#     type = "nnet",
#     lags = compose_lags(y = y, p = opt.p, P = opt.P),
#     hidden = opt.hid,
#     from = 1,
#     to = training
#   )
#
#   obj <- list(
#     model = opt.mod,
#     ga = ga.nnetar,
#     y = y,
#     p = opt.p,
#     P = opt.P,
#     hidden = opt.hid,
#     optimization.time = duration
#   )
#
#   return(obj)
# }

# MLP
# mlp_ga <- function(y, p, P, unit.max = 50, training, xreg = NULL, ...) {
#
#   # GA fitness function
#   mlp_ga_fitness <- function(par, y, act.fun.labels, training, xreg) {
#
#     # Selects lag candidates
#     lags <- compose_lags(y, round(par[1]), round(par[2]))
#
#     # Randomly selects one of the activation function candidates
#     afu <- act.fun.labels[round(par[4])]
#
#     # MLP model
#     mod <- GWL_ts_model(
#       y = y,
#       type = 'mlp',
#       lags = lags,
#       units = round(par[3]),
#       act.fun = afu,
#       epochs = 50,
#       from = 1,
#       to = training)
#
#     # Validation
#     prev <- GWL_ts_forecast(mod, h = length(mod$validation$y))$forecasts
#
#     # MSE
#     return(-mean((mod$validation$y - prev)^2))
#   }
#
#   # Compose activation function label candidates
#   act.fun.labels <- c('relu','sigmoid','softmax','softplus','softsign',
#                       'tanh','selu','elu','exponential')
#
#   # GA optimization
#   t1 <- Sys.time()
#   ga.mlp <- GA::ga(
#     type = "real-valued",
#     fitness = mlp_ga_fitness, y, act.fun.labels, training, xreg,
#     lower = c(1, 1, 1, 1),
#     upper = c(p, P, unit.max, length(act.fun.labels)),
#     names = c('p', 'P', 'units', 'act.fun'),
#     ...
#   )
#   duration <- as.numeric(Sys.time() - t1, units = "secs")
#
#   # Optimum model
#   opt.p <- round(ga.mlp@solution[1,1])
#   opt.P <- round(ga.mlp@solution[1,2])
#   opt.uni <- round(ga.mlp@solution[1,3])
#   opt.afu <- act.fun.labels[round(ga.mlp@solution[1,4])]
#
#   opt.mod <- GWL_ts_model(
#     y = y,
#     type = "mlp",
#     lags = compose_lags(y = y, p = opt.p, P = opt.P),
#     units = opt.uni,
#     act.fun = opt.afu,
#     epochs = 50,
#     from = 1,
#     to = training
#   )
#
#   obj <- list(
#     model = opt.mod,
#     ga = ga.mlp,
#     y = y,
#     p = opt.p,
#     P = opt.P,
#     units = opt.uni,
#     acf.fun = opt.afu,
#     optimization.time = duration
#   )
#
#   return(obj)
# }
