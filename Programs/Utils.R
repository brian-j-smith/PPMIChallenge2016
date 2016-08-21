# Add and use libraries
add <- function(package, character.only=FALSE) {
  if(!character.only) package <- as.character(substitute(package))
  if(!require(package, character.only=TRUE)) install.packages(package)
}

using <- function(package, character.only=FALSE) {
  if(!character.only) package <- as.character(substitute(package))
  add(package, character.only=TRUE)
  library(package, character.only=TRUE)
}


# Area under the curve of x over time
auc.change <- function(x, time) {
  n <- ncol(x)
  timediff <- diff(time)
  
  stopifnot(n == length(time))
  stopifnot(all(timediff > 0))
  
  auc <- 0
  for(i in seq(2, n, by=1)) {
    xmin <- pmin(x[,i], x[,i-1])
    auc <- auc + timediff[i-1] * (0.5 * abs(x[,i] - x[,i-1]) + xmin - x[,1])
  }
  auc
}

# Helper function for outcome.rate
# This function measures "Increase in outcome score per visit"
getSlope <- function(x, outcome, scale, sig.level = 0.05, ttsp = F) {
  nvisits <- sum(!is.na(x$time) & !is.na(x[outcome])) 
  
  if(nvisits < 2 ) return(NA)
  
  link <- ifelse(scale == 'absolute', 'identity', 'log')
  
  # Some fits don't converge when the scale is relative
  fit <- suppressWarnings(
    glm(x[[outcome]] ~ x$time, family = gaussian(link = link), 
        start = c(1,0), control = list(maxit = 1000)))
  
  if(!fit$converged) {
    warning(paste0('Fit not converged for patient ', x$patno[1], 
                   '\nSlope assumed to be 0\n'))
    print(x)
    return(c(nvisits, 0, 1, NA, NA))
  }
  
  # Delete NA values
  x <- x[x$event_id == 'BL' | substr(x$event_id,1,1) == 'V',]
  sp_ind <- NA # Indicates statistical significance
  
  ## Calculate Time to significant progression (if ttsp == T)
  
  if(dim(x)[1] > 2 & ttsp) {
    ttsp <- x$time[dim(x)[1]]
    sp_ind <- 0 # Indicates statistical significance
    for(j in 2:dim(x)[1]) {
      fit.j <- lm(x[[outcome]][1:j] ~ x$time[1:j])
      if(!is.na(fit.j$coef[2])) {
        p.j <- summary(fit.j)$coef[2,4]
        p.j <- ifelse(is.na(p.j), 1, p.j)
        
        if(p.j < sig.level & fit.j$coef[2] > 0) {
          sp_ind <- 1
          ttsp <- x$time[j]
          t_ind <- j
        }
        
        if(sp_ind == 1 & p.j > sig.level){
          sp_ind <- 0
          ttsp <- x$time[j]
          t_ind <- 0
        }
      }
    }
  }
  s <- suppressWarnings(summary(fit)$coef)
  return(c(nvisits, unname(s[2,c(1,4)]), ttsp, sp_ind))
}

# outcome is string

outcome.rate <- function(outcome, data, sig.level = 0.05, scale = 'absolute', 
                         patno_only = F, ...){
  
  outcome.df <- data[c('patno', 'event_id', outcome)]
  outcome.df$time <- ifelse(outcome.df$event_id == 'BL', 0, NA)
  outcome.df$time[outcome.df$event_id == 'V01'] <- 3
  outcome.df$time[outcome.df$event_id == 'V02'] <- 6
  outcome.df$time[outcome.df$event_id == 'V03'] <- 9
  outcome.df$time[outcome.df$event_id == 'V04'] <- 12
  outcome.df$time[outcome.df$event_id == 'V05'] <- 18
  outcome.df$time[outcome.df$event_id == 'V06'] <- 24
  outcome.df$time[outcome.df$event_id == 'V07'] <- 30
  outcome.df$time[outcome.df$event_id == 'V08'] <- 36
  outcome.df$time[outcome.df$event_id == 'V09'] <- 42
  outcome.df$time[outcome.df$event_id == 'V10'] <- 48
  outcome.df$time[outcome.df$event_id == 'V11'] <- 54
  outcome.df$time[outcome.df$event_id == 'V12'] <- 60
  
  # NAs occur when a visit is not baseline or visit V
  patnos <- unique(outcome.df$patno)
  
  # Sometimes only need patnos
  if(patno_only) return(data.frame(patno = patnos))
  
  n <- length(patnos)
  
  # Initialize vectors to contain patient level data
  rates <- numeric(n)
  p_rates <- numeric(n)
  nvisits <- numeric(n)
  ttsp <- numeric(n) # Time to significant progression
  sp_ind <- numeric(n) # Time to significant progression indicator
  time_on_study <- numeric(n)
  
  # Iterate through all patients, calculate slope
  for(i in 1:n){
    x <- outcome.df[outcome.df$patno == patnos[i],]
    x <- x[!is.na(x$time),]
    y <- (getSlope(x, outcome, scale))
    nvisits[i] <- y[1]
    rates[i] <- y[2]
    p_rates[i] <- y[3]/2
    ttsp[i] <- y[4]
    sp_ind[i] <- y[5]
    time_on_study[i] <- ifelse(!all(is.na(x$time)), max(x$time, na.rm = T), NA)
  }
  
  # Calculate truncated rate
  rates0 <- rates * (p_rates < sig.level)
  rates0[rates == 0] <- 0
  
  # Return data frame
  myOutcome <- data.frame(patno = patnos, rates = rates, rates0 = rates0, p_rates = p_rates, 
                          ttsp = ttsp, sp_ind = sp_ind, time_on_study = time_on_study)
}

# Get Changepoint Survival outcome

outcome.changepoint <- function(outcome, data, sig.level = .05, clin.sig.level = 3, 
                                scale = 'absolute', plot = T, ...){
  
  outcome.df <- data[c('patno', 'event_id', outcome)]
  outcome.df$time <- ifelse(outcome.df$event_id == 'BL', 0, NA)
  outcome.df$time[outcome.df$event_id == 'V01'] <- 3
  outcome.df$time[outcome.df$event_id == 'V02'] <- 6
  outcome.df$time[outcome.df$event_id == 'V03'] <- 9
  outcome.df$time[outcome.df$event_id == 'V04'] <- 12
  outcome.df$time[outcome.df$event_id == 'V05'] <- 18
  outcome.df$time[outcome.df$event_id == 'V06'] <- 24
  outcome.df$time[outcome.df$event_id == 'V07'] <- 30
  outcome.df$time[outcome.df$event_id == 'V08'] <- 36
  outcome.df$time[outcome.df$event_id == 'V09'] <- 42
  outcome.df$time[outcome.df$event_id == 'V10'] <- 48
  outcome.df$time[outcome.df$event_id == 'V11'] <- 54
  outcome.df$time[outcome.df$event_id == 'V12'] <- 60
  
  # NAs occur when a visit is not baseline or visit V, get rid of em
  outcome.df <- outcome.df[outcome.df$event_id == 'BL' | substr(outcome.df$event_id,1,1) == 'V',]
  outcome.df <- outcome.df[!is.na(outcome.df$event_id),]
  
  patnos <- unique(outcome.df$patno)
  n <- length(patnos)
  
  # Initialize vectors to contain patient level data
  nvisits <- numeric(n)
  ttcsp <- numeric(n) # Time to significant progression
  csp_ind <- numeric(n) # Time to significant progression indicator
  effect.size <- numeric(n)
  
  # Use poisson glm when scale = 'relative'
  family <- ifelse(scale == 'absolute', 'gaussian', 'poisson')
  
  # For each patient...
  for(i in 1:n){
    x <- outcome.df[outcome.df$patno == patnos[i],]
    outcome.i <- x[[outcome]]
    ttcsp[i] <- 0
    csp_ind[i] <- 0
    
    if(length(x$time) > 1 & !all(is.na(outcome.i))) {
      ## For each time point for that patient
      AICs <- numeric(length(x$time))
      for(t in 1:length(x$time)) {
        dichotomized_time <- x$time >= x$time[t]
        fit.effect <- glm(outcome.i ~ dichotomized_time, family = family)
        AICs[t] <- AIC(fit.effect)
      }
      cuttoff <- x$time[which.min(AICs)]
      fit.effect <- lm(outcome.i ~ (x$time >= cuttoff))
      nvisits[i] <- length(x$time)
      ttcsp[i] <- max(x$time)
      if(!is.na(coef(fit.effect)[2])) {
        
        # Find minimum of onesided sig.level CI 
        min.plausible <- suppressWarnings(confint(fit.effect, level = 1 - sig.level*2)[2,1])
        min.plausible <- ifelse(is.nan(min.plausible), -Inf, min.plausible)
        if(family == 'poisson') min.plausible <- exp(min.plausible)
        
        # Would we reject null at clin.sig effect size?
        csp_ind[i] <- (min.plausible > clin.sig.level)
        ttcsp[i] <- cuttoff * (csp_ind[i]) + (!csp_ind[i]) * max(x$time)
      }
    }
  }
  if(plot) {
    S <- survfit(Surv(ttcsp, csp_ind) ~ 1)
    plot(S, main = paste('All cohorts sig level:',sig.level, 
                         'clin.sig:', clin.sig.level,
                         '\n scale:', scale) 
         , ylab = outcome, ...)
  }
  myOutcome <- data.frame(patno = patnos, ttcsp = ttcsp, csp_ind = csp_ind)
}



dropfactors <- function(data) {
  i <- 1
  while(i <= length(data)) {
    x <- data[[i]]
    if(is.factor(x) && nlevels(x) < 2) {
      data[[i]] <- NULL
    } else {
      i <- i + 1
    }
  }
  data
}


join.ppmi <- function(..., by=NULL, subset, select, na.add=FALSE, ST2V=FALSE) {
  X <- join_all(list(...), by=by)
  if(ST2V) {
    X <- ddply(X, .(patno), mutate, event_id = ST2V(event_id, infodt))
  }
  Xsub <- droplevels(do.call(base::subset, list(X, subset=substitute(subset),
                                                select=substitute(select))))
  f <- colwise(function(x) if(na.add && is.factor(x)) addNA(x, ifany=TRUE) else x)
  f(Xsub)
}


model.data <- function(fo, data, method=NULL, prop.na=0.20, ...) {
  mf <- model.frame(fo, data, na.action=na.pass)
  x <- model.matrix(fo, mf)
  if(attr(terms(mf), "intercept")) x <- subset(x, select=-`(Intercept)`)
  y <- model.response(mf)
  
  idx1 <- complete.cases(y)
  idx2 <- apply(x, 2, function(x) mean(is.na(x)) <= prop.na)
  x <- subset(x, idx1, idx2)
  y <- subset(y, idx1)
  
  if(length(method)) {
    pp <- preProcess(x, method=method, ...)
    x <- predict(pp, x)
  }
  
  list(x=x, y=y)
}


seq.names <- function(x, from, to) {
  vals <- names(x)
  idx <- match(c(from, to), vals)
  vals[seq(idx[1], idx[2])]
}


ST2V <- function(event_id, infodt) {
  i <- which(event_id == "BL")
  j <- which(event_id == "ST")
  
  tomonths <- function(dt) {
    sapply(strsplit(dt, "/"), function(x) sum(as.numeric(x) * c(1, 12)))
  }
  
  if(length(i) && length(j)) {
    d <- tomonths(infodt) - tomonths(infodt[i])
    
    visits <- c(V01=3, V02=6, V03=9, V04=12, V05=18, V06=24, V07=30, V08=36,
                V09=42, V10=48, V11=54, V12=60)
    lwr <- visits - 1
    upr <- visits + 1
    
    vid <- sapply(d, function(d) {
      k <- which(lwr <= d & d <= upr)
      if(length(k)) names(k) else NA
    })
    
    v <- vid[j]
    if(!(v %in% event_id)) event_id[j] <- v
  }
  event_id
}

## Function for within sample error
## Takes a Fit object and returns a dataframe with the within-sample RMSE and Rsquared for each model
getWithinSampleError <- function(models) {
  
  # initialize data frame for results
  results <- data.frame()
  
  # Iterate through all models in the Fit object
  for(i in names(models)) {
    for(j in names(models[[i]])) {
      for(k in names(models[[i]][[j]])){
        ## Calculate for each model separately
        model <- models[[i]][[j]][[k]]
        
        # Extract outcome
        y <- model$trainingData$.outcome
        
        # Extract predictions
        predicted <- predict(model)
        
        # Calculate Rsquared
        wsRsquared <- 1 - sum((y-predicted)^2)/sum((y-mean(y))^2)
        
        # Calculate RMSE
        wsRMSE <- (1-wsRsquared) * sd(y)
        
        # Extract cvRMSE and cvRsquared (picks final model on lowest RMSE)
        idx <- which.min(model$results$RMSE)
        cvRMSE <- model$results$RMSE[idx]
        cvRsquared <- model$results$Rsquared[idx]
        
        cvRMSESD <- model$results$RMSESD[idx]
        cvRsquaredSD <- model$results$RsquaredSD[idx]
        
        # Create modelName
        modelName <- paste(i, j, k, sep = '.')
        
        result <- data.frame(modelName = modelName, 
                             wsRMSE = wsRMSE, wsRsquared = wsRsquared,
                             cvRMSE = cvRMSE, cvRsquared = cvRsquared,
                             cvRMSESD = cvRMSESD, cvRsquaredSD = cvRsquaredSD)
        results <- rbind(results, result)
      }
    }
  }
  return(results)
}


## Function to extract max summary metric from FitList object and return results in a tidy tabular(matrix)
SummaryTable <- function(FitListObj, metric = "Rsquared", digits = 2,
                         na.rm = FALSE) {
  
  outVars <- names(FitListObj)
  typeMethods <- names(FitListObj[[outVars[1]]])
  if (any(typeMethods == "RFE")){
    idx <- which(typeMethods == "RFE")
    typeMethods <- typeMethods[-idx]
  }
  
  Result <- list()
  
  for(outVar in outVars){
    
    for (typeMethod in typeMethods){
      
      metrics <- lapply(FitListObj[[outVar]][[typeMethod]], function(x){
        m <- round(mean(x$resample[,metric], na.rm=na.rm), digits)
        s <- round(sd(x$resample[,metric], na.rm=na.rm), digits)
        r <- paste0(m, " (", s, ")")
        return(r)
      })
      Result[[outVar]][[typeMethod]] <- unlist(metrics)
      
    }
    
    Result[[outVar]] <- unlist(Result[[outVar]])
    
  }
  
  Tbl <- do.call(cbind, Result)
  return(Tbl)
  
}


## Returns the best models for variables in a list of fits
bestmodel <- function(FitList, metric = "Rsquared", max = (metric != "RMSE"),
                      na.rm = FALSE) {
  f <- function(Fit) {
    Fit <- unlist(Fit, recursive=FALSE)
    stat <- function(fit) apply(fit$resample[metric], 2, mean, na.rm=na.rm)
    vals <- sapply(Fit, stat)
    idx <- if(max) which.max(vals) else which.min(vals)
    Fit[[idx]]
  }
  lapply(FitList, f)
}


## Return outcomes values for shiny app
outValsList <- function(BestFitList, digits = getOption("digits")) {
  lapply(BestFitList, function(fit) {
    data.frame(obs = fit$trainingData$.outcome,
               pred = round(as.vector(predict(fit)), digits))
  })
}
