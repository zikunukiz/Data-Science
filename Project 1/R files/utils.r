#Libraries----------------------------------------------------

require(ggplot2)
require(ggrepel)
require(lme4)


#Global Variables---------------------------------------------

# % of max heart rate for where training zone (left endpoint of each interval)
CAT6_HR <- 135/200
CAT5_HR <- 150/200
CAT4_HR <- 165/200
CAT3_HR <- 175/200
CAT2_HR <- 185/200
CAT1_HR <- 190/200

HR_CATS <- c(1, CAT1_HR, CAT2_HR, CAT3_HR, CAT4_HR, CAT5_HR, CAT6_HR)

#heart rate estimates for elite female rugby players
MAX_HR <- 190
REST_HR <- 60


#Functions----------------------------------------------------

#' Calculates fraction of hear rate reserve as defined in "Rationale and resources for teaching the mathematical 
#' modeling of athletic training and performance" paper provided by Ming.
#'
#' @param hr_avg numeric: average heart rate for workout 
#' @param hr_max numeric: athlete's maximum heart rate 
#' @param hr_rest numeric: athlete's resting heart rate 
#'
#' @return FHRR as defined/referenced above
#' @export
#'
#' @examples
FHRR <- function(hr_avg, hr_max=200, hr_rest=60){
  return((hr_avg - hr_rest)/(hr_max - hr_rest))
}


#' Integrates one second TRIMP (TRIMP defined as in "Rationale and resources for teaching the mathematical 
#' modeling of athletic training and performance" paper provided by Ming) over length of workout. The choice 
#' to integrate second by second heart rate data, rather than using the average heart rate for the workout,
#' was to ensure that the sum of the loads from any partition of the workout would equal the load for the total workout. 
#'
#' @param hr numeric vector: second by second heart rate data
#' @param hr_max numeric: athlete's maximum heart rate 
#' @param hr_rest numeric: athlete's resting heart rate 
#' @param men logical: is the athelete male?
#'
#' @return sum of second by second TRIMP for the workout
#' @export
#'
#' @examples
TRIMP <- function(hr, hr_max=200, hr_rest=60, men=TRUE){
  ffhr <- FHRR(hr, hr_max=200, hr_rest=60)
  
  if(men){
    k <- 0.64 * exp(1.92 * ffhr)
  }else{
    k <- 0.86 * exp(1.67 * ffhr)
  }
  
  load <- sum(1/60 * ffhr * k)
  
  return(load)
}


#' Wrapper function for TRIMP. See definition of TRIMP for details.
#'
#' @param hr 
#' @param hr_max 
#' @param hr_rest 
#' @param men 
#'
#' @return
#' @export
#'
#' @examples
heartRateToLoad <- function(hr, hr_max=200, hr_rest=60, men=TRUE){
  return(TRIMP(hr, hr_max, hr_rest, men))
}


#' Converts RPE to training load as measured by TRIMP. Possible categories for the trainig session are 
#' found according to the duration of the session and the RPE. The hypothesized heart rate for the session 
#' is the the mean of the point estimates for the possible heart rate zones. In the even that the session 
#' classifications do not overlap the workout is deemed to have been null and the training load is zero. This 
#' corresponds to workouts such as when the athlete reports an RPE of 2 and a duration of 3 mins.
#'
#' @param rpe numeric: rate of perceived exertion
#' @param dur numeric: duration of workout in minutes
#'
#' @return estimated load measured by TRIMP
#' @export
#'
#' @examples
rpeToLoad <- function(rpe, dur){
  classification1 <- NA
  classification2 <- NA
  
  #classify according to duration
  if(dur > 60){
    classification1 <- 6
  }else if(dur > 30){
    classification1 <- c(5, 6)
  }else if(dur > 10){
    classification1 <- c(4, 5, 6)
  }else if(dur > 6){
    classification1 <- c(3, 4, 5, 6)
  }else if(dur > 2){
    classification1 <- c(2, 3, 4, 5, 6)
  }else{
    classification1 <- c(1, 2, 3, 4, 5, 6)
  }
  
  #classify according to rpe
  if(rpe >= 7){
    classification2 <- c(1, 2, 3, 4, 5, 6)
  }else if(rpe >= 5){
    classification2 <- c(4, 5, 6)
  }else if(rpe >=3){
    classification2 <- c(5, 6)
  }else if(rpe >=1){
    classification2 <- c(6)
  }else{
    classification2 <- NULL
  } 
  
  workout_cat <- intersect(classification1, classification2)
  
  if(length(workout_cat) == 0){
    #failed to classify workout - load is zero
    load <- 0 
  }else{
    avg_hr <- rep(mean(MAX_HR * (HR_CATS[workout_cat + 1] +  HR_CATS[workout_cat]) / 2), dur*60) #point estimate for HR is mean of training zone
    load <- TRIMP(avg_hr, MAX_HR, REST_HR, men=FALSE)
  }
  
  
  return(load)
}

#' Classifies heart rate into training zones. 
#'
#' @param hr numeric: average heart rate for workout 
#' @param hr_max numeric: athlete's maximum heart rate 
#'
#' @return taining category corresponding to heart rate
#' @export
#'
#' @examples
heartRateToCat <- function(hr, max_hr=200){
  hr <- hr / max_hr
  hr_cats <- rep(NA, length(hr))
  
  #classify heart rates into training zones
  hr_cats[is.na(hr_cats) & hr > CAT1_HR] <- 1
  hr_cats[is.na(hr_cats) & hr > CAT2_HR] <- 2
  hr_cats[is.na(hr_cats) & hr > CAT3_HR] <- 3
  hr_cats[is.na(hr_cats) & hr > CAT4_HR] <- 4
  hr_cats[is.na(hr_cats) & hr > CAT5_HR] <- 5
  hr_cats[is.na(hr_cats) & hr > CAT6_HR] <- 6
  hr_cats[is.na(hr_cats)] <- "Recovery"
  
  return(hr_cats)
}


#' Plots second by second heart rate and training category along with the load of the training session.
#'
#' @param hr numeric: average heart rate for workout 
#' @param title string: title for plot 
#'
#' @return heart rate graph
#' @export
#'
#' @examples
plotHeartRate <- function(hr, title=waiver()){
  times <- 0:(length(hr) - 1) / 60
  Category <- as.factor(heartRateToCat(hr))
  dat <- cbind.data.frame(hr, times, Category)
  load <- heartRateToLoad(hr, hr_max = 200, hr_rest = 80)
  
  gg <- ggplot(dat, aes(times, hr, color=Category)) + 
    geom_line(aes(group=1), size=1) + 
    labs(subtitle = paste0("Training Load: ", round(load, 2)), title = title) +
    xlab("Minute") + 
    ylab("Heart Rate (bpm)") + 
    ylim(min(100, min(hr)), max(200, max(hr)))
  
  
  return(gg)
}


wellness_metric <- function(dat, fit, id){
  
  fixed_effects <- fixef(fit) #extract fixed effects
  random_effects <- unlist(ranef(fit)$PlayerID)[id] #extract random effects
  beta <- c(random_effects, fixed_effects)
  
  wellness_vars <- names(fixed_effects)
  
  dat <- dat[dat$PlayerID==id, wellness_vars]
  dat <- cbind.data.frame(intercept = rep(1, nrow(dat)), dat) #add intercept
  
  wellness_score <- as.matrix(dat)%*%(beta) #/ sqrt(sum((beta)^2)) #normalize wellness score
  
  return(wellness_score)
}
