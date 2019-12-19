library("dplyr")
library("lme4")
library("lmerTest")

source("utils.r")

#read in data
wellness <- read.csv("../data/wellness_adj.csv")
performance <- read.csv("../data/performance.csv")
games <- read.csv("../data/games.csv")
rpe <- read.csv("../data/rpe.csv")

#drop index column
wellness <- wellness[,2:ncol(wellness)]
performance <- performance[,2:ncol(performance)]

wellness <- wellness[complete.cases(wellness),]
performance <- performance[complete.cases(performance),]
games <- games[complete.cases(games),]
rpe <- rpe[complete.cases(rpe),]

#merge date onto game data
colnames(games)[1] <- "GameID"
performance <- merge(games, performance, by=c("Date", "GameID"))
performance <- performance[,c("Date", "Tournament", "TournamentGame", "PlayerID", "AccelImpulse", "AccelLoad", "Speed")]

#format date
wellness$Date <- as.Date(wellness$Date, "%Y-%m-%d")
performance$Date <- as.Date(performance$Date, "%Y-%m-%d")
rpe$Date <- as.Date(rpe$Date, "%Y-%m-%d")

#normalize ordinal variables as if they were numeric
ord_vars <- c("Fatigue", "Soreness", "Desire", "Irritability", "SleepQuality", "Nutrition")

for(player in unique(wellness$PlayerID)){
  for(var in ord_vars){
    baseline <- round(mean(wellness[wellness$PlayerID == player, var]), 2) #mean value of ordinal variable
    wellness[wellness$PlayerID == player, var] <- wellness[wellness$PlayerID == player, var] / baseline
  }
}

#merge wellness data onto game data
performance <- merge(performance, wellness, by.x=c("Date", "PlayerID"), all.x=TRUE)
performance <- performance[complete.cases(performance),]

q <- 0.02
performance$Impulse_idx <- performance$AccelImpulse > quantile(performance$AccelImpulse)
performance$Load_idx <- performance$AccelLoad > quantile(performance$AccelLoad)
performance$Speed_idx <- performance$Speed > quantile(performance$Speed)


write.csv(performance, "../data/performance_normalized.csv", row.names = FALSE)

fit_impulse <- lmer(AccelImpulse ~ -1 + Fatigue + Soreness + Desire + Irritability + BedTime + SleepHours + SleepQuality + #fixed effects 
                      Pain + Illness + Menstruation + Nutrition + NutritionAdjustment +  USG + TrainingReadiness +
                      (1|PlayerID), #random effects
                    data=performance[performance$Impulse_idx == 1,])

saveRDS(fit_impulse, "../data/fit_impulse.rds")
summary(fit_impulse)


fit_load <- lmer(AccelLoad ~ -1 + Fatigue + Soreness + Desire + Irritability + BedTime + SleepHours + SleepQuality + #fixed effects 
                      Pain + Illness + Menstruation + Nutrition + NutritionAdjustment +  USG + TrainingReadiness +
                     (1|PlayerID), #random effects
                    data=performance[performance$Load_idx == 1,])

saveRDS(fit_load, "../data/fit_load.rds")
summary(fit_load)

fit_speed <- lmer(Speed ~ -1 + Fatigue + Soreness + Desire + Irritability + BedTime + SleepHours + SleepQuality + #fixed effects 
                   Pain + Illness + Menstruation + Nutrition + NutritionAdjustment +  USG + TrainingReadiness +
                   (1|PlayerID), #random effects
                 data=performance[performance$Speed_idx == 1,])

saveRDS(fit_speed, "../data/fit_speed.rds")
summary(fit_speed)


