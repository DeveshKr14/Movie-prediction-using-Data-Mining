#Packages loading
setwd("F://Rsoftware/New")
getwd()
library(ggplot2)
library(dplyr)
library(caret)
library(statsr)
library(gridExtra)
library(GGally)
library(ggthemes)
library(knitr)

#Loading Data
load("movies.Rdata")
str(movies)
summary(movies)
#Generating Training and Testing Dataset
set.seed(5329)
inTrain <- createDataPartition(y=movies$imdb_rating, p=0.994, list=FALSE)
inTrain
training <- movies[inTrain,]
testing <- movies[-inTrain,]

#Dimention of training and Testing Dataset
dim(training)
dim(testing)

#normal distribution of imdb_num_votes
quantile(training$imdb_num_votes, c(0, 0.25, 0.5, 0.75, 0.9, 1))

#distribution of critics_score and imdb_rating
d1 <- ggplot(data = training, aes(y = imdb_rating, x = critics_score, colour = critics_rating)) + geom_point()
d2 <- ggplot(data = training, aes(y = imdb_rating, x = critics_score, colour = audience_rating)) + geom_point()
grid.arrange(d1, d2, nrow = 1, ncol = 2)

#Finding minimum,maximum,mean and median of imdb_rating, imdb_num_votes, critics_score, audience_score
minv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(min) %>% sapply(round,2)
maxv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(max) %>% sapply(round,2)
meanv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(mean) %>% sapply(round,2)
medianv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(median) %>% sapply(round,2)
df <- rbind(minv, maxv, meanv, medianv)
rownames(df) <- c("min", "max", "mean", "median")
kable(df)

#Representation of imdb_rating,imdb_num_votes,critics_score and audience_score on histogram 
p1 <- ggplot(data = training, aes(x = imdb_rating)) + geom_histogram(colour = "black", fill = "skyblue", binwidth = .3)
p2 <- ggplot(data = training, aes(x = imdb_num_votes)) + geom_histogram(colour = "black", fill = "salmon", binwidth = 40000, alpha = 0.5)
p3 <- ggplot(data = training, aes(x = critics_score)) + geom_histogram(colour = "black", fill = "cyan", binwidth = 5, alpha = 0.5)
p4 <- ggplot(data = training, aes(x = audience_score)) + geom_histogram(colour = "black", fill = "yellow", binwidth = 5, alpha = 0.7)
grid.arrange(p1, p2, p3, p4, nrow = 1, ncol = 4)

#Data representation on class basis of best_actor_win, best_actress_win, best_director_win in the form of table 
actr <- table(training$best_actor_win)
acts <- table(training$best_actress_win)
dir <- table(training$best_dir_win)
flm2 <- training %>% mutate(oscar = ifelse(best_actor_win == "yes" | best_actress_win == "yes" | best_dir_win == "yes", "yes", "no"))
osc <- flm2 %>% select(oscar) %>% group_by(oscar) %>% table() %>% rbind(actr, acts, dir)
rownames(osc) <- c("At.least.one.Oscar", "best.actor", "best.actress", "best.director")
osc

#Representation of above creates table data on the basis of imdb reting and critics score
oscar_in_cast <- flm2 %>% filter(oscar == "yes") %>% arrange(imdb_rating) %>% select(title) %>% data.frame() %>% head(6)
x1 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = oscar)) + geom_point() + scale_colour_discrete(name="Combined") + scale_fill_hue(name="Combined")
x2 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_actor_win)) + geom_point() + scale_colour_discrete(name="Actor") + scale_fill_hue(name="Actor")
x3 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_actress_win)) + geom_point() + scale_colour_discrete(name="Actress") + scale_fill_hue(name="Actress")
x4 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_dir_win)) + geom_point() + scale_colour_discrete(name="Director") + scale_fill_hue(name="Director")
grid.arrange(x1, x2, x3, x4, nrow = 1, ncol = 4)

#Data representation on class basis of best_pic_win, bes_pic_nom and oscar nomination win in tabular form
nom <- table(flm2$best_pic_nom)
win <- table(flm2$best_pic_win)
flm2 <- flm2 %>% mutate(oscar_nom_win = ifelse(best_pic_nom == "yes" | best_pic_win == "yes", "yes", "no"))
nom_win <- table(flm2$oscar_nom_win)
comb_nom_win <- rbind(nom_win, nom, win)
rownames(comb_nom_win) <- c("combined", "nominations", "wins")
comb_nom_win

#Representation of above creates table data on the basis of imdb reting and critics score
w1 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = oscar_nom_win)) + geom_point()
w2 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_pic_nom)) + geom_point()
w3 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_pic_win)) + geom_point()
grid.arrange(w1, w2, w3, nrow = 1, ncol = 3)
outlier_best_pic_nom <- flm2 %>% filter(best_pic_nom == "yes") %>% arrange(imdb_rating) %>% data.frame() %>% head(1)

#Histogram along with Scatterplot showing theater release day, month and year data
g1 <- ggplot(data = flm2, aes(x = thtr_rel_year)) + geom_histogram(colour = "black", fill = "orange", alpha = 0.5)
g2 <- ggplot(data = flm2, aes(x = thtr_rel_month)) + geom_histogram(colour = "black", fill = "blue", alpha = 0.5)
g3 <- ggplot(data = flm2, aes(x = thtr_rel_day)) + geom_histogram(colour = "black", fill = "green", alpha = 0.5)
g4 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(thtr_rel_year))) + geom_point()
g5 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(thtr_rel_month))) + geom_point()
g6 <- ggplot(data = movies, aes(y = imdb_rating, x = critics_score, colour = factor(thtr_rel_day))) + geom_point()
grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2, ncol = 3)

#Histogram along with Scatterplot showing dvd release day, month and year data
g7 <- ggplot(data = flm2, aes(x = dvd_rel_year)) + geom_histogram(colour = "black", fill = "orange", alpha = 0.5)
g8 <- ggplot(data = flm2, aes(x = dvd_rel_month)) + geom_histogram(colour = "black", fill = "blue", alpha = 0.5)
g9 <- ggplot(data = flm2, aes(x = dvd_rel_day)) + geom_histogram(colour = "black", fill = "green", alpha = 0.5)
g10 <-ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(dvd_rel_year))) + geom_point()
g11 <- ggplot(data = movies, aes(y = imdb_rating, x = critics_score, colour = factor(dvd_rel_month))) + geom_point()
g12 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(dvd_rel_day))) + geom_point()
grid.arrange(g7, g8, g9, g10, g11, g12 ,nrow = 2, ncol = 3)

#Building Correlation plot
num_var <- flm2 %>% select(imdb_rating,imdb_num_votes, critics_score,audience_score)
ggcorr(num_var, name = "Correlation", label = TRUE, alpha = TRUE, palette = "PuOr") + ggtitle("correlation matrix plot") + theme_dark()

#Finding best fitted attribute for model development and prediction
fit1 <- lm(imdb_rating ~ audience_score, data = flm2)
fit2 <- lm(imdb_rating ~ audience_score + critics_score, data = flm2)
summary(fit2)
fit3 <- lm(imdb_rating ~ audience_score + critics_score + imdb_num_votes, data = flm2)
fit4 <- lm(imdb_rating ~ audience_score + critics_score + oscar, data = flm2)
summary(fit4)

#Ploting residual data
t1 <-  ggplot(data = flm2, aes(x = audience_score, y = resid(fit4))) + geom_hline(yintercept = 0, size = 1)  + xlab("audience_score") + ylab("Residual") + geom_point()  
t2 <-  ggplot(data = flm2, aes(x = critics_score, y = resid(fit4))) + geom_hline(yintercept = 0, size = 2)  + xlab("critics_score") + ylab("Residual") + geom_point()
grid.arrange(t1, t2, nrow = 1, ncol = 2)

#Ploting residual data for analsys
par(mfrow = c(1,3))
hist(fit4$residuals, breaks = 25, main = "Histogram of Residuals", col = "blue", border = "pink", prob = TRUE)
curve(dnorm(x, mean = mean(fit4$residuals), sd = sd(fit4$residuals)), col="red", add=T, lwd = 3)  
qqnorm(fit4$residuals)
qqline(fit4$residuals)
plot(fit4$residuals, main = "Plot of Residuals VS order of Observations")

#Ploting graph on fit4 data
t3 <- ggplot(data.frame(x = fit4$fitted.values, y = resid(fit4)), aes(x=x, y=y)) + geom_hline(yintercept = 0, size = 1)  + xlab("fitted.values") + ylab("Residual") + geom_point()  
t4 <- ggplot(data.frame(x =fit4$fitted.values, y = abs(resid(fit4))), aes(x=x, y=y)) + geom_hline(yintercept = 0, size = 1)  + xlab("fitted.values") + ylab("Residual") + geom_point() 
grid.arrange(t3, t4, nrow = 1, ncol = 2)

#Box plot model for Genere wise movie runtime in minutes
p_genrerun <- ggplot(movies, aes(x=factor(genre), y=runtime)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genrerun + ggtitle("Genre to runtime") + geom_hline(yintercept =median(movies$runtime, na.rm = TRUE), col = "royalblue",lwd = 1)

#Box plot model for Genere wise IMDB_rating
p_genreimdb <- ggplot(movies, aes(x=factor(genre), y=imdb_rating)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genreimdb + ggtitle("Genre to IMDB rating") + geom_hline(yintercept =median(movies$imdb_rating, na.rm = TRUE), col = "royalblue",lwd = 1)

#Scatter plot of imdb_rating and runtime in minutes
plot(movies$imdb_rating,movies$runtime, main="IMDB rating to runtime", xlab = "IMDB rating", ylab="Runtime in minutes")
abline(lm(movies$runtime~movies$imdb_rating),col = "royalblue",lwd = 1)

#Scatter plot of audience score and critics score
ggplot(data = movies, aes(x = critics_score, y = audience_score)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) + ggtitle("Critics score to audience score - Rotten")

#Scatter plot of imdb_rating and audience score
ggplot(data = movies, aes(x = imdb_rating, y = audience_score)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) + ggtitle("Critics score to audience score - IMDB")

#Scatter plot of imdb_rating and critics score
ggplot(data = movies, aes(x = critics_score, y = imdb_rating)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) + ggtitle("IMDB vs. Rotten")

#regression analysis of audience_score with imdb_rating and critics_score
regres <- lm(movies$audience_score ~ movies$critics_score + movies$imdb_rating)
summary(regres)

#regression analysis of critics_score and imdb_rating
reg <- lm( movies$critics_score ~ movies$imdb_rating)
summary(reg)

#Model development for final prediction
model <- lm(audience_score ~ critics_score, data=movies) #funny thing, if you will not use "data=" you will not be able to get predict() to work properly
par(mfrow=c(2,2)) #combine plots to 2x2 table
hist(model$residuals, main="residuals")
qqnorm(model$residuals)
qqline(model$residuals)  
plot(model$residuals ~ model$fitted)
summary(model)

#Prediction-1 
film <- data.frame(title ="Disaster Movie", critics_score = 1)
predict(model, film, interval = "prediction", level = 0.95)

#Prediction-2
film2 <- data.frame(title ="Hellraiser - Bloodline", critics_score = 25)
predict(model, film2, interval = "prediction", level = 0.95)


 