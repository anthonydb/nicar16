
## if you cannot load the libraries, uncomment the two lines below and run the install
# install.packages("datasets")
# install.packages("nlme")

library("datasets")
library("nlme")

head(USJudgeRatings)

colMeans(USJudgeRatings)

judges.scaled <- scale(USJudgeRatings)

head(judges.scaled)

head(sort(rowSums(judges.scaled), decreasing = TRUE))

scores <- data.frame(bdf)
head(scores)

bigclass <- ifelse(scores$classsiz>=25, 1, 0) # marking a "large" class if it has 25+ students
scores <- cbind(scores, bigclass) # adding the column back to the data frame
head(scores[,c("langPOST", "bigclass")])

tapply(scores$langPOST, scores$bigclass, mean)

t.test(langPOST ~ bigclass, data=scores)

t.test(langPOST ~ sex, data=scores)

denom <- factor(scores$denomina, labels=(c("public","Protestant","Catholic","non-denom")))

round(tapply(scores$langPOST, denom, mean), digits=2)

aov.denom <- aov(langPOST ~ factor(denomina), data=scores)
summary(aov.denom)

pairwise.t.test(scores$langPOST, denom, p.adj="holm")

statedata <- data.frame(state.x77)
head(statedata)

plot(Life.Exp ~ HS.Grad, data = statedata, xlab = "High school graduates (%)", ylab = "Life expectancy (years)")

hs_le_model <- lm(Life.Exp ~ HS.Grad, data = statedata)

summary(hs_le_model)

multi_model <- lm(Life.Exp ~ HS.Grad + Murder + Illiteracy, data = statedata)

summary(multi_model)

statedata$Life.Exp_predicted <- predict(multi_model)
statedata$residual <- residuals(multi_model)
head(
    statedata[
        order(statedata$residual, decreasing = TRUE),
        c('HS.Grad','Murder','Illiteracy','Life.Exp','Life.Exp_predicted','residual')
    ]
)

# save the R data into a new variable
titanic <- data.frame(Titanic)
head(titanic[order(titanic$Freq, decreasing = TRUE),])

# drop all the rows with a frequency of 0
titanic <- titanic[titanic$Freq != 0,]
# replicate each line as many times as the "Freq" column calls for
# also, drop the "Freq" column
titanic_long <- titanic[,c(1,2,3,4)][rep(seq_len(dim(titanic)[1]), titanic$Freq),]
# confirm that we now have a row for every passenger, which matches the sum of "Freq"
print(c(nrow(titanic_long), sum(titanic$Freq)))

head(titanic_long)

str(titanic_long)

titanic_model <- glm(
    Survived ~ Class + Sex + Age,
    data = titanic_long,
    family = 'binomial'
)

summary(titanic_model)

table(titanic_long$Age, titanic_long$Survived)

child_risk <- 57 / (57 + 52)
child_risk

adult_risk <- 654 / (654 + 1438)
adult_risk

child_risk / adult_risk
