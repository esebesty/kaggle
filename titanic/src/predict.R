library("tidyverse")

train <- read_csv("titanic/data/csv/train.csv", col_types = "iiiccdiicdcc")
test  <- read_csv("titanic/data/csv/test.csv",  col_types = "iiccdiicdcc")

# TODO
# deal with NA values

titanic_logistic <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                        data = train, family = binomial)

titanic_pred <- predict(titanic_logistic, test)
titanic_pred <- ifelse(titanic_pred > 0.5, 1, 0)

test_full <- cbind(test, titanic_pred) %>%
    select(PassengerId, titanic_pred) %>%
    rename(Survived = titanic_pred)

write_csv(test_full, "titanic/res/submission01.csv")


