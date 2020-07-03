library("tidyverse")
library("viridis")

train <- read_csv("titanic/data/csv/train.csv", col_types = "iiiccdiicdcc")
test  <- read_csv("titanic/data/csv/test.csv",  col_types = "iiccdiicdcc")

full <- train

# missing values ----------------------------------------------------------

train_na <- train %>%
    gather(variable, value, -PassengerId) %>%
    mutate(is_na = ifelse(is.na(value), "Yes", "No"))

ggplot(train_na, aes(x = PassengerId, y = variable, fill = is_na)) +
    geom_tile() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal()

# missing embarked value --------------------------------------------------

train_embarked <- train %>%
    filter(!is.na(Embarked))

ggplot(train_embarked, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal()

full$Embarked[is.na(full$Embarked)] <- 'C'

# logistic regression -----------------------------------------------------

titanic_logistic <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                        data = train, family = binomial)

titanic_pred <- predict(titanic_logistic, test)
titanic_pred <- ifelse(titanic_pred > 0.5, 1, 0)

test_full <- cbind(test, titanic_pred) %>%
    select(PassengerId, titanic_pred) %>%
    rename(Survived = titanic_pred)

write_csv(test_full, "titanic/res/submission01.csv")
