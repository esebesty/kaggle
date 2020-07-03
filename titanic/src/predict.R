library("tidyverse")
library("viridis")
library("mice")

train <- read_csv("titanic/data/csv/train.csv", col_types = "iiiccdiicdcc") %>%
    mutate(data_group = "train")

test  <- read_csv("titanic/data/csv/test.csv",  col_types = "iiccdiicdcc") %>%
    mutate(data_group = "test") %>%
    mutate(Survived = NA)

test <- test[, c(1, 13, 2:12)]
full <- rbind(train, test)

# missing values ----------------------------------------------------------

full_na <- full %>%
    gather(variable, value, -PassengerId) %>%
    mutate(is_na = ifelse(is.na(value), "Yes", "No"))

ggplot(full_na, aes(x = PassengerId, y = variable, fill = is_na)) +
    geom_tile() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal()

# missing embarked value --------------------------------------------------

full_embarked <- full %>%
    filter(!is.na(Embarked)) %>%
    filter(!is.na(Fare))

ggplot(full_embarked, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal()

full$Embarked[is.na(full$Embarked)] <- 'C'

# missing fare value ------------------------------------------------------

full_fare <- full %>%
    filter(Pclass == 3 & Embarked == "S" & !is.na(Fare))

ggplot(full_fare, aes(x = Fare)) +
    geom_line(stat = "density") +
    geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)),
               color = "red", linetype = "dashed") +
    theme_minimal()

full$Fare[is.na(full$Fare)] <- median(full[full$Pclass == '3' &
                                               full$Embarked == 'S', ]$Fare,
                                      na.rm = TRUE)

# missing age -------------------------------------------------------------

full <- full %>%
    mutate(PassengerId = as.factor(PassengerId)) %>%
    mutate(Pclass = as.factor(Pclass)) %>%
    mutate(Sex = as.factor(Sex)) %>%
    mutate(Embarked = as.factor(Embarked))

exclude <- c("PassengerId", "Name", "Ticket", "Cabin", "Survived", "data_group")

full_mice <- full[, colnames(full)[!(colnames(full) %in% exclude)]]

mice_mod    <- mice(full_mice, method = "rf")
mice_output <- complete(mice_mod)

full$Age <- mice_output$Age

# logistic regression -----------------------------------------------------

train <- full[1:891, ]
test  <- full[892:1309, ]

titanic_logistic <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch +
                            Fare + Embarked,
                        data = train, family = binomial)

titanic_pred <- predict(titanic_logistic, test)
titanic_pred <- ifelse(titanic_pred > 0.5, 1, 0)

test_full <- cbind(test, titanic_pred) %>%
    select(PassengerId, titanic_pred) %>%
    rename(Survived = titanic_pred)

write_csv(test_full, "titanic/res/submission01.csv")
