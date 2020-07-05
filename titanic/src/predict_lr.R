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

full_na_plot <- ggplot(full_na, aes(x = PassengerId, y = variable,
                                    fill = is_na)) +
    geom_tile() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal()

ggsave("titanic/fig/full_na_plot.pdf", full_na_plot, width = 6, height = 4,
       units = "in")

# missing embarked value --------------------------------------------------

full_embarked <- full %>%
    filter(!is.na(Embarked)) %>%
    filter(!is.na(Fare))

full_embarked_plot <- ggplot(full_embarked, aes(x = Embarked, y = Fare,
                                                fill = factor(Pclass))) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal()

ggsave("titanic/fig/full_embarked_plot.pdf", full_embarked_plot, width = 6,
       height = 4, units = "in")

full$Embarked[is.na(full$Embarked)] <- 'C'

# missing fare value ------------------------------------------------------

full_fare <- full %>%
    filter(Pclass == 3 & Embarked == "S" & !is.na(Fare))

full_fare_plot <- ggplot(full_fare, aes(x = Fare)) +
    geom_line(stat = "density") +
    geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)),
               color = "red", linetype = "dashed") +
    theme_minimal()

ggsave("titanic/fig/full_fare_plot.pdf", full_fare_plot, width = 6,
       height = 4, units = "in")

full$Fare[is.na(full$Fare)] <- median(full[full$Pclass == '3' &
                                               full$Embarked == 'S', ]$Fare,
                                      na.rm = TRUE)

# family ------------------------------------------------------------------

full <- full %>%
    mutate(surname = str_split_fixed(Name, "[\\.,]", 2)[, 1]) %>%
    mutate(family_size = SibSp + Parch + 1) %>%
    mutate(family = paste(surname, family_size, sep = "_")) %>%
    mutate(family_category = ifelse(family_size == 1, "Single",
                                    ifelse(family_size > 4, "Large", "Small")))

# do stuff with names -----------------------------------------------------

full <- full %>%
    mutate(title = str_replace(str_extract(Name, ", .+?\\."), ", ", "")) %>%
    mutate(title = str_replace(title, "\\.", "")) %>%
    group_by(title) %>%
    mutate(title_count = length(title)) %>%
    ungroup() %>%
    mutate(title_group = title) %>%
    mutate(title_group = ifelse(title_group == "Mlle", "Miss", title_group)) %>%
    mutate(title_group = ifelse(title_group == "Ms", "Miss", title_group)) %>%
    mutate(title_group = ifelse(title_group == "Mme", "Mrs", title_group)) %>%
    mutate(title_group = ifelse(title_group == "Dona", "Mrs", title_group)) %>%
    mutate(title_group = ifelse(title_group == "Don", "Mr", title_group)) %>%
    mutate(title_group = ifelse(title_group == "Jonkheer", "Mr", title_group)) %>%
    mutate(title_group = ifelse(title_group == "Ms", "Miss", title_group)) %>%
    mutate(title_group = ifelse(title_group %in% c("Mr", "Mrs", "Miss", "Master"),
                                title_group, "Rare")) %>%
    mutate(title_group = as.factor(title_group))

# missing age -------------------------------------------------------------

# probably fucking up something here as getting a message from mice
# Warning message:
# Number of logged events: 25

full <- full %>%
    mutate(PassengerId = as.factor(PassengerId)) %>%
    mutate(Pclass = as.factor(Pclass)) %>%
    mutate(Sex = as.factor(Sex)) %>%
    mutate(Embarked = as.factor(Embarked)) %>%
    mutate(title_group = as.factor(title_group)) %>%
    mutate(surname = as.factor(surname)) %>%
    mutate(family = as.factor(family)) %>%
    mutate(family_category = as.factor(family_category))

exclude <- c("PassengerId", "Name", "Ticket", "Cabin", "family", "surname",
             "Survived", "title", "title_count", "data_group")

full_mice <- full[, colnames(full)[!(colnames(full) %in% exclude)]]

mice_mod    <- mice(full_mice, method = "rf")
mice_output <- complete(mice_mod)

full_age_orig_plot <- ggplot(full, aes(x = Age)) +
    geom_histogram(color = "black", fill = "blue") +
    theme_minimal()

ggsave("titanic/fig/full_age_orig_plot.pdf", full_age_orig_plot, width = 6,
       height = 4, units = "in")

full_age_mice_plot <- ggplot(mice_output, aes(x = Age)) +
    geom_histogram(color = "black", fill = "blue") +
    theme_minimal()

ggsave("titanic/fig/full_age_mice_plot.pdf", full_age_mice_plot, width = 6,
       height = 4, units = "in")

full$Age <- mice_output$Age

# messing with other variables --------------------------------------------

full <- full %>%
    mutate(deck = str_extract(Cabin, ".")) %>%
    mutate(cabin_no = str_count(Cabin, "[[:alnum:]]+")) %>%
    mutate(is_child = ifelse(Age < 18, "Child", "Adult")) %>%
    mutate(is_mother = ifelse(Sex == "female" & Parch > 0 & Age > 18 &
                                  title != "Miss", "Yes", "No"))

# logistic regression -----------------------------------------------------

train <- full[1:891, ]
test  <- full[892:1309, ]

# prediction01 ------------------------------------------------------------

titanic_logistic1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                             Embarked,
                         data = train, family = binomial)

titanic_pred1 <- predict(titanic_logistic1, test)
titanic_pred1 <- ifelse(titanic_pred1 > 0.5, 1, 0)

test_full1 <- cbind(test, titanic_pred1) %>%
    select(PassengerId, titanic_pred1) %>%
    rename(Survived = titanic_pred1)

write_csv(test_full1, "titanic/res/logistic01.csv")

# prediction02 ------------------------------------------------------------

titanic_logistic2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                             Embarked + title_group + family_category +
                             is_child + is_mother,
                         data = train, family = binomial)

titanic_pred2 <- predict(titanic_logistic2, test)
titanic_pred2 <- ifelse(titanic_pred2 > 0.5, 1, 0)

test_full2 <- cbind(test, titanic_pred2) %>%
    select(PassengerId, titanic_pred2) %>%
    rename(Survived = titanic_pred2)

write_csv(test_full2, "titanic/res/logistic02.csv")
