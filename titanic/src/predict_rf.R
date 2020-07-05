library("randomForest")

titanic_rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp +
                               Parch + Fare + Embarked + title_group +
                               family_category + is_child + is_mother,
                           data = train)

titanic_pred3 <- predict(titanic_rf, test)

test_full3 <- cbind(test, titanic_pred3) %>%
    select(PassengerId, titanic_pred3) %>%
    rename(Survived = titanic_pred3)

write_csv(test_full3, "titanic/res/randomforest01.csv")
