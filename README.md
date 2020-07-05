# kaggle

stuff for kaggle competitions

## titanic

### first logistic regression

first prediction using logistic regression and some ideas from [here](https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic)

variables include `Pclass + Sex + Age + SibSp + Parch + Fare + Embarked` but
something changed since the very first submission, as the score is different

maybe age imputation has some issues?

### second logistic regression

variables include `Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title_group + family_category + is_child + is_mother`

this seems to be **worse** than the first logistic regression

## first random forest

same as second logistic regression, but with random forest

and this is also **worse** than the first logistic regression
