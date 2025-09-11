#Polynomial Regression

#Importing the data set

dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# 
# library(caTools)

#Fitting the Linear Reg model to the data set
lin_reg = lm(formula = Salary ~ . ,
             data = dataset)

#Fitting the Polynomial Reg model to the data set
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3

poly_reg = lm(formula = Salary~ . ,
              data = dataset)

# Visualising the Linear Regression model 
library(ggplot2)
ggplot() + 
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colours = 'red')+
  geom_line(aes(x=dataset$Level,y= predict(lin_reg, newdata = dataset)),
            colour= 'blue')+
  ggtitle('Linear Regression results')+
  xlab('Level')+
  ylab('Salary')

# Visualising the Polynomial Regression Model
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue')+
  ggtitle('Polynomial Regression Results')+
  xlab('Level')+
  ylab('Salary')

# Predicting a new result with Linear Regression
y_pred = predict(lin_reg, data.frame(Level = 6.5))


# Predicting a new result with Polynomial Regression

y_pred2 = predict(poly_reg, data.frame(Level = 6.5,
                                       Level2 = 6.5^2,
                                       Level3 = 6.5^3))

