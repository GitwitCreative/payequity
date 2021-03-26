library(shiny)
library(dplyr)
library(DT)
library(formattable)
library(ggResidpanel)

function(input, output) {
  output$example=renderTable({
    data=read.csv("example_csv.csv")
  })
    
    output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return("Upload a csv.")
    
    df=read.csv(inFile$datapath)
  })
  output$summary <- renderTable({
    inFile2 <- input$file1
    
    if (is.null(inFile2))
      return("Upload a csv.")
    
    data=read.csv(inFile2$datapath)
    
    summary_base <- group_by(data, gender)
    summary_base <- summarise(summary_base, meanSalary =mean(salary, na.rm = TRUE), medSalary = median(salary, na.rm = TRUE), cnt = sum(!(is.na(salary))))
  })
  output$regression <-renderTable({
    inFile2 <- input$file1
    
    if (is.null(inFile2))
      return(NULL)
    
    data=read.csv(inFile2$datapath)
    
    # Log transform salaries
    data$log_salary <- log(data$salary, base = exp(1))
    
    # Create five employee age bins.
    data$age_bin <- 0
    data$age_bin <- ifelse(data$age < 25, 1, data$age_bin) # Below age 25
    data$age_bin <- ifelse(data$age >= 25 & data$age < 30, 2, data$age_bin) # Age 25-29.
    data$age_bin <- ifelse(data$age >= 30 & data$age < 35, 3, data$age_bin) # Age 30-34.
    data$age_bin <- ifelse(data$age >= 35 & data$age < 40, 4, data$age_bin) # Age 35-39.
    data$age_bin <- ifelse(data$age >= 40, 5, data$age_bin) # Age 40+.
    
    # Create gender dummies (male = 1, female = 0. 
    data$male <- ifelse(data$gender == "Male", 1, 0) # Male = 1, Female = 0.
    
    # Cast all categorical variable as factors for the regression analysis. 
    data$gender <- as.factor(data$gender)
    data$education <- as.factor(data$education)
    data$team <- as.factor(data$team)
    
    # No controls. ("unadjusted" pay gap.)
    #model1 <- lm(log_salary ~ male, data = data)
    
    # Adding "human capital" controls (performance evals, age and education).
    #model2 <- lm(log_salary ~ male + age_bin + education, data = data)
    
    # Adding all controls. ("adjusted" pay gap.)
    model3 <- lm(log_salary ~ male + age_bin + education + team + joblevel + tenure, data = data)
    df=as.data.frame(coef(summary(model3)))
  }, rownames=TRUE)
  output$residuals <-renderPlot({
    inFile2 <- input$file1
    
    if (is.null(inFile2))
      return(NULL)
    
    data=read.csv(inFile2$datapath)
    
    # Log transform salaries
    data$log_salary <- log(data$salary, base = exp(1))
    
    # Create five employee age bins.
    data$age_bin <- 0
    data$age_bin <- ifelse(data$age < 25, 1, data$age_bin) # Below age 25
    data$age_bin <- ifelse(data$age >= 25 & data$age < 30, 2, data$age_bin) # Age 25-29.
    data$age_bin <- ifelse(data$age >= 30 & data$age < 35, 3, data$age_bin) # Age 30-34.
    data$age_bin <- ifelse(data$age >= 35 & data$age < 40, 4, data$age_bin) # Age 35-39.
    data$age_bin <- ifelse(data$age >= 40, 5, data$age_bin) # Age 40+.
    
    # Create gender dummies (male = 1, female = 0. 
    data$male <- ifelse(data$gender == "Male", 1, 0) # Male = 1, Female = 0.
    
    # Cast all categorical variable as factors for the regression analysis. 
    data$gender <- as.factor(data$gender)
    data$education <- as.factor(data$education)
    data$team <- as.factor(data$team)
    
    # No controls. ("unadjusted" pay gap.)
    #model1 <- lm(log_salary ~ male, data = data)
    
    # Adding "human capital" controls (performance evals, age and education).
    #model2 <- lm(log_salary ~ male + age_bin + education, data = data)
    
    # Adding all controls. ("adjusted" pay gap.)
    model3 <- lm(salary ~ male + age_bin + education + team + joblevel + tenure, data = data)
    plot=resid_interact(model3,plots="yvp")
  })
  output$restbl <-renderTable({
    inFile2 <- input$file1
    
    if (is.null(inFile2))
      return(NULL)
    
    data=read.csv(inFile2$datapath)
    
    # Log transform salaries
    #data$log_salary <- log(data$salary, base = exp(1))
    
    # Create five employee age bins.
    data$age_bin <- 0
    data$age_bin <- ifelse(data$age < 25, 1, data$age_bin) # Below age 25
    data$age_bin <- ifelse(data$age >= 25 & data$age < 30, 2, data$age_bin) # Age 25-29.
    data$age_bin <- ifelse(data$age >= 30 & data$age < 35, 3, data$age_bin) # Age 30-34.
    data$age_bin <- ifelse(data$age >= 35 & data$age < 40, 4, data$age_bin) # Age 35-39.
    data$age_bin <- ifelse(data$age >= 40, 5, data$age_bin) # Age 40+.
    
    # Create gender dummies (male = 1, female = 0. 
    data$male <- ifelse(data$gender == "Male", 1, 0) # Male = 1, Female = 0.
    
    # Cast all categorical variable as factors for the regression analysis. 
    data$gender <- as.factor(data$gender)
    data$education <- as.factor(data$education)
    data$team <- as.factor(data$team)
    
    # No controls. ("unadjusted" pay gap.)
    #model1 <- lm(log_salary ~ male, data = data)
    
    # Adding "human capital" controls (performance evals, age and education).
    #model2 <- lm(log_salary ~ male + age_bin + education, data = data)
    
    # Adding all controls. ("adjusted" pay gap.)
    model3 <- lm(salary ~ male + age_bin + education + team + joblevel + tenure, data = data)
    data$residuals=residuals.lm(model3)
    data$predictedsalary=predict.lm(model3)
    data <- data[order(data$residuals),]
  })
}