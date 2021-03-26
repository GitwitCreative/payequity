#if (interactive()) {
  ui <- fluidPage(
    titlePanel("Pay Equity Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr()
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Instructions", h3("Pay Equity Analysis Instructions"),
                   markdown("
                            1. Generate a .csv file with the following names:
                                + salary - numbers only without currency formatting
                                + gender - either Male or Female
                                + tenure - number of years at Gitwit
                                + age
                                + education - any textual description of education. We suggest using the following categories:
                                  + InCollege
                                  + Bachelors
                                  + Masters
                                + joblevel - integer coded variable representing seniority. We suggest using the following structure ranging from 1-6:
                                  + 1: Intern
                                  + 2: Junior
                                  + 3: Mid
                                  + 4: Senior
                                  + 5: Director
                                  + 6: Principal
                                + team - any textual description of team. Use as few categories as necessary, such as:
                                  + Concept
                                  + Design
                                  + Operations
                                  + Data
                                  + Motion
                                + An example .csv can be viewed in the Example CSV tab.
                            2. Upload .csv file using tool in sidebar.
                            3. View your uploaded data in the My CSV tab. Ensure the data is accurate.
                            4. Click the Summary tab. This shows the unadjusted mean and median pay for men and women.
                            5. Click the Regression Results tab. This shows the OLS model output for your data.
                                + The percent impact of each variable is shown in the coefficient column.
                                + Variables where p>0.05 did not have a significant impact on salary.")),
          tabPanel("Example CSV", tableOutput("example")),
          tabPanel("My CSV", tableOutput("contents")),
          tabPanel("Summary", tableOutput("summary")),
          tabPanel("Regression Results", tableOutput("regression")),
          tabPanel("Residuals", tableOutput("restbl"))
        )
      )
    )
  )
#}