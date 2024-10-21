# Load necessary libraries
library(dplyr)
library(stringi)

# Set a random seed for reproducibility
set.seed(123)

# Number of samples
n_samples <- 1000

# Helper function to generate review text based on loan purpose and writing style
generate_review_text <- function(purpose, style) {
  reviews <- list(
    Home = list(
      Formal = c(
        "I require a loan to renovate my house, specifically to overhaul the kitchen. This renovation will significantly enhance the value of my property. Additionally, I plan to upgrade the plumbing and electrical systems to ensure everything is up to code. The loan will be essential for these improvements.",
        "I am building a new home for my family and need funds to commence the project. We have already acquired the land, and the loan will help cover construction costs. Our goal is to complete the build within the next twelve months."
      ),
      Informal = c(
        "Need some cash to fix up my house, especially the kitchen. It really needs a lot of work! Also thinking about updating the plumbing and electrical stuff. This loan would be super helpful.",
        "Starting to build a new home for my family, and we need some money to get going. We already bought the land, and the loan will help us with the construction costs. Hoping to finish it in a year."
      ),
      Descriptive = c(
        "My house is in desperate need of a loan for renovation, focusing mainly on the kitchen. This project will greatly increase the overall value of my home. Furthermore, I intend to upgrade the plumbing and electrical systems to modern standards, ensuring safety and efficiency.",
        "We are embarking on the exciting journey of building a new family home and require funds to initiate the project. The land is already purchased, and the loan will aid in covering the construction expenses, with a completion target of twelve months."
      ),
      Concise = c(
        "Need a loan for kitchen renovation and system upgrades. Will enhance home value.",
        "Building a new home, need funds for construction. Land bought, goal: finish in a year."
      ),
      Narrative = c(
        "When we bought our current home, we knew it needed some work, but now it's time for a major renovation. We're particularly focused on the kitchen, which is the heart of our home. With this loan, we can update the plumbing and electrical systems too, making our house safer and more comfortable for years to come.",
        "After years of saving, we finally purchased a plot of land to build our dream home. This loan will be the key to starting construction, allowing us to cover the costs and move forward with our plans. We're excited to see our vision come to life within the next year."
      )
    ),
    Car = list(
      Formal = c(
        "I am seeking a loan to purchase a new vehicle to replace my current one, which is beyond repair. A reliable car is essential for my daily commute to work. The loan will facilitate the acquisition of a dependable and fuel-efficient vehicle, ensuring smooth and efficient transportation.",
        "I need a loan to buy a used car for commuting to my workplace. Public transportation options are limited, and owning a car will significantly reduce travel time. I have identified a well-maintained vehicle that fits within my budget."
      ),
      Informal = c(
        "Looking to get some cash to buy a new car since my old one is just done for. Really need a reliable car for getting to work every day. This loan will help me get something that's good on gas and dependable.",
        "Need a loan to get a used car for getting to work. Public transport isn't great where I live, and a car will save a lot of time. Found a good used one that's within my budget."
      ),
      Descriptive = c(
        "My current vehicle has become unreliable and is beyond repair, necessitating the purchase of a new car. Reliable transportation is crucial for my daily commute. This loan will enable me to acquire a new, fuel-efficient car, ensuring I can travel safely and efficiently.",
        "Given the limited public transportation options in my area, I require a loan to purchase a used car for my daily commute. Owning a car will drastically reduce my travel time. I have found a well-maintained vehicle that suits my budget and needs."
      ),
      Concise = c(
        "Need loan for new car, current one is broken. Essential for commute.",
        "Seeking loan for used car due to limited public transport. Found one in budget."
      ),
      Narrative = c(
        "I've been struggling with my old car for months, and it's finally given up. Every morning, the uncertainty of whether it would start has been stressful. This loan will allow me to purchase a reliable and fuel-efficient vehicle, ensuring I can get to work without any hiccups. It's a necessary step for my daily routine.",
        "Public transportation in my area is almost non-existent, making daily commutes a challenge. After much research, I found a used car that fits my needs and budget perfectly. A loan will help me secure this vehicle, cutting down my commute time and providing the convenience I need."
      )
    ),
    Education = list(
      Formal = c(
        "I am in need of a loan to cover my college tuition and living expenses. My current savings are insufficient to meet these costs. This loan will allow me to concentrate on my studies without financial stress. I am pursuing a degree in computer science, which offers promising career prospects.",
        "I require financial assistance to cover my educational expenses, including books and supplies. These costs have exceeded my initial budget. The loan will ensure I have all necessary materials for my courses, enabling me to maintain a high GPA and graduate on time."
      ),
      Informal = c(
        "Need some money for college tuition and living costs. My savings just aren't enough. This loan will help me focus on my studies without worrying about money. I'm studying computer science, and it's got good job prospects.",
        "Looking for a loan to cover educational costs like books and supplies. These expenses are higher than I thought. The loan will make sure I have everything I need for my courses and help me keep my grades up."
      ),
      Descriptive = c(
        "I am seeking a loan to fund my college education, specifically for tuition and living expenses. My savings are insufficient to cover the total costs. This loan will alleviate financial pressures, allowing me to focus on my computer science studies, which hold significant career potential.",
        "Due to unexpectedly high educational expenses, including books and supplies, I need a loan to bridge the gap. This financial support will ensure I have all the resources necessary for my courses, helping me maintain my GPA and stay on track for graduation."
      ),
      Concise = c(
        "Loan needed for college tuition and living expenses. Studying computer science.",
        "Seeking loan for educational costs like books. Need to maintain GPA."
      ),
      Narrative = c(
        "As I embark on my college journey, the financial burden has become overwhelming. My savings are quickly depleting, and I still have tuition and living expenses to cover. This loan would provide the financial relief I need to focus on my computer science degree, which promises a bright future.",
        "Starting college has been a dream come true, but the costs are more than I anticipated. With books and supplies adding up, a loan will ensure I have everything I need to succeed in my studies. Maintaining a high GPA is crucial, and this financial support will help me achieve that goal."
      )
    ),
    Business = list(
      Formal = c(
        "I am requesting a loan to start a new business, covering initial expenses such as equipment and inventory. The funds will be instrumental in setting up the business, which has strong market potential. I have prepared a comprehensive business plan that outlines a clear path to profitability.",
        "I need financial assistance to expand my existing business by purchasing additional equipment. This expansion will increase production capacity and meet growing customer demand. The loan will also support hiring additional staff, ensuring smooth business operations."
      ),
      Informal = c(
        "Starting a new business and need some cash for the initial setup like equipment and inventory. This loan will be crucial to getting things started. I have a solid business plan and see good potential for making profits.",
        "Need a loan to expand my current business by buying more equipment. This will help us produce more and meet customer demand. We'll also be hiring more staff to keep things running smoothly."
      ),
      Descriptive = c(
        "I am seeking a loan to launch a new business, focusing on initial expenses such as purchasing equipment and inventory. The funds will play a crucial role in establishing the business, which has significant market potential. My detailed business plan outlines a clear strategy for achieving profitability.",
        "To meet increasing customer demand, I need a loan to expand my existing business by acquiring additional equipment. This expansion will boost our production capacity. The loan will also facilitate hiring more staff, ensuring efficient business operations."
      ),
      Concise = c(
        "Loan needed to start new business. Funds for equipment and inventory.",
        "Seeking loan to expand business. Need more equipment and staff."
      ),
      Narrative = c(
        "Starting a new business has always been my dream, and now I'm ready to take the plunge. With the funds from this loan, I can purchase the necessary equipment and inventory to get started. I've crafted a detailed business plan that charts a path to success and profitability.",
        "Our business has seen tremendous growth, and we're ready to expand. The loan will allow us to purchase additional equipment and hire more staff to keep up with demand. This expansion is a pivotal moment for us, and the financial support will ensure we continue to thrive."
      )
    ),
    Other = list(
      Formal = c(
        "I am seeking a loan to cover personal expenses, including medical bills and home repairs. These expenses are urgent and cannot be postponed. The loan will enable me to manage these costs without depleting my savings, and I am committed to timely repayment.",
        "I require financial support for unexpected expenses that have arisen, such as car repairs and urgent home maintenance. The loan will provide the necessary funds to address these issues promptly, ensuring my financial stability is maintained."
      ),
      Informal = c(
        "Need some money for personal expenses like medical bills and fixing up the house. These costs can't wait. The loan will help me cover these without using up all my savings. I'll make sure to repay it on time.",
        "Looking for a loan to handle unexpected costs that have come up, like car repairs and home maintenance. This loan will give me the funds I need to take care of these issues quickly."
      ),
      Descriptive = c(
        "I am in urgent need of a loan to cover personal expenses, specifically medical bills and necessary home repairs. These costs are pressing and cannot be delayed. The loan will help me manage these expenses without draining my savings, and I am committed to repaying the loan on schedule.",
        "Due to unforeseen circumstances, I require financial assistance to address expenses such as car repairs and urgent home maintenance. The loan will provide the funds needed to handle these issues promptly, ensuring my financial stability remains intact."
      ),
      Concise = c(
        "Loan needed for medical bills and home repairs. Expenses are urgent.",
        "Seeking loan for unexpected car repairs and home maintenance."
      ),
      Narrative = c(
        "Recently, I've been faced with several unexpected expenses that have strained my finances. Medical bills and urgent home repairs are piling up, and this loan will provide the relief I need. With this support, I can manage these costs without depleting my savings and ensure everything is taken care of promptly.",
        "Life threw a few surprises my way with sudden car repairs and urgent home maintenance. This loan will help me address these issues quickly, providing the necessary funds to keep things running smoothly. It's a crucial step to maintain my financial stability during this challenging time."
      )
    )
  )
  sample(reviews[[purpose]][[style]], 1)
}

# Generate synthetic data with meaningful associations
CustomerID <- 1:n_samples
LoanPurpose <- sample(c('Home', 'Car', 'Education', 'Business', 'Other'), n_samples, replace = TRUE)
WritingStyle <- sample(c('Formal', 'Informal', 'Descriptive', 'Concise', 'Narrative'), n_samples, replace = TRUE)

AnnualIncome <- runif(n_samples, 20000, 150000)
CreditScore <- rnorm(n_samples, mean = 700, sd = 50)
CreditScore <- ifelse(CreditScore < 300, 300, ifelse(CreditScore > 850, 850, CreditScore))
LoanAmount <- ifelse(LoanPurpose == 'Home', runif(n_samples, 20000, 50000),
                     ifelse(LoanPurpose == 'Car', runif(n_samples, 5000, 30000),
                            ifelse(LoanPurpose == 'Education', runif(n_samples, 10000, 40000),
                                   ifelse(LoanPurpose == 'Business', runif(n_samples, 15000, 45000),
                                          runif(n_samples, 5000, 20000)))))
InterestRate <- ifelse(CreditScore < 600, runif(n_samples, 15, 20),
                       ifelse(CreditScore < 700, runif(n_samples, 10, 15),
                              runif(n_samples, 3, 10)))
LoanTerm <- sample(c(12, 24, 36, 48, 60), n_samples, replace = TRUE)
EmploymentStatus <- sample(c('Employed', 'Unemployed', 'Self-Employed'), n_samples, replace = TRUE)

# Generate ReviewText based on LoanPurpose and WritingStyle
ReviewText <- mapply(generate_review_text, LoanPurpose, WritingStyle)

# Generate realistic Default variable using logistic regression probabilities
# Adjusted coefficients and intercept to achieve ~5% default rate
intercept <- -5
coefficients <- c(
  CreditScore = -0.05, 
  LoanAmount = 0.0065, 
  AnnualIncome = -0.005,
  EmploymentStatus = 1.0, 
  InterestRate = 0.22
)

# Calculate the linear predictor
linear_predictor <- intercept +
  coefficients['CreditScore'] * CreditScore +
  coefficients['LoanAmount'] * LoanAmount +
  coefficients['AnnualIncome'] * AnnualIncome +
  coefficients['EmploymentStatus'] * as.numeric(EmploymentStatus == 'Unemployed') +
  coefficients['InterestRate'] * InterestRate

# Convert to probability using the logistic function
probability <- 1 / (1 + exp(-linear_predictor))

# Simulate Default outcome based on the probability
Default <- ifelse(runif(n_samples) < probability, 'Yes', 'No')

# Create the data frame
loan_data <- data.frame(
  CustomerID,
  LoanAmount,
  InterestRate,
  LoanTerm,
  CreditScore,
  EmploymentStatus,
  AnnualIncome,
  LoanPurpose,
  Default,
  ReviewText,
  WritingStyle,
  stringsAsFactors = FALSE
)

# Check the proportion of defaulters
prop.table(table(Default))

# Save the dataset to a CSV file
write.csv(loan_data, "loan_data.csv", row.names = FALSE)

# Print the first few rows of the dataset
head(loan_data)
