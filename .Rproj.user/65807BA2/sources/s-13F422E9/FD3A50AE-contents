library(FinancialMath)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(scales)

AssetPrice = 800000
LVR = 70 / 100

Deposit = AssetPrice*(1-LVR)
LoanAmount = AssetPrice*LVR

LoanPeriod = 30 * 12

Interest = 3 / 100


x <- amort.table(Loan = LoanAmount, n = LoanPeriod, i = Interest, ic = 12, pf = 12)
y <- amort.period(Loan = LoanAmount, n = LoanPeriod, i = Interest, ic = 12, pf = 12)[2,]

x1 <- x$Schedule %>% as.data.frame()

p1 <- ggplot(data = x1, mapping = aes(x = Year)) + theme_bw() + 
  geom_line(aes(y = Balance), size=1, colour="darkblue") + 
  labs(title = "Loan amount outstanding", y = NULL) +
  scale_y_continuous(labels = dollar_format())
p3 <- ggplotly(p1)
p3

p1 + geom_line(aes(y = `Interest Paid`)) + geom_line(aes(y = `Principal Paid`)) 

x2 <- x1 %>% gather(key = PI, value = paid, `Interest Paid`, `Principal Paid`) %>% 
  select(Year, PI, paid)

p2 <- ggplot(data = x2, aes(x = Year, y = paid))+ geom_col(aes(fill = PI))+theme_classic()

p4 <- ggplotly(p2)

subplot(p3, p4, shareX = TRUE, nrows = 2)




