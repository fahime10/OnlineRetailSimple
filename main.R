# Import and set up
install.packages("tidyverse")
install.packages("readxl")

library(tidyverse)
library(readxl)

df <- read_xlsx("./data/Online Retail.xlsx")

# Explore dataset
head(df)
str(df)

# Pre-processing
print(colSums(is.na(df)))

cleaned_df <- df %>% 
  filter(!is.na(CustomerID))

print(any(df$Quantity < 0))

cleaned_df <- cleaned_df %>%
  filter(Quantity > 0)

cleaned_df <- cleaned_df %>%
  mutate(Revenue = Quantity * UnitPrice)

cleaned_df <- cleaned_df %>%
  mutate(InvoiceDate = as.Date(InvoiceDate))

# Process
customer_segment <- cleaned_df %>%
  group_by(CustomerID) %>%
  summarise(TotalRevenue = sum(Revenue),
            TotalOrders = n(),
            AvgRevenuePerOrder = mean(Revenue)) %>%
  arrange(desc(TotalRevenue))

View(customer_segment)

customer_segment <- customer_segment %>%
  mutate(Segment = case_when(
    TotalRevenue >= quantile(TotalRevenue, 0.75) ~ "High",
    TotalRevenue >= quantile(TotalRevenue, 0.25) ~ "Medium",
    TRUE ~ "Low"
  ))

View(customer_segment)

# Analysis
ggplot(customer_segment, aes(x = Segment, fill = Segment)) +
  geom_bar() +
  labs(title = "Customer Segmentation based on Revenue",
       y = "Number of customers") +
  theme_minimal()

top_10 <- customer_segment %>% top_n(10, TotalRevenue)

ggplot(top_10, aes(x = reorder(CustomerID, TotalRevenue), y = TotalRevenue)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +
  labs(title = "Top 10 Customers by Revenue", x = "CustomerID", y = "Total Revenue") +
  theme_minimal()