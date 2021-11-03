# 0. Importing R libraries
library(data.table)           
library(readxl)               
library(tidyverse)
library(lubridate)
library(skimr)                
library(knitr)                
library(treemap)
library(httr)
library(recommenderlab)

# 1. Get UC Irvine online retail dataset
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx'
httr::GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
retail <- read_excel(tf, 1L)

# 2. Prepare data
retail %>%  skim()

# Remove cancelled orders
retail %>% 
  filter(grepl("C", retail$InvoiceNo)) %>% 
  summarise(Total = n())
retail  <- retail %>% 
  filter(!grepl("C", retail$InvoiceNo))

# Remove non-positive number of products
retail %>% 
  filter(Quantity <= 0) %>% 
  group_by(Description, UnitPrice) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()
retail  <- retail %>% 
  filter(Quantity > 0)

# Remove non-product related codes
stc <- c('AMAZONFEE', 'BANK CHARGES', 'C2', 'DCGSSBOY',
         'DCGSSGIRL', 'DOT', 'gift_0001_', 'PADS', 'POST')
retail %>%  
  filter(grepl(paste(stc, collapse="|"), StockCode))  %>% 
  group_by(StockCode, Description) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()
retail <- filter(retail, 
                 !grepl(paste(stc, collapse="|"), StockCode))

# Additional adjustment codes to remove
descr <- c( "check", "check?", "?", "??", "damaged", "found", 
            "adjustment", "Amazon", "AMAZON", "amazon adjust", 
            "Amazon Adjustment", "amazon sales", "Found", "FOUND",
            "found box", "Found by jackie ","Found in w/hse","dotcom", 
            "dotcom adjust", "allocate stock for dotcom orders ta", "FBA", 
            "Dotcomgiftshop Gift Voucher £100.00", "on cargo order",
            "wrongly sold (22719) barcode", "wrongly marked 23343",
            "dotcomstock", "rcvd be air temp fix for dotcom sit", 
            "Manual", "John Lewis", "had been put aside", 
            "for online retail orders", "taig adjust", "amazon", 
            "incorrectly credited C550456 see 47", "returned", 
            "wrongly coded 20713", "came coded as 20713", 
            "add stock to allocate online orders", "Adjust bad debt", 
            "alan hodge cant mamage this section", "website fixed",
            "did  a credit  and did not tick ret", "michel oops",
            "incorrectly credited C550456 see 47", "mailout", "test",
            "Sale error",  "Lighthouse Trading zero invc incorr", "SAMPLES",
            "Marked as 23343", "wrongly coded 23343","Adjustment", 
            "rcvd be air temp fix for dotcom sit", "Had been put aside." )

# Filtering out the unwanted entries.
retail <- retail %>% 
  filter(!Description %in% descr)

# Remove NAs
sum(is.na(retail$Description))
retail <- retail %>% 
  filter(!is.na(Description))

# Use 'InvoiceNo' as orders for the analysis
retail$CustomerID %>%  
  skim()
sapply(retail[ ,c('InvoiceNo','CustomerID')], 
       function(x) length(unique(x)))

# Final cleansing
retail <- retail %>%
  # Setting 'Description' and 'Country' as factors
  mutate(Description = as.factor(Description)) %>%
  mutate(Country = as.factor(Country)) %>% 
  # Changing 'InvoiceNo' type to numeric
  mutate(InvoiceNo = as.numeric(InvoiceNo)) %>% 
  # Extracting 'Date' and 'Time' from 'InvoiceDate'
  mutate(Date = as.Date(InvoiceDate)) %>% 
  mutate(Time = as.factor(format(InvoiceDate,"%H:%M:%S")))

glimpse(retail)

# 3. Exploring the data
retail %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(Description, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
  coord_flip() +
  theme_grey(base_size = 12)

retail %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  mutate(pct=(count/sum(count))*100) %>% 
  arrange(desc(pct)) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)

retail %>% 
  ggplot(aes(hour(hms(Time)))) + 
  geom_histogram(stat = "count",fill = "#E69F00", colour = "red") +
  labs(x = "Hour of Day", y = "") +
  theme_grey(base_size = 12)

retail %>% 
  ggplot(aes(wday(Date, 
                  week_start = getOption("lubridate.week.start", 1)))) + 
  geom_histogram(stat = "count" , fill = "forest green", colour = "dark green") +
  labs(x = "Day of Week", y = "") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                     labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_grey(base_size = 14)

retail %>% 
  group_by(InvoiceNo) %>% 
  summarise(n = mean(Quantity)) %>%
  ggplot(aes(x=n)) +
  geom_histogram(bins = 100000,fill = "purple",colour = "black") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = "Average Number of Items per Purchase", y = "") +
  theme_grey(base_size = 14)

retail %>% 
  mutate(Value = UnitPrice * Quantity) %>% 
  group_by(InvoiceNo) %>% 
  summarise(n = mean(Value)) %>%
  ggplot(aes(x=n)) +
  geom_histogram(bins = 200000, fill="firebrick3", colour = "sandybrown") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = "Average Value per Purchase", y = "") + 
  theme_grey(base_size = 14)

treemap(retail,
        index      = c("Country"),
        vSize      = "Quantity",
        title      = "",
        palette    = "Set2",
        border.col = "grey40")

# 4. Removing duplicates 
retail <- retail %>%
  mutate(InNo_Desc = paste(InvoiceNo, Description, sep = ' ')) 
retail <- retail[!duplicated(retail$InNo_Desc), ] %>% 
  select(-InNo_Desc)

# 5. Create binary rating matrix 
ratings_matrix <- retail %>%
  select(InvoiceNo, Description) %>% 
  mutate(value = 1) %>%
  spread(Description, value, fill = 0) %>%
  select(-InvoiceNo) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")

gc()

# 6. Create evaluation scheme
scheme <- ratings_matrix %>% 
  evaluationScheme(method = "cross",
                   k      = 5, 
                   train  = 0.8,
                   given  = -1)

# 7. Set up List of Algorithms
algorithms <- list(
  "association rules" = list(name  = "AR", param = list(supp = 0.01, conf = 0.01)),
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 5)),
  "user-based CF"     = list(name  = "UBCF", param = list(method = "Cosine", nn = 500))
)

# 8. Estimate the Models
results <- recommenderlab::evaluate(scheme, 
                                    algorithms, 
                                    type  = "topNList", 
                                    n     = c(1, 3, 5, 10, 15, 20)
)

# 9. Evaluate models

# Retrieve results for each single model 
results$'popular' %>% 
  getConfusionMatrix() 

# Sort out performance metrics (results)
tmp <- results$`user-based CF` %>% 
  getConfusionMatrix()  %>%  
  as.list() 
as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
  mutate(n = c(1, 3, 5, 10, 15, 20)) %>% 
  select('n', 'precision', 'recall', 'TPR', 'FPR') 
avg_conf_matr <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}
results_tbl <- results %>%
  map(avg_conf_matr) %>%
  enframe() %>% 
  unnest()

# ROC curve
results_tbl %>%
  ggplot(aes(FPR, TPR, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# Precision-Recall curve
results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# 10. Obtain predictions for a new user

# Create a made-up order with 6 products selected at random.
customer_order <- c("GREEN REGENCY TEACUP AND SAUCER",
                    "SET OF 3 BUTTERFLY COOKIE CUTTERS",
                    "JAM MAKING SET WITH JARS",
                    "SET OF TEA COFFEE SUGAR TINS PANTRY",
                    "SET OF 4 PANTRY JELLY MOULDS")

# Put string in a format that recommenderlab accepts.
new_order_rat_matrx <- retail %>% 
  select(Description) %>% 
  unique() %>% 
  mutate(value = as.numeric(Description %in% customer_order)) %>% 
  spread(key = Description, value = value) %>%
  as.matrix() %>% 
  as("binaryRatingMatrix")

# Create a Recommender
recomm <- Recommender(getData(scheme, 'train'), 
                      method = "IBCF",   
                      param = list(k = 5))

# Pass the Recommender and the made-up order to the predict function to create 
# a top 10 recommendation list for the new customer.
pred <- predict(recomm, 
                newdata = new_order_rat_matrx, 
                n       = 10)

# Inspect prediction as a list
as(pred, 'list')