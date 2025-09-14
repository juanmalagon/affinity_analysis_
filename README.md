# Affinity Analysis for Market Basket Analysis

## 📋 Overview

This repository contains an R-based implementation of affinity analysis (market basket analysis) on online retail data. The project demonstrates how to use association rules and collaborative filtering techniques to identify product relationships and generate recommendations based on purchase patterns.

## 📊 Dataset

The analysis uses the **Online Retail Dataset** from the UCI Machine Learning Repository, which contains transaction data from a UK-based online retailer. The dataset includes:

- Invoice numbers
- Product descriptions
- Quantities
- Transaction timestamps
- Customer IDs
- Country information

## 🛠️ Implementation

### Dependencies

The analysis requires the following R packages:
- `data.table`
- `readxl`
- `tidyverse`
- `lubridate`
- `skimr`
- `knitr`
- `treemap`
- `httr`
- `recommenderlab`

### Methodology

1. **Data Acquisition**: Automatically downloads the dataset from UCI repository
2. **Data Cleaning**:
   - Removal of cancelled orders
   - Filtering non-positive quantities
   - Exclusion of non-product items
   - Handling missing values
   - Date/time extraction

3. **Exploratory Analysis**:
   - Top selling products visualization
   - Transaction patterns by time of day and day of week
   - Purchase size distribution
   - Geographic distribution of sales

4. **Model Development**:
   - Binary rating matrix creation
   - Multiple recommendation algorithms:
     - Association Rules (AR)
     - Random Items
     - Popular Items
     - Item-Based Collaborative Filtering (IBCF)
     - User-Based Collaborative Filtering (UBCF)

5. **Evaluation**:
   - Cross-validation scheme
   - ROC curve analysis
   - Precision-Recall evaluation

6. **Prediction**:
   - Example of generating recommendations for new users
   - Top-N product recommendations

## 📈 Key Insights

The analysis provides:
- Identification of frequently purchased together items
- Evaluation of different recommendation algorithms
- Visualization of purchase patterns and trends
- Framework for generating real-time product recommendations

## 🚀 Usage

1. Clone the repository
2. Install required R packages
3. Run the `affinity_analysis.R` script
4. Review generated visualizations and model evaluations

## 📚 References

1. Webber, R. (2013). The evolution of direct, data and digital marketing. Journal of Direct, Data and Digital Marketing Practice, 14, 291-309.
2. Singh, A., Rumantir, G., South, A., & Bethwaite, B. (2014). Clustering Experiments on Big Transaction Data for Market Segmentation. Proceedings of the 2014 International Conference on Big Data Science and Computing.
3. You, Z., Si, Y. W., Zhang, D., Zeng, X., Leung, S. C. H., & Li, T. (2015). A decision-making framework for precision marketing. Expert Systems with Applications, 42(7), 3357-3367.
4. Market Basket Analysis with recommenderlab. RPubs. Retrieved from https://rpubs.com/DiegoUsai/478772

## 📝 License

This project is provided for educational and research purposes. Please cite appropriately if using this code in your work.

## 🤝 Contributing

Contributions to improve the analysis or extend functionality are welcome. Please submit pull requests or open issues for discussion.

---

*Note: This implementation serves as a practical example of market basket analysis and can be adapted for various retail and e-commerce applications.*