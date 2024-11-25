# Pizza Sales Initial Analysis Subproject  🍕

This subproject is part of a larger repository that aims to analyze and optimize the financial performance of a pizzeria. Specifically, this analysis focuses on extracting key insights from sales data, visualizing trends, and identifying areas for improvement.

## Features

1. **Data Analysis:**
   - Aggregates and analyzes daily pizza sales data.
   - Calculates key metrics such as total sales, average order value, total pizzas sold, and average price per pizza.

2. **Statistical Insights:**
   - Provides statistics such as:
     - Best and worst sales days.
     - Days with the highest and lowest pizza volumes.
     - Average daily pizzas sold and average price per pizza.
   
3. **Visualizations:**
   - Interactive dashboards created with Plotly, featuring:
     - Daily sales and pizza quantity trends.
     - Moving averages for better trend analysis.
     - Distribution plots of daily sales and quantities.
   - Outputs the dashboard as an HTML file and a static PNG image.

4. **Summary Table:**
   - Summarizes performance metrics by weekday for actionable insights.

5. **Error Handling:**
   - Includes error handling for missing or improperly formatted data files.

---

## Requirements

### Dependencies
- **Python 3.7+**
- Libraries:
  - `pandas`
  - `plotly`
  - `datetime`

Install the required libraries with:
```bash
pip install pandas plotly
```

### Dataset
This project uses the pizza sales dataset from Kaggle. Make sure to have the file `Data Model - Pizza Sales.xlsx` in the data directory of the src folder. You can replace this file with your own sales data, ensuring it has the following columns:
- `order_date`: Date of the order (in `YYYY-MM-DD` format).
- `total_price`: Total sales value of the order.
- `quantity`: Number of pizzas sold in the order.
- `order_id`: Unique identifier for the order.

---

## How to Run

1. Clone the repository and navigate to the subproject directory.
2. Ensure the required libraries are installed.
3. Place the sales dataset in the working directory.
4. Run the script:
   ```bash
   python pizza_analysis.py
   ```

---

## Outputs

1. **Analysis Results:**
   - Prints daily sales and quantity statistics.
   - Summarizes performance by weekday.

2. **Interactive Dashboard:**
   - An HTML file (`pizza_analysis_dashboard.html`) for an interactive dashboard.
   - A PNG image (`pizza_analysis_dashboard.png`) as a static version.

---

## Example Use Case

1. Identify the most profitable days of the week.
2. Discover high-demand periods to optimize staffing and inventory.
3. Analyze pizza pricing strategies and adapt to improve profit margins.

---


## Acknowledgements

Dataset sourced from [Kaggle](https://www.kaggle.com/code/melikedilekci/eda-pizza-restaurant-sales/notebook).
