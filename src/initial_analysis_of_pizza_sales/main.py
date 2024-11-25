import pandas as pd # Import the pandas library
import plotly.graph_objects as go # Import the graph objects module from the plotly library
import plotly.express as px # Import the express module from the plotly library
from plotly.subplots import make_subplots # Import the make_subplots module from the plotly library
from datetime import datetime # Import the datetime module from the datetime library

# dataset from https://www.kaggle.com/code/melikedilekci/eda-pizza-restaurant-sales/notebook

# retuns a DataFrame with the daily analysis and statistics
def analyze_pizza_business(file_path):
    #  check if the file exists
    try:
        df = pd.read_excel(file_path)
    except FileNotFoundError:
        print(f"Error: File '{file_path}' not found.")
        return None
    except Exception as e:
        print(f"Error reading file: {e}")
        return None

    # Convert order_date to datetime if it isn't already
    df['order_date'] = pd.to_datetime(df['order_date'])

    # in our particular we want to analyze by day
    daily_analysis = df.groupby('order_date').agg({
        'total_price': ['sum', 'mean'],
        'quantity': ['sum', 'mean'],
        'order_id': 'nunique'
    }).round(2)

    daily_analysis.columns = [
        'total_sales',
        'average_order_value',
        'total_pizzas',
        'average_pizzas_per_order',
        'number_of_orders'
    ]

    # add average prcie per pizza
    daily_analysis['average_price_per_pizza'] = (
        daily_analysis['total_sales'] / daily_analysis['total_pizzas']
    ).round(2)

    # Calculate statistics, might want to add more analysis on pizza type to know which is the most popular
    # and which is the least popular, to adapt the inventario accordingly (datos demanda)
    stats = {
        'Sales Statistics': {
            'Average Daily Sales': daily_analysis['total_sales'].mean().round(2),
            'Best Sales Day': daily_analysis['total_sales'].idxmax().strftime('%Y-%m-%d'),
            'Worst Sales Day': daily_analysis['total_sales'].idxmin().strftime('%Y-%m-%d'),
            'Average Price per Pizza': daily_analysis['average_price_per_pizza'].mean().round(2)
        },
        'Quantity Statistics': {
            'Average Daily Pizzas': daily_analysis['total_pizzas'].mean().round(2),
            'Highest Volume Day': daily_analysis['total_pizzas'].idxmax().strftime('%Y-%m-%d'), # bulk order
            'Lowest Volume Day': daily_analysis['total_pizzas'].idxmin().strftime('%Y-%m-%d'), # slow day
            'Average Pizzas per Order': daily_analysis['average_pizzas_per_order'].mean().round(2)
        }
    }

    return daily_analysis, stats

def plot_comprehensive_analysis(daily_analysis):
    """
    Create comprehensive visualizations using Plotly
    """
    # Calculate moving averages
    sales_ma = daily_analysis['total_sales'].rolling(window=7).mean()
    quantity_ma = daily_analysis['total_pizzas'].rolling(window=7).mean()

    # Create subplots
    fig = make_subplots(
        rows=3, cols=2,
        subplot_titles=(
            'Daily Sales Over Time', '',
            'Daily Pizza Quantities Over Time', '',
            'Distribution of Daily Sales', 'Distribution of Daily Pizza Quantities'
        ),
        specs=[[{"colspan": 2}, None],
               [{"colspan": 2}, None],
               [{}, {}]],
        vertical_spacing=0.12
    )

    # Plot 1: Daily Sales Over Time
    fig.add_trace(
        go.Scatter(x=daily_analysis.index, y=daily_analysis['total_sales'],
                  name='Daily Sales', mode='lines', line=dict(color='blue', width=1)),
        row=1, col=1
    )
    fig.add_trace(
        go.Scatter(x=daily_analysis.index, y=sales_ma,
                  name='7-day Moving Average (Sales)', 
                  line=dict(color='red', width=2, dash='dash')),
        row=1, col=1
    )

    # Plot 2: Daily Pizza Quantities
    fig.add_trace(
        go.Scatter(x=daily_analysis.index, y=daily_analysis['total_pizzas'],
                  name='Daily Quantities', mode='lines', 
                  line=dict(color='green', width=1)),
        row=2, col=1
    )
    fig.add_trace(
        go.Scatter(x=daily_analysis.index, y=quantity_ma,
                  name='7-day Moving Average (Quantities)', 
                  line=dict(color='red', width=2, dash='dash')),
        row=2, col=1
    )

    # Plot 3: Distribution of Daily Sales
    fig.add_trace(
        go.Histogram(x=daily_analysis['total_sales'],
                    name='Sales Distribution',
                    nbinsx=30,
                    marker_color='blue'),
        row=3, col=1
    )
    fig.add_vline(x=daily_analysis['total_sales'].mean(),
                  line_dash="dash", line_color="red",
                  row=3, col=1)

    # Plot 4: Distribution of Daily Pizza Quantities
    fig.add_trace(
        go.Histogram(x=daily_analysis['total_pizzas'],
                    name='Quantity Distribution',
                    nbinsx=30,
                    marker_color='green'),
        row=3, col=2
    )
    fig.add_vline(x=daily_analysis['total_pizzas'].mean(),
                  line_dash="dash", line_color="red",
                  row=3, col=2)

    # Update layout
    # TODO: add dark mode to the whole page, for the moment it is only for the plot section, the rest of the page is still in light mode :/
    fig.update_layout(
        height=1200,
        width=1200,
        showlegend=True,
        title_text="Pizza Business Analysis Dashboard",
        title_x=0.5,
        template="plotly_white" # for more themes check https://plotly.com/python/templates/
    )

    # Update axes labels
    fig.update_xaxes(title_text="Date", row=1, col=1)
    fig.update_xaxes(title_text="Date", row=2, col=1)
    fig.update_xaxes(title_text="Total Sales ($)", row=3, col=1)
    fig.update_xaxes(title_text="Number of Pizzas", row=3, col=2)

    fig.update_yaxes(title_text="Total Sales ($)", row=1, col=1)
    fig.update_yaxes(title_text="Number of Pizzas", row=2, col=1)
    fig.update_yaxes(title_text="Frequency", row=3, col=1)
    fig.update_yaxes(title_text="Frequency", row=3, col=2)

    # Save the plots as HTML file (interactive)
    fig.write_html("pizza_analysis_dashboard.html")
    
    # Save as PNG as well
    fig.write_image("pizza_analysis_dashboard.png")
    
    # Show the plot
    fig.show()

def create_summary_table(daily_analysis):
    """
    Create a summary table of key performance indicators by weekday
    """
    # Add weekday information
    daily_analysis['weekday'] = daily_analysis.index.day_name()
    
    # Calculate weekday averages
    weekday_analysis = daily_analysis.groupby('weekday').agg({
        'total_sales': 'mean',
        'total_pizzas': 'mean',
        'number_of_orders': 'mean',
        'average_price_per_pizza': 'mean'
    }).round(2)
    
    # Sort by typical weekday order
    weekday_order = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 
                     'Friday', 'Saturday', 'Sunday']
    weekday_analysis = weekday_analysis.reindex(weekday_order)
    
    return weekday_analysis

def main(file_path):
    daily_analysis, stats = analyze_pizza_business(file_path)
    
    if daily_analysis is not None:
        # Print sales statistics
        print("\nSales Statistics:")
        for metric, value in stats['Sales Statistics'].items():
            print(f"{metric}: {value}")
            
        # Print quantity statistics
        print("\nQuantity Statistics:")
        for metric, value in stats['Quantity Statistics'].items():
            print(f"{metric}: {value}")
        
        # Create and display weekday summary
        weekday_summary = create_summary_table(daily_analysis)
        print("\nWeekday Performance Summary:")
        print(weekday_summary)
        
        # Create visualizations
        plot_comprehensive_analysis(daily_analysis)


if __name__ == "__main__":
    file_path = "Data Model - Pizza Sales.xlsx"
    main(file_path)
    