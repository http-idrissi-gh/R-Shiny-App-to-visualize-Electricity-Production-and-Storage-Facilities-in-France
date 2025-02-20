# Analysis of Electrical Installations in France

This project is an R Shiny application that analyzes electricity production and storage installations in France using interactive visualizations, using the open dataset from: https://explore.data.gouv.fr/fr/datasets/5bfcc5a006e3e744e304ccf0/#/resources/c14e5a7d-2ca6-4ad8-bc61-93889d13fc25 , the CSV in this repository latest update was Jan 31, 2025 at 16:07. Feel free to use the latest version if you need to.

## Features

The application offers several types of analysis:

- **Overview**: Global statistics and distribution of installations.
- **Temporal Analysis**: Evolution of the number of installations and installed capacity over time.
- **Sector Analysis**: Comparison of different energy sectors and their impact.
- **Geographical Analysis**: Mapping of installations across French regions and departments.
- **Custom Exploration**: Interactive filters to refine the analysis based on various criteria.

## Technologies Used

- **R Shiny**: For developing the interactive application.
- **shinydashboard**: Modern and intuitive user interface.
- **ggplot2**: Data visualization.
- **leaflet**: Interactive mapping.
- **dplyr**: Data manipulation and filtering.
- **DT**: Dynamic table display.
- **sf**: Spatial data handling.
- **stringr**: String manipulation.

## Installation

1. Ensure that **R** and **RStudio** are installed on your machine.
2. Install the required libraries if they are not already installed by clicking on `install` from the **RStudio** pop-op, or by using this R code:
   ```r
   install.packages(c("shiny", "shinydashboard", "ggplot2", "leaflet", "dplyr", "DT", "scales", "lubridate", "sf", "stringr"))
   ```
3. Clone this repository:
   ```bash
   git clone https://github.com/http-idrissi-gh/R-Shiny-App-to-visualize-Electricity-Production-and-Storage-Facilities-in-France.git
   ```
4. Navigate to the project folder and run the application from **RStudio**:
   ```r
   shiny::runApp()
   ```

## Project Structure

- `app.R`: Contains the user interface and server logic.
- `data/`: Contains the datasets used for analysis.

## Usage

1. Launch the application using the Run App button or using `shiny::runApp()`.
2. Navigate through the tabs to explore different types of analysis.
3. Use interactive filters to customize data display.

## Authors

Mohamed, Théo & Youssouf



