# flowgraphs

Automated calculation and graphing of flow data tables
The flow_graphs script is not smart, so you need to tell it what to do. The original flow_graphs is intended for calculating and graphing total cell numbers from the cell counts in your FCS files combined with either Countess or bead counts. It will plot this as total numbers extrapolated to the entire tissue, and will also plot the frequency of each cell type out of the total. The flow_graph_percents script is intended for graphing the expression of markers on a single cell type (e.g., %CD25+ on Tregs), but for any and all markers you want to analyze.

flow_graphs instructions
To start, analyze your data in FlowJo and create a data table. Set the outputs of the data table to be “Count” rather than the default “Frequency of Parent”. For this to work, you must include the data on the total number of live leukocytes in the file. Usually, this will be something like viable single CD45+ cells or viable single lymphocytes if you don’t have CD45 in your panel. Also include the data for counting beads (if you have them). Export the data to Excel and rename the columns (or do this in FlowJo) to a short description of each cell type (e.g., B Cells, Treg, Macrophages). Save the file as a CSV in the folder where you want to make the graphs, with the word “numbers” at the end.
Now, you will need to be sure that your CSV file contains a column labeled “beads”, one labeled “Total” (your live CD45+ singlets or live lymphocytes). Next, you need to create two new columns and add the appropriate data from your cell count sheet. First, add a column called “cell count” and put in any cell count data (paste values, not formulas) you have. Second, add a column called “fraction stained) and put in that information if you have it (e.g., if you stained half the brain, put 0.5 or =1/2). You need to have either a cell count or a fraction stained for each sample for this to work. The names are case-sensitive at the moment.
Double check that you have columns with these names:
•	beads
•	Total
•	cell count
•	fraction stained
Save the “numbers” CSV file.
Put a copy of the script in the folder with the CSV file and double click to open it in RStudio.
Under “set groups”, line 37, you will need to tell it the names you want to use for the groups and how many samples are in each group. This needs to be in the same order as in the CSV file. The script will check to see if you’ve entered the same number as it sees in your data.
Select the type of plot you’d like to run, and the colors for the groups.
Run the rest of the script. It will calculate the total number of each cell type you have in your CSV file and the percent of each cell type among all cells. It should create graphs of each for each cell type. It’s currently set to graph standard error, and the statistical test is Tukey’s post-test on ANOVA.



flow_graphs_percents instructions
Export the “percent of parent” data for a given cell type from FlowJo. In the example csv I’ve provided, the data are for several markers on Tregs. If you name this file as something_something_percents.csv, the script should pick it up and graph all the markers for you. Again, you’ll need to tell it the names of your groups and how many samples are in each, in the order they appear in the CSV file (top to bottom). Also, tell the script what your cell type is (in this example, Tregs), to get that information in the plot titles.
