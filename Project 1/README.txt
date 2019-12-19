NOTE: datasets are private from the Canadian Sports Institute so it is not included.

To reproduce our results:

- Preprocessing - 

First, in the python file folder, run data_clean.py which preprocesses the raw datasets from the web links and stores performance.csv and wellness_adj.csv to data folder.

- Models - 

Then, in the R files folder, run wellness_model.r to create performance_normalized.csv and store models to rds files in the data folder.

- Results - 

Lastly, in the R files folder, run Report.rmd to retrieve models from rds and produce results and the report.
