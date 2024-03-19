Directories
“final_data”: after raw data are treated and assembled through ETL, the final dataset is stored here
“product”: it contains graphs and pictures generated through rstudio
“raw_data”: it contains untreated datasets and their respective codebooks. These datasets correspond to the main inputs in my work

Scripts
“analysis1.R”: this script runs descriptive analysis and generating some graphs. This script uses “final_data\db_socialx_pcp” dataset
“cmd_etl.R”: this script runs ETL of commodity prices and trade and builds up a price commodity index prototype. It is still under
development.
“database_etl.R”: this script runs ETL of the social spending dataset. Currently, it aggregates social spending, political and economic data. It is still under
development.
“dsotm.R”: script running the analysis from my article "The Dark Side of the Moon: explicando o Welfare State da América Central durante o Boom das Commodities". This script uses “final_data\db_socialx_pcp” dataset
“doctoral_research”: R project.