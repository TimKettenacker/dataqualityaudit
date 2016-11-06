A Data Quality Audit determines the status quo of data quality in its entirety, which means that not only data is at the center of focus but also the demands for information from various departments are. To achieve this, firstly the process analysis is done where the business aspect of the project and the current data resources is clarified through constructive communication. Secondly, the complete data set is analyzed using Uniserv’s data quality tools; ‘Data Analyzer’ & ‘Data Quality Batch Suite’. This analysis may include one or more of the following:

  1.	Completeness of the datasets at field level.
  2.	Correctness of syntax for fields containing date, email, country code etc.
  3.	Plausibility of address elements based on correctness and deliverability.
  4.	Singularity of data sets based on predetermined parameters.
  5.	Accuracy of address elements based on address relocation.

The necessity to incorporate engaging visualizations in the Data Quality arises from the need to share complex data with our customers: by using interactive graphs we are creating a structured interpretation path in a way that it both helps our customers to grasp the connection between their datasets as well as giving them the freedom to experience their data as they are moved by their own curiosity. Data tells stories, and visualizing it creates emotional connection to retain the information it conveys.

Advanced techniques for integrating data visualization into solution landscapes are becoming more commonplace nowadays, with an increasing share being powered by open source technology. That is why we decided to make use of the variety of available open source libraries to cater for specific visualization needs. 

When setting up a data quality audit, we use the R programming language to process the output of our data quality tools and connect to the visualization libraries. Originally intended for statistical purposes, the R programming language now comes with a vibrant ecosystem of packages to solve all kinds of data-related matters. R packages are constantly created and maintained by an ever-growing community. 

The Data Quality Audit framework is written in R Markdown, which allows converting its contents to valid HTML and many other formats, thus facilitating to share the results with our audience.  One single R Markdown file contains a standardized script to visualize the data we already processed and validated in DQBT, our data quality batch software. 
