# forestMIDN

<h3>Package for importing, joining and querying MIDN forest data</h3>

The first step to using this package is to either execute the <b>importData()</b> or <b>importCSV()</b> function, which loads and names all of the views in the Analysis schema of the MIDN forest database into a new environment named VIEWS_MIDN (default setting) or the global environment. 

The <b>importData()</b> function connects to either a local instance or the remote server to import views in the Analysis schema of the MIDN forest database. This approach requires at least a local instance of the backend in SQL Server Management Studio, and will only work with Windows-based operating systems. RODBC and DBI packages must also be installed. 

The <b>importCSV()</b> function imports comma separated values files that were exported from the Analysis schema of the MIDN Forest Database. As long as the user has these exported .csv tables, this function will import and name the views the same as the importData() function. 

Function names that start with "join" pull together data from the views and allow the user to filter by common factors, such as park, years, exotic or native species, etc. Function names that start with "sum" are higher level functions that summarize data and typically have join functions under the hood. Function names starting with "prep" are typically internal functions used to prepare data for summarizing or visualization.
