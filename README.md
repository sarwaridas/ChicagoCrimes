## Criminals in Chicago getting Coffee

# OVERVIEW
In this study, I aim to examine the relationship between gentrification and neighborhood crime rates in Chicago by using growth in coffee shops in a community area as a proxy for gentrification. Although policymakers associate gentrification in a city with a reduction in crime, in practice, empirical relationships between the same have produced contradictory findings. In this analysis, I first attempt to explain this variation through the neighborhood area in which gentrification has occurred. Here, I will account for sociological factors like racial composition of a community, which may be a significant predictor of crime. Secondly, my analysis will be longitudinal; this gives us insights into how crime changes over time as the number of coffee shops grows.

# RESEARCH QUESTIONS
* Are the number of coffee shops in a neighborhood a significant predictor of crime in that area?
* How does this relationship vary across neighborhood? How does this relationship vary across time?
* What role do other sociological factors play in this relationship: for example, are results different for White neighborhoods vs Black neighborhoods?
* Potential work: What is the change in this relationship pre-gentrification vs post-gentrification?

Similar research1 has studied the relationship between gentrification and neighborhood crime over a 15-year period by examining the annual increase in corporate and noncorporate coffee shops. Although the data for my research will be similar, I will be using a hierarchal model to examine variation across time, while previous research has relied on fixed effects models.

# DATA

* Response variable: _Crime_ 
My data is extracted from the Chicago Police Department's CLEAR (Citizen Law Enforcement Analysis and Reporting) system. This dataset reflects reported incidents of crime that occurred in the City of Chicago from 2001 to present, minus the most recent seven days. In defining crime, I subset for homicides and robberies.

* Predictors: _Coffee Shops_
Coffee shops are a common symbol of neighborhood change, and have been referenced by sociologists in the past to create qualitative and descriptive accounts of gentrified neighborhoods. My data comes from Business licenses issued by the Department of Business Affairs and Consumer Protection in the City of Chicago from 2002 to the present. I subset for licenses with ‘coffee’ or ‘café’ in their titles.
  
Additionally, I collect the following information about a neighborhood to study its demographic characteristics: % White (Non-Hispanic), % Black, % Latino, % College Education (Bachelor’s degree or higher), Median Family Income (Real), % Families Below Poverty, Median House Value (Real), % Employed. This data comes from
community area snapshots provided by the Chicago Metropolitan Agency for Planning.
