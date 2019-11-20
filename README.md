# site_visits
Site Visits

This is code to help prioritize site visits based on location and other attributes
. 
The code files all start with the letter r for run. 

r1 takes the site visit location Excel file on the WSDOT sharepoint site and converts the addresses into lat/long that can be added to maps. 

r2 calculates the distances in miles and minutes between any pair of sites. The calculation assumes noon on a Tuesday for average midweek travel times given that one site visit would have already occured that day.

r3 produces a basic ggmap to show the locations of the sites across the site. 

r4 is the shiny web app. This is a zoomable map that zooms to any grantee site selected and shows all other sites that you could get to within 60 minutes. Popups show the names of the sites as well as the days since the last site visit by visit type. If there are no sites within 60 minutes, only the site selected is show and the map zooms out to the state level. The site selection drop down is searchable. 

r5 is the code outside of the shiny app for producing the type and days since last visit data frame from the Excel file on the SSO sharepoint site. 
