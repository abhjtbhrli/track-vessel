# [Track the Vessel](https://abhjt-bhrli.shinyapps.io/vessel-tracker/) 
## R/Shiny Application for Appsilon

![Screenshot 2021-10-01 at 12 37 46 PM](https://user-images.githubusercontent.com/37649445/135579645-0bd4af5d-a76f-4fde-9e28-a9dcea00a359.png)

Here is a small preview of the application.


https://user-images.githubusercontent.com/37649445/132986018-6971c3f5-b21e-4edc-8182-0ec5c5c27c00.mp4



Marine AIS signal data is used to identify the longest distance covered by a ship between two consecutive observations. The vessels are categorised by ship type and every ship has its own unique name.

The application has been created in `shiny.semantic`, using Shiny modules. The maps incorporated in the application have been generated using the `leaflet` package. Other packages that have been used are `shiny`, `tidyverse` and `geosphere`.

The longest distance calculation is the Haversine distance between two sets of longitudes and latitudes. 

The entire application source code, including the modules, are in the [app.R file](https://github.com/abhjtbhrli/track-vessel/blob/main/app.R).

The application is hosted on [shinyapps.io](https://www.shinyapps.io/) at the following address: [https://abhjt-bhrli.shinyapps.io/vessel-tracker/](https://abhjt-bhrli.shinyapps.io/vessel-tracker/).

