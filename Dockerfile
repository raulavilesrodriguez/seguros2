FROM rocker/shiny-verse:latest

# Add the repository (assuming it's a Debian/Ubuntu-based image)
RUN apt-get update && apt-get install -y software-properties-common
RUN add-apt-repository ppa:c2d4u.team/c2d4u4.0+

# Update the package list and install any required packages
RUN apt-get update
RUN apt-get install -y make
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y libsodium-dev

RUN R -e "install.packages('config')"
RUN R -e "install.packages('DBI')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('glue')"
RUN R -e "install.packages('fontawesome')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('RPostgreSQL')"
RUN R -e "install.packages('pool')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('uuid')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinyauthr')"
RUN R -e "install.packages('readxl')"
RUN R -e "install.packages('writexl')"
RUN R -e "install.packages('shinyBS')"










 


  
        
        
        
        
        