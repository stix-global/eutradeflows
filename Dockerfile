FROM rocker/shiny

MAINTAINER Paul Rougieux "https://github.com/paulrougieux/"


# Install dependencies 
# * libmariadb-client-lgpl-dev is required by the RMySQL package
# * mariadb-client is used as a mysql client and for mysqldump
RUN apt-get update && apt-get install -y \
  libmariadb-client-lgpl-dev \
  mariadb-client 

# Packages
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('dbplyr')"
RUN Rscript -e "install.packages('RMySQL')"

# Set the working directory to /R
WORKDIR /R

# Copy the current directory contents into the container at /R/tradeharvester
ADD . /R/eutradeflows

RUN R CMD build eutradeflows

# Install dependencies 
# RUN Rscript -e "install.packages('devtools')"
# RUN R -e 'devtools::install_github("EuropeanForestInstitute/tradeflows")'
