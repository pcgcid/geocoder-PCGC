FROM rocker/r-ver:4.2.1

# DeGAUSS container metadata
ENV degauss_name="geocoder"
ENV degauss_version="3.3.0"
ENV degauss_description="geocodes"
ENV degauss_argument="valid_geocode_score_threshold [default: 0.5]"

# add OCI labels based on environment variables too
LABEL "org.degauss.name"="${degauss_name}"
LABEL "org.degauss.version"="${degauss_version}"
LABEL "org.degauss.description"="${degauss_description}"
LABEL "org.degauss.argument"="${degauss_argument}"

ADD https://geomarker.s3.amazonaws.com/geocoder_2021.db /opt/geocoder.db
# COPY geocoder.db /opt/geocoder.db

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    make \
    sqlite3 \
    libsqlite3-dev \
    flex \
    ruby-full \
    bison \
    gnupg \
    software-properties-common \
    libsodium-dev \
    libpng-dev \
    gdal-bin libgdal-dev \
    libudunits2-dev \
    libv8-dev\
    libfontconfig1-dev \
    libfreetype6-dev \
    && apt-get clean


# Install necessary R packages
#RUN R -e "install.packages(c('systemfonts', 'ggplot2'), dependencies=TRUE)"


RUN gem install sqlite3 json Text

RUN mkdir /app
WORKDIR /app

COPY Makefile.ruby .
COPY /src ./src
COPY /lib ./lib
COPY /gemspec ./gemspec

RUN make -f Makefile.ruby install \
    && gem install Geocoder-US-2.0.4.gem


# install required version of renv
RUN R --quiet -e "install.packages('remotes', repos = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R --quiet -e "remotes::install_github('rstudio/renv@0.15.4')"

COPY renv.lock .
RUN R --quiet -e "renv::restore()"

RUN R -e "renv::install('degauss-org/dht')"
RUN apt update && apt install -y libsecret-1-0

RUN R -e "renv::install('Rcpp')"
RUN R -e "renv::install('GIScience/openrouteservice-r')"
RUN R -e "renv::install('sf')"
RUN apt install -y  libudunits2-dev libproj-dev libgdal-dev libgeos-dev
RUN R -e 'install.packages(c("shiny", "shinydashboard", "docopt", "sf"), repos="https://cran.rstudio.com/")'


COPY geocode.rb .
COPY entrypoint.R .
COPY utils.R .


COPY ./ctsa_centers.csv /app
COPY ./isochrones.rds /app
COPY ./app.R /app




WORKDIR /tmp


ENTRYPOINT ["/app/entrypoint.R"]



#CMD ["R", "-e", "shiny::runApp('/app',host='0.0.0.0',port = 3838)"]

# Expose port 3838
EXPOSE 3838
#CMD ["/bin/bash"]