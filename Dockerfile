FROM rocker/r-ver:4.4.1

ARG GIT_COMMIT
ARG GIT_DATE

ENV GIT_COMMIT=$GIT_COMMIT
ENV GIT_DATE=$GIT_DATE

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

# Create the directory if it doesn't exist
#RUN mkdir -p /opt/

ADD https://geomarker.s3.amazonaws.com/geocoder_2021.db /opt/geocoder.db
# COPY geocoder.db /opt/geocoder.db
ADD https://geomarker.s3.us-east-2.amazonaws.com/geometries/tracts_2010_sf_5072.rds /opt/tracts_2010_sf_5072.rds
ADD https://geomarker.s3.us-east-2.amazonaws.com/tract_dep_index_2018.rds /opt/tract_dep_index_18.rds



RUN apt-get update && apt-get install -y \
    libssl-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
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
    ruby-full \
    libgit2-dev \
    libproj-dev \
    libgeos-dev \
    libsecret-1-0 \
    && apt-get clean


# Install necessary R packages
#RUN R -e "install.packages(c('systemfonts', 'ggplot2'), dependencies=TRUE)"

RUN gem install sqlite3 -v 1.6.0

RUN gem install json Text

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
#RUN R --quiet -e "install.packages('remotes', repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'))"
RUN R --quiet -e "remotes::install_github('rstudio/renv')"

COPY renv.lock .
RUN R --quiet -e "renv::restore()"

#RUN R -e "renv::install('degauss-org/dht')"

RUN R -e "renv::install('Rcpp')"
RUN R -e "renv::install('GIScience/openrouteservice-r')"
COPY geocode.rb .
COPY entrypoint.R .
COPY utils.R .


COPY ./pcgc_isochrones.csv /app
COPY ./isochrones_pcgc_no_overlap.rds /app

# Copy test script and data
COPY ./tests /app/tests


WORKDIR /tmp  
# Run tests in a separate build step
RUN Rscript /app/tests/testthat/test_units_prebuilt.R
RUN Rscript /app/tests/testthat/test_units_prebuilt_2.R
RUN Rscript /app/tests/testthat/test_units_prebuilt_3.R

# Set entrypoint
ENTRYPOINT ["/app/entrypoint.R"]
