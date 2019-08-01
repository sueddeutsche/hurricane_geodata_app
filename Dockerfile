FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libssl-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libv8-dev \
    libgdal-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libgsl-dev \
    libjq-dev \
    vim


# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='$MRAN')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    chown shiny:shiny /var/lib/shiny-server

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('rvest', 'dplyr', 'stringr', 'rgdal', 'rmapshaper', 'geojson', 'geojsonio'), repos='http://cran.rstudio.com/')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# allow permissions
RUN sudo chown -R shiny:shiny /srv/shiny-server

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]