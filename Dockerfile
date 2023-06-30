FROM rocker/r-ver:3.6.3
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libicu-dev libssl-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'install.packages("fs",upgrade="never", version = "1.6.1")'
RUN Rscript -e 'install.packages("doParallel",upgrade="never", version = "1.0.17")'
RUN Rscript -e 'install.packages("plyr",upgrade="never", version = "1.8.8")'
RUN Rscript -e 'install.packages("data.table",upgrade="never", version = "1.14.8")'
RUN Rscript -e 'install.packages("knitr",upgrade="never", version = "1.42")'
RUN Rscript -e 'install.packages("antaresRead",upgrade="never", version = "2.5.1")'
RUN Rscript -e 'install.packages("rmarkdown",upgrade="never", version = "2.21")'
RUN Rscript -e 'install.packages("testthat",upgrade="never", version = "3.1.7")'
RUN Rscript -e 'install.packages("logger",upgrade="never", version = "0.2.2")'
RUN Rscript -e 'install.packages("pipeR",upgrade="never", version = "0.6.1.3")'
RUN Rscript -e 'install.packages("antaresEditObject",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'install.packages("rAMPL",upgrade="never", version = "2.0.12.0-20230210")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
# userLauncher to be written here
#COPY  launcherAdequacyTest.R launcherAdequacyTest.R
#RUN Rscript launcherAdequacyTest.R
