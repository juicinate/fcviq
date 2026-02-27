## syntax=docker/dockerfile:1

# set argument for the directory to be installed in
ARG APPDIR=fcviq

FROM rocker/verse:latest AS builder

# install packages set that I usually need
COPY quarto_build.sh .
RUN bash quarto_build.sh

RUN install2.r --error \
    devtools \
    remotes \
    dplyr \
    shiny \
    quarto



FROM builder AS renv_stage

ARG APPDIR

# make folder for app
WORKDIR /$APPDIR

# copy files for renv
RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# actually run renv
RUN R -e "renv::restore()"



FROM renv_stage

ARG APPDIR
WORKDIR /$APPDIR

RUN mkdir data

# copy files for the app, expects all necessary files to be in the same folder as this dockerfile
COPY app/ .

# expose the port 
EXPOSE 3499

# run the app
CMD ["Rscript", "app.R"] 

