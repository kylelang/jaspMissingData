# -*- Makefile -*-

BUILD_DIR := builds
PKG_NAME := jaspMissingData
R_VERSION := 4.5.2

all: renv

## Build and install the package via renv:
renv:
	rig run -r $(R_VERSION) -f ./renv_build.R

## Update the packages in the renv project library:
update:
	rig run -r $(R_VERSION) -e "renv::update()"

## Update the state of the lockfile to match the renv library state:
snapshot:
	rig run -r $(R_VERSION) -e "renv::snapshot(dev = TRUE, exclude = 'colorout')"

regression:
	rig run -r $(R_VERSION) -e "renv::install('~/data/software/jasp/modules/regression/jaspRegression', repos = NULL, rebuild = TRUE)"

## Run unit tests:
test: tests/*
	rig run -r $(R_VERSION) -e 'jaspTools::setPkgOption("module.dirs", "./"); jaspTools::testAll()'

## Update the NAMESPACE and help files:
roxygen:
	rig run -r $(R_VERSION) -e "roxygen2::roxygenize(clean = TRUE)"

## Make sure we have a build directory:
$(BUILD_DIR):
	mkdir $(BUILD_DIR)

.PHONY: $(BUILD_DIR) roxygen renv update snapshot regression
