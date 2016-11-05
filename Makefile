ggedit_VERSION=$(shell grep -i ^[Vv]ersion ggedit/DESCRIPTION | cut -d : -d \  -f 2)
ggedit_NAME=$(shell grep -i ^[Pp]ackage: ggedit/DESCRIPTION | cut -d : -d \  -f 2)

ggedit_R_FILES := $(wildcard ggedit/R/*.R)
ggedit_DATA_FILES := $(wildcard ggedit/data/*.rda)
ggedit_APPLICATION_FILES := $(wildcard ggedit/application/*.rda)
ggedit_ADDIN_FILES := $(wildcard ggedit/rstudio/*.dcf)


.PHONY: install check clean build

check: pkg/$(ggedit_NAME)_$(ggedit_VERSION).tar.gz
	R CMD check pkg/$(ggedit_NAME)_$(ggedit_VERSION).tar.gz
	
#data: data/slideDefaults.rda data/ThemeDefaultClass.rda
# R CMD BATCH --no-save slideDefaults.R
#	R CMD BATCH --no-save ThemeDefaultClass.R

build: pkg/$(ggedit_NAME)_$(ggedit_VERSION).tar.gz
	R CMD build $(ggedit_NAME) 
	mv $(ggedit_NAME)_$(ggedit_VERSION).tar.gz pkg/$(ggedit_NAME)_$(ggedit_VERSION).tar.gz

install: pkg/$(ggedit_NAME)_$(ggedit_VERSION).tar.gz
	R CMD INSTALL pkg/$(ggedit_NAME)_$(ggedit_VERSION).tar.gz -l lib

clean:
	-rm -f pkg/$(ggedit_NAME)_*.tar.gz
	-rm -fr lib/$(ggedit_NAME)