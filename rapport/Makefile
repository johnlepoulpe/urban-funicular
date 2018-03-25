.PHONY: clean astyle evince all

default: all

PDFLATEX_TAGS :=

TEX_FILE := $(wildcard *.tex)
SUB_TEX_FILES := $(wildcard includes/*.tex)
AUX_FILES := $(filter-out $(TEX_FILE) ,$(wildcard *.*))
SUB_AUX_FILES := $(filter-out $(SUB_TEX_FILES) ,$(wildcard includes/*.*))

all:
	pdflatex $(TEX_FILE)
	pdflatex $(TEX_FILE)
	pdflatex $(TEX_FILE)

ASTYLE_OPTIONS := --style=attach --indent=spaces=2

astyle:
	astyle $(TEX_FILE) $(SUB_TEX_FILES)

evince: all
	evince Freire_Legrand-Duchesne.pdf

clean:
	rm -r $(AUX_FILES) $(SUB_AUX_FILES)
