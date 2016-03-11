# Can be fsc or scalac
SC = fsc
PACKAGE = compy
SCALA_FILES = $(wildcard src/*.scala)
VPATH = src

all: $(SCALA_FILES)
	$(SC) $(SCALA_FILES)

clean:
	rm -rf $(PACKAGE)

