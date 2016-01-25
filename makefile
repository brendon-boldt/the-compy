# Can be fsc or scalac
SC = fsc
CLASS_DIRS = $(patsubst %.scala,%,$(wildcard *.scala))

all: *.scala
	$(SC) *.scala

clean:
	rm -rf $(CLASS_DIRS)

