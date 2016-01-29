# Can be fsc or scalac
SC = fsc
PACKAGE = compy

all: *.scala
	$(SC) *.scala

clean:
	rm -rf $(PACKAGE)

