JAVASRC = helloworld.java
SOURCES = README Makefile $(JAVASRC)
MAINCLASS = helloworld
CLASSES = helloworld.class
JARFILE = helloworld.jar

all: $(JARFILE)

$(JARFILE): $(CLASSES)
	echo Main-class: $(MAINCLASS) > Manifest
	jar cvfm $(JARFILE) Manifest $(CLASSES)
	rm Manifest

$(CLASSES): $(JAVASRC)
	javac -Xlint $(JAVASRC)

clean:
	rm $(CLASSES) $(JARFILE)

run:
	java $(MAINCLASS)

submit:
	submit cmps112-wm.w19 lab0 helloworld.java Makefile
