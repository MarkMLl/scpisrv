# This is intended to build a text-only variant of the program in this directory, based on
# http://troydm.github.io/blog/2014/01/26/making-30-years-old-pascal-code-run-again/
# It probably won't attempt anything fancy like working out what revision the sources are
# which might result in warnings during compilation and possibly linkage, to do the
# whole job properly use lazbuild or the Lazarus IDE. MarkMLl.

# PREREQUISITE: FPC (Free Pascal Compiler), v2.6.0 but preferably v3.0.2 or above.

############

# FPCFLAGS can usefully be transcribed from the Lazarus IDE's "Show Options" output.

FPC=/usr/local/bin/fpc
FPCFLAGS=-O3 -CX -XX -k--build-id 
CPU=$(shell uname -m | sed 's/i686/i386/' | sed 's/armv7l/arm/')
OPSYS=$(shell uname -o | sed 's/GNU\/Linux/linux/')

# Note that the CPU identifier above loses potentially-useful information, but
# this was done in order to generate the same filenames emitted by Lazarus.

############

_=scpidemo
TARGET=$_-$(CPU)-$(OPSYS)

all: $(TARGET)

any: Makefile

# NOTE THAT THIS MIGHT ONLY BE A PARTIAL DEPENDENCY LIST.

$(TARGET):any scpiserver.pas scpiparser.pas scpidemo.lpr 
	$(FPC) $(FPCFLAGS) -o$(TARGET) $_.lpr 

test: $(TARGET)
	./$(TARGET) --version

############

clean:
	rm -f scpidemo.o scpiserver.ppu scpiserver.o scpiparser.ppu scpiparser.o
                
distclean: clean
	rm -f $(TARGET)* *lps $_.tar $_.tar.gz $_.tgz $_.zip

pubclean: distclean
	rm -rf backup lib *~

PUBFILES=*

tar: pubclean
	tar chf $_.tar $(PUBFILES)

tar.gz: pubclean
	tar chzf $_.tar.gz $(PUBFILES)

tgz: tar.gz
	mv $_.tar.gz $_.tgz

zip: pubclean
	zip $_.zip $(PUBFILES)

publish: zip

PUBDIR:=$(shell mktemp -d /tmp/$_-XXX)

pubtest: zip
	- unzip -d $(PUBDIR) $_.zip | grep -v warning:
	make -C $(PUBDIR) test
	rm -rf $(PUBDIR)

