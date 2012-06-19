# makefile for transfer of emacs lisp files

VERSION = 1.9.5

PACKAGENAME = elisp-$(VERSION)

INSTDRIVE=a:
CP=cp
MV=mv

INSTSRC=nvscomp.el install.txt ChangeLog relnotes.txt Makefile nvselisp.texi 
TESTS=tests eltest.el

SRC=alists.el cl-read.el cclass.el autoproto.el block-cm.el        \
    c-boxes.el c-stuff.el c-support.el cando.el comm-align.el      \
    align-eq.el align-regexp.el convert.el date.el faceedit.el     \
    fcreate.el functions.el classdoc.el fundoc.el generic-code.el  \
    multi-mode.el header.el instalit.el makedocs.el makefile.el    \
    mk-comp.el modefn.el nvs-auto.el nvs-menu.el nvs.el nvscomp.el \
    os2patch.el par-align.el protos.el testsup.el whitesp.el       \
    wsearch.el msvt.el mifmode.el c++-tools.el idl-mode.el 	   \
    spellsup.el diredsup.el ptmacros.el

COMPILED=$(SRC:.el=.elc) nvselisp.info

SRC18=patches.el

INDEV=makefile.el rcs.el rcs-cks.el

all: compile

nvselisp.info: nvselisp.texi

compile: 
	emacs -batch -q -nw -l nvscomp.el -f compile-nvs

release:  $(PACKAGENAME).tar.gz 

$(PACKAGENAME).tar.gz: elisp-$(VERSION).tar
	gzip $?

$(PACKAGENAME).tar: $(SRC) $(INSTSRC) compile nvselisp.info
	-tar cvf $@ $(SRC) $(SRC19) $(INSTSRC) $(COMPILED)

test-release19: $(SRC) $(SRC19) $(INSTSRC) $(TESTS)
	tar cvf elisptst.tar $(SRC) $(SRC19) $(INSTSRC) $(TESTS)
	gzip elisptst.tar
	$(MV) elisptst.tar.gz elisptst.tgz



release18: $(SRC) $(SRC18) $(INSTSRC)
	@echo "Emacs release 18 is no longer supported"

test:
	cd tests ; emacs -q -nw -l doall.el

clean:
	rm -f $(COMPILED)


unix-conv:
	-unix_dos -d $(SRC) $(INITSRC)

dos-conv:
	-unix_dos -u $(SRC) $(INITSRC)

