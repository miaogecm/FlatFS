-include ../../config.mak.autogen
-include ../../config.mak

prefix ?= /usr/local
mandir ?= $(prefix)/share/man
libexecdir ?= $(prefix)/libexec/git-core
gitdir ?= $(shell git --exec-path)
man1dir ?= $(mandir)/man1

gitver ?= $(word 3,$(shell git --version))

# this should be set to a 'standard' bsd-type install program
INSTALL ?= install

ASCIIDOC_CONF      = ../../Documentation/asciidoc.conf
MANPAGE_NORMAL_XSL =  ../../Documentation/manpage-normal.xsl

GIT_SUBTREE_SH := git-subtree.sh
GIT_SUBTREE    := git-subtree

GIT_SUBTREE_DOC := git-subtree.1
GIT_SUBTREE_XML := git-subtree.xml
GIT_SUBTREE_TXT := git-subtree.txt
GIT_SUBTREE_HTML := git-subtree.html

all: $(GIT_SUBTREE)

$(GIT_SUBTREE): $(GIT_SUBTREE_SH)
	cp $< $@ && chmod +x $@

doc: $(GIT_SUBTREE_DOC) $(GIT_SUBTREE_HTML)

install: $(GIT_SUBTREE)
	$(INSTALL) -d -m 755 $(DESTDIR)$(libexecdir)
	$(INSTALL) -m 755 $(GIT_SUBTREE) $(DESTDIR)$(libexecdir)

install-doc: install-man

install-man: $(GIT_SUBTREE_DOC)
	$(INSTALL) -d -m 755 $(DESTDIR)$(man1dir)
	$(INSTALL) -m 644 $^ $(DESTDIR)$(man1dir)

$(GIT_SUBTREE_DOC): $(GIT_SUBTREE_XML)
	xmlto -m $(MANPAGE_NORMAL_XSL)  man $^

$(GIT_SUBTREE_XML): $(GIT_SUBTREE_TXT)
	asciidoc -b docbook -d manpage -f $(ASCIIDOC_CONF) \
		-agit_version=$(gitver) $^

$(GIT_SUBTREE_HTML): $(GIT_SUBTREE_TXT)
	asciidoc -b xhtml11 -d manpage -f $(ASCIIDOC_CONF) \
		-agit_version=$(gitver) $^

test:
	$(MAKE) -C t/ test

clean:
	rm -f *~ *.xml *.html *.1
	rm -rf subproj mainline
