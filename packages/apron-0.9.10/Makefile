include Makefile.config

REQUESTED = \
mlgmpidl/Makefile.config \
apron/Makefile.config

all: $(REQUESTED)
ifneq ($(HAS_OCAML),)
	(cd mlgmpidl; make all install)
endif
	(cd apron; make all)

install:
ifneq ($(HAS_OCAML),)
	(cd mlgmpidl; make install)
endif
	(cd apron; make install)

rebuild: $(REQUESTED)
ifneq ($(HAS_OCAML),)
	(cd mlgmpidl; make rebuild all install)
endif
	(cd apron; make rebuild)

clean: $(REQUESTED)
	(cd mlgmpidl; make clean)
	(cd apron; make clean)

mostlyclean: $(REQUESTED)
	(cd mlgmpidl; make mostlyclean)
	(cd apron; make mostlyclean)
	/bin/rm -f $(REQUESTED)

uninstall: $(REQUESTED)
	(cd mlgmpidl; make uninstall)
	(cd apron; make uninstall)

distclean: uninstall

doc: 
ifneq ($(HAS_OCAML),)
	(cd mlgmpidl; make mlgmpidl.pdf html)
endif
	(cd apron; make doc)

mlgmpidl/Makefile.config: Makefile.config Makefile
	( echo "HAS_MPFR=1"; cat Makefile.config; ) > $@

apron/Makefile.config: Makefile.config Makefile
	cp -f $< $@ 

# make distribution
PKGDIST = apron-dist
PKGNAME = apron
PKGVERSION = 0.9.10
PKGFILES = Makefile Makefile.config.model README README.mac README.windows
PKGDIRS  = apron mlgmpidl

dist:
	$(MAKE) all
	$(MAKE) doc
	mkdir -p $(PKGDIST)-$(PKGVERSION)
	$(MAKE) $(foreach pkg,$(PKGDIRS),pkg_$(pkg))
	cp $(PKGFILES) $(PKGDIST)-$(PKGVERSION)
	tar vczf $(PKGDIST)-$(PKGVERSION).tgz $(PKGDIST)-$(PKGVERSION)
	rm -rf $(PKGDIST)-$(PKGVERSION)

pkg_apron:
	(cd apron; $(MAKE) dist)
	(cd $(PKGDIST)-$(PKGVERSION); tar xzf ../apron/apron-$(PKGVERSION).tgz; mv $(PKGNAME)-$(PKGVERSION) $(PKGNAME))
	rm -f apron/apron-$(PKGVERSION).tgz

pkg_mlgmpidl:
	(cd mlgmpidl; $(MAKE) dist)
	(cd $(PKGDIST)-$(PKGVERSION); tar xzf $(HOME)/mlgmpidl.tgz)
	rm -f $(HOME)/mlgmpidl.tgz

