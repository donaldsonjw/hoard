#TOOLS
BIGLOO = bigloo
BGLAFILE = bglafile
BGLTAGS = bgltags
LD = ld
AR = ar 
RANLIB = ranlib
INSTALL = install
LINK = ln

BIGLOO_VERSION := $(shell $(BIGLOO) -eval "(print *bigloo-version*) (exit 0)" -q)
BIGLOOLIBDIR := $(shell $(BIGLOO) -eval "(print *default-lib-dir*) (exit 0)" -q)


#install related variables
DESTDIR = /usr
INSTLIBDIR = $(DESTDIR)/lib
INSTBIGLOOLIBDIR = $(INSTLIBDIR)/bigloo/$(BIGLOO_VERSION)




#Bigloo Flags
BHEAPFLAGS = -I src/Llib -unsafe -q -mkaddheap -mkaddlib \
	     -heap-library $(LIBNAME)

BCOMMONFLAGS = -mkaddlib -fsharing -q \
               -copt '$(CCOMMONFLAGS)' -I src/Llib

BSAFEFLAGS = $(BCOMMONFLAGS) -cg -O3 

BUNSAFEFLAGS = $(BCOMMONFLAGS) -O4 -unsafe

#C Flags
CCOMMONFLAGS = -fPIC

# directory variables
OBJDIR = objs
DISTDIR = dist

# sources
BIGLOOSRCS = $(wildcard src/Llib/*.scm)
TESTSRCS = $(wildcard recette/*.scm)
HEAPSRC = src/Misc/make_lib.scm


# object files

_SRCSWOUTDIR = $(foreach src, $(BIGLOOSRCS), $(notdir $(src)))
_BASEOBJSWOUTDIR = $(_SRCSWOUTDIR:%.scm=%)

SAFEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_s.o)

UNSAFEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_u.o)

_ESRCSWOUTDIR = $(foreach src, $(HEAPSRC), $(notdir $(src)))
_EBASEOBJSWOUTDIR = $(_ESRCSWOUTDIR:%.scm=%)

ESAFEOBJECTS = $(_EBASEOBJSWOUTDIR:%=$(OBJDIR)/%_s.o)

EUNSAFEOBJECTS = $(_EBASEOBJSWOUTDIR:%=$(OBJDIR)/%_u.o)

TESTOBJECTS = $(TESTSRCS:%.scm=%.o)

.SUFFIXES:

$(OBJDIR)/%_s.o:src/Llib/%.scm
	$(BIGLOO) -c $(BSAFEFLAGS) -o $@ $^

$(OBJDIR)/%_u.o:src/Llib/%.scm
	$(BIGLOO) -c $(BUNSAFEFLAGS) -o $@ $^

recette/%.o:recette/%.scm heap
	$(BIGLOO) -c $(BSAFEFLAGS) -L $(DISTDIR) -o $@ $<


#targets

all: .afile .etags lib 

test: recette/test$(LIBNAME)
	./recette/test$(LIBNAME)

.afile: $(BIGLOOSRCS)  $(TESTSRCS)
	$(BGLAFILE) -o $@  $^

.etags: $(BIGLOOSRCS) src/Llib/*.sch $(TESTSRCS)
	$(BGLTAGS) -o $@ $^

heap: $(DISTDIR)/$(LIBNAME).heap

$(DISTDIR)/$(LIBNAME).heap: $(DISTDIR) $(HEAPSRC)
	$(BIGLOO) $(BHEAPFLAGS) $(HEAPSRC)  -addheap $@

$(OBJDIR)/make_lib_s.o:src/Misc/make_lib.scm
	$(BIGLOO) -c $(BSAFEFLAGS) -o $@ $^

$(OBJDIR)/make_lib_u.o:src/Misc/make_lib.scm
	$(BIGLOO) -c $(BUNSAFEFLAGS) -o $@ $^


lib: init heap $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a   \
               $(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so \
	       $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a   \
	       $(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so

init: $(DISTDIR)/$(LIBNAME).init

$(DISTDIR)/$(LIBNAME).init : $(DISTDIR) src/Llib/*.sch src/Misc/$(LIBNAME).init.in 
	cat src/Misc/$(LIBNAME).init.in src/Llib/*.sch  > $@


$(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so: $(OBJDIR) $(DISTDIR) $(SAFEOBJECTS)
	$(LD) -G -o $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so $(SAFEOBJECTS) -lc -lm

$(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(SAFEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a $(SAFEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(ESAFEOBJECTS)
	$(LD) -G -o $(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so $(ESAFEOBJECTS) -lc -lm

$(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(UNSAFEOBJECTS)
	$(LD) -G -o $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so $(UNSAFEOBJECTS) -lc -lm

$(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(UNSAFEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a $(SAFEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(EUNSAFEOBJECTS)
	$(LD) -G -o $(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so $(EUNSAFEOBJECTS) -lc -lm


recette/test$(LIBNAME) : $(TESTOBJECTS) lib 
	$(BIGLOO) -static-all-bigloo $(BUNSAFEFLAGS) -L $(DISTDIR) -o $@ $(TESTOBJECTS) 


$(OBJDIR):
	mkdir $@

$(DISTDIR):
	mkdir $@

check-syntax: .afile
	$(BIGLOO) -afile .afile -syntax-check ${CHK_SOURCES}



install: all
	for file in $(DISTDIR)/*; do \
	  $(INSTALL) $$file $(INSTBIGLOOLIBDIR)/`basename $$file`; \
	done; \
	for file in $(DISTDIR)/*.so; do \
	  $(LINK) -s $(INSTBIGLOOLIBDIR)/`basename $$file` $(INSTLIBDIR)/`basename $$file`; \
	done

clean:
	rm -f $(TESTOBJECTS);\
	rm -f -r $(OBJDIR);\
	rm -f -r $(DISTDIR);\
	rm -f $(LIBNAME).heap;\
	rm -f $(LIBNAME).init;\
	rm -f recette/test$(LIBNAME)



