#
#
#

MHD_SRCDIR = $$(F_SRCDIR)/MHD_src

LIB_MHD=       -lcalypso_MHD
LIB_MHD_FILE= libcalypso_MHD.a

SUBDIRS = \
IO \
sph_MHD \
field_data

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SRCDIR = $(MHD_SRCDIR)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo LIB_MHD =      $(LIB_MHD)  >> $(MAKENAME)
	@echo LIB_MHD_FILE = $(LIB_MHD_FILE)  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB)' '$$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo '$$(LIB_MHD_FILE): $$(MOD_MHD)' >> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done

mod_list:
	@echo MOD_MHD= \\ >> $(MAKENAME)
	@echo '$$(MOD_MHD_IO)'       \\  >> $(MAKENAME)
	@echo '$$(MOD_SPH_MHD)'      \\  >> $(MAKENAME)
	@echo '$$(MOD_SPH_SNAPSHOT)' \\  >> $(MAKENAME)
	@echo >> $(MAKENAME)
	
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( echo 'generate compile list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate dependencies for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		depends; \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
