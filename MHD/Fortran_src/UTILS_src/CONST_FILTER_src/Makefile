#
#
#

FILTER_SRCDIR = $$(UTILS_SRCDIR)/CONST_FILTER_src

LIB_GEN_FILTER =       -lcommute_3d
LIB_GEN_FILTER_FILE = libcommute_3d.a

SUBDIRS = \
filter_3d \
filter_z \
IO

FILTER3D_DIRS = \
filter_3d \
IO

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_SRCDIR = $(FILTER_SRCDIR)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_GEN_FILTER =      $(LIB_GEN_FILTER)'      >> $(MAKENAME)
	@echo  'LIB_GEN_FILTER_FILE = $(LIB_GEN_FILTER_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_GEN_FILTER_FILE): $$(MOD_GEN_FILTER)' >> $(MAKENAME)

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
	@echo MOD_GEN_FILTER= \\      >> $(MAKENAME)
	@echo '$$(MOD_3D_FILTER_MAT)' \\ >> $(MAKENAME)
	@echo '$$(MOD_Z_COMMUTE)' \\  >> $(MAKENAME)
	@echo '$$(MOD_GEN_FILTER_IO)'    >> $(MAKENAME)
	@echo >> $(MAKENAME)
	
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module \
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
