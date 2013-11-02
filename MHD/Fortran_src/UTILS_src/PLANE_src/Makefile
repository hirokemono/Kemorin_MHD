#
#
#

PLANE_SRCDIR = $$(UTILS_SRCDIR)/PLANE_src

LIB_PLANE_MODEL =        -lplane_model
LIB_PLANE_MODEL_FILE =  libplane_model.a


SUBDIRS =     \
plane_core    \
IO            \
plane_spectr  \
correlation   \
cubmesh

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_SRCDIR = $(PLANE_SRCDIR)' >> $(MAKENAME)
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
	@echo  'LIB_PLANE_MODEL =      $(LIB_PLANE_MODEL)'      >> $(MAKENAME)
	@echo  'LIB_PLANE_MODEL_FILE = $(LIB_PLANE_MODEL_FILE)' >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

lib_tasks: libtarget lib_archve

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_PLANE_MODEL_FILE): $$(MOD_PLANE_MODEL)' >> $(MAKENAME);

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
	@echo MOD_PLANE_MODEL= \\           >> $(MAKENAME)
	@echo '$$(MOD_PLANE_COMMON)' \\     >> $(MAKENAME)
	@echo '$$(MOD_PLANE_IO)' \\         >> $(MAKENAME)
	@echo '$$(MOD_FFT_PLANE_COMMON)' \\ >> $(MAKENAME)
	@echo '$$(MOD_CORRELATE_PLANE)'  \\ >> $(MAKENAME)
	@echo '$$(MOD_CUBMESH)'          \\ >> $(MAKENAME)
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
