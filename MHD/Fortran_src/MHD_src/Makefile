#
#
#

MHD_SRCDIR = $$(F_SRCDIR)/MHD_src

LIB_MHD=       -lkemo_MHD
LIB_MHD_FILE= libkemo_MHD.a

MODULE_LIST=   'MOD_MHD'
UPPER_MODULES=


SUBDIRS = \
common \
evolution \
initial_data \
solver_vector_smp \
IO \
nodal_boundary \
surface_boundary \
fem_mhd_integration \
sgs_model \
sgs_dynamic \
MG_for_MHD   \
\
Fortran2003 \
\
sph_MHD \
sph_SGS_MHD \
radial_FDM

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
	@echo '$$(LIB_MHD_FILE): $$('$(MODULE_LIST)')' >> $(MAKENAME)

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
	@echo '$(MODULE_LIST)= '  >> $(MAKENAME)
	@echo '#'>> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MODULE_LIST)' \
		mod_list; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct compile list for ' $$dir ; \
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
