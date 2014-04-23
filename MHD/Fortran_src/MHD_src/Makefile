#
#
#

MHD_SRCDIR = $$(F_SRCDIR)/MHD_src

LIB_MHD=       -lkemo_MHD
LIB_MHD_FILE= libkemo_MHD.a

SUBDIRS = \
common \
evolution \
initial_data \
solver_vector_smp \
IO \
nodal_boundary \
surface_boundary \
fem_mhd_integration \
rhs_bc_vector \
sgs_model \
sgs_dynamic \
sgs_model_bc \
MG_for_MHD   \
\
sph_MHD

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
	@echo '$$(MOD_COMMON)'      \\  >> $(MAKENAME)
	@echo '$$(MOD_KEMO)'        \\  >> $(MAKENAME)
	@echo '$$(MOD_MHD_IO)'      \\  >> $(MAKENAME)
	@echo '$$(MOD_RHS)'         \\  >> $(MAKENAME)
	@echo '$$(MOD_NOD_BD)'      \\  >> $(MAKENAME)
	@echo '$$(MOD_SF_BD)'       \\  >> $(MAKENAME)
	@echo '$$(MOD_RHS_BC)'      \\  >> $(MAKENAME)
	@echo '$$(MOD_SGS)'         \\  >> $(MAKENAME)
	@echo '$$(MOD_SGS_DYNAMIC)' \\  >> $(MAKENAME)
	@echo '$$(MOD_SGS_BC)'      \\  >> $(MAKENAME)
	@echo '$$(MOD_EVOLVE)'      \\  >> $(MAKENAME)
	@echo '$$(MOD_MHD_SOLVE)'   \\  >> $(MAKENAME)
	@echo '$$(MOD_MG_4_MHD)'    \\  >> $(MAKENAME)
	@echo '$$(MOD_SPH_MHD)'         >> $(MAKENAME)
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
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
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
