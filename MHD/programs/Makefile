#
#
#

SUBDIRS = \
FEM_MHD \
SPH_MHD \
VIZ_only \
interpolate \
construct_filter \
\
mesh_utilities \
data_utilities \
VIEWER \
\
Rayleigh_link \
\
TESTS



#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PROG_DIR = $$(MHDDIR)/programs'     >> $(MAKENAME)
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

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done

target_task:
	@echo 'parallels: '            \
	'fem_mhd  sph_mhd visualizer'  \
	'const_filteres itp_tools'     >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'utilities: '            \
	'data_utils mesh_utils'        \
	'rayleigh_link'                >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'FEM_minimum: '          \
	'fem_mhd cubed_sph_utils '     \
	'partitioners refineres'       >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		target_task \
		) \
	done


lib_name:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate library list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		lib_name \
		) \
	done

lib_tasks:


mod_list:
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
		module; \
		) \
	done

includes:
	cd VIEWER;  \
	echo 'generate symbolic link commands for ' $$dir ; \
	$(GMAKE) \
	GMAKE='$(GMAKE)' \
	MAKENAME='$(MAKENAME)' \
	includes


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
