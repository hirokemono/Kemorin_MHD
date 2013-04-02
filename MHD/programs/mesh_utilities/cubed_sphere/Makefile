#
#
#

CUBE_SPH_DIR = $$(MESHER_DIR)/cubed_sphere

SUBDIRS = \
cubed_sph_mesh \
cut_shell \
add_element_grp


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CUBE_SPH_DIR = $(CUBE_SPH_DIR)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_task:
	@echo cubed_sph_utils: \
	'$$(TARGET_CUBED_SPH)'   \
	'$$(TARGET_RECT_SPH)'    \
	'$$(TARGET_SQURE_CIRC)'  \
	'$$(TARGET_CUTSHELL)'    \
	'$$(TARGET_ADDGRP_SPH) ' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:

mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done

module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
