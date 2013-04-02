#
#
#

PICK_SURFDIR =    $$(UTILS_SRCDIR)/pick_surface

LIB_PICKSURF =       -lpick_surf
LIB_PICKSURF_FILE = libpick_surf.a

SRC_PICK_SURFACE = $(shell ls *.f90 *.F90)
MOD_PICK_SURFACE = $(addsuffix .o,$(basename $(SRC_PICK_SURFACE)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'PICK_SURFDIR = $(PICK_SURFDIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_PICKSURF =      $(LIB_PICKSURF)'      >> $(MAKENAME)
	@echo  'LIB_PICKSURF_FILE = $(LIB_PICKSURF_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_PICKSURF_FILE): $$(MOD_PICK_SURFACE)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_PICK_SURFACE)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_PICK_SURFACE=  \\ >> $(MAKENAME)
	@echo  $(MOD_PICK_SURFACE) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PICK_SURFDIR)' $(SRC_PICK_SURFACE)
