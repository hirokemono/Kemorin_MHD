#
#
#

CUBMESH_DIR = $$(PLANE_SRCDIR)/cubmesh

LIB_CUBMESH = cubmesh

SOURCES = $(shell ls *.f90)
MOD_CUBMESH = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CUBMESH_DIR = $(CUBMESH_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_CUBMESH)' >> $(MAKENAME)

mod_list:
	@echo  MOD_CUBMESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBMESH) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(CUBMESH_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
