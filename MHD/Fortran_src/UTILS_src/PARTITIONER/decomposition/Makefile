#
#
#

DECOMP_DIR = $$(PARTITONER_DIR)/decomposition
SOURCES = $(shell ls *.f90)
MOD_PART_DECOMP = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DECOMP_DIR = $(DECOMP_DIR)'  >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget


mod_list:
	@echo  MOD_PART_DECOMP=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_DECOMP) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(DECOMP_DIR)' $(SOURCES)
