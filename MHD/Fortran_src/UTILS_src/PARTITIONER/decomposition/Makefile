#
#
#

DECOMP_DIR = $$(PARTITONER_DIR)/decomposition
SOURCES = $(shell ls *.f90)

MOD_PART_DECOMP = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_PART_DECOMP'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DECOMP_DIR = $(DECOMP_DIR)'  >> $(MAKENAME)

lib_name:

lib_tasks:

libtarget:

lib_archve: libtarget


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_PART_DECOMP)'   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(DECOMP_DIR)' $(SOURCES)
