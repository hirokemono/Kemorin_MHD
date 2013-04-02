#
#
#

DECOMP_DIR = $$(PARTITONER_DIR)/decomposition
SRC_PART_DECOMP = $(shell ls *.f90)
MOD_PART_DECOMP = $(addsuffix .o,$(basename $(SRC_PART_DECOMP)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DECOMP_DIR = $(DECOMP_DIR)'  >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:


mod_list:
	@echo  MOD_PART_DECOMP=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_DECOMP) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(DECOMP_DIR)' $(SRC_PART_DECOMP)
