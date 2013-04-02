#
#
#

PART_COMMONDIR = $$(PARTITONER_DIR)/part_kemo
SRC_PART_COMMON = $(shell ls *.f90)
MOD_PART_COMMON = $(addsuffix .o,$(basename $(SRC_PART_COMMON)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_COMMONDIR = $(PART_COMMONDIR)'      >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:


mod_list:
	@echo  MOD_PART_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_COMMON) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PART_COMMONDIR)' $(SRC_PART_COMMON)
