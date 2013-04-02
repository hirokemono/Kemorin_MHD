#
#
#

PART_PERIDIR = $$(PARTITONER_DIR)/periodic_model
SRC_PART_PERI = $(shell ls *.f90)
MOD_PART_PERI = $(addsuffix .o,$(basename $(SRC_PART_PERI)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_PERIDIR = $(PART_PERIDIR)' >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:


mod_list:
	@echo  MOD_PART_PERI=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_PERI) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PART_PERIDIR)' $(SRC_PART_PERI)
