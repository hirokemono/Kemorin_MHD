#
#
#

PART_IODIR =  $$(PARTITONER_DIR)/IO
SRC_PART_IO = $(shell ls *.f90)
MOD_PART_IO = $(addsuffix .o,$(basename $(SRC_PART_IO)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_IODIR = $(PART_IODIR)' >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:


mod_list:
	@echo  MOD_PART_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_IO) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PART_IODIR)' $(SRC_PART_IO)
