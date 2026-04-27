#
#
#

PART_IODIR =  $$(PARTITONER_DIR)/IO
SOURCES = $(shell ls *.f90)

MOD_PART_IO = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_PART_IO'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_IODIR = $(PART_IODIR)' >> $(MAKENAME)

lib_name:

lib_tasks:

libtarget:

lib_archve: libtarget


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_PART_IO)'       >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(PART_IODIR)' $(SOURCES)
