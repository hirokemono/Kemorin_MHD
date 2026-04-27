#
#
#

PART_COMMONDIR = $$(PARTITONER_DIR)/part_kemo
SOURCES = $(shell ls *.f90)

MOD_PART_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_PART_COMMON'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_COMMONDIR = $(PART_COMMONDIR)'      >> $(MAKENAME)

lib_name:

lib_tasks:

libtarget:

lib_archve: libtarget


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_PART_COMMON)'   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(PART_COMMONDIR)' $(SOURCES)
