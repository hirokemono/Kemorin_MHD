#
#
#

PART_PERIDIR = $$(PARTITONER_DIR)/periodic_model
SOURCES = $(shell ls *.f90)

MOD_PART_PERI = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_PART_PERI'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_PERIDIR = $(PART_PERIDIR)' >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_PART_PERI)'     >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(PART_PERIDIR)' $(SOURCES)
