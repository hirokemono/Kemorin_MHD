#
#
#

PART_COMMONDIR = $$(PARTITONER_DIR)/part_kemo
SOURCES = $(shell ls *.f90)
MOD_PART_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_COMMONDIR = $(PART_COMMONDIR)'      >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget


mod_list:
	@echo  MOD_PART_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_COMMON) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PART_COMMONDIR)' $(SOURCES)
