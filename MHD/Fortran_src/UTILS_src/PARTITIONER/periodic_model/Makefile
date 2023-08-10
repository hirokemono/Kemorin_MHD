#
#
#

PART_PERIDIR = $$(PARTITONER_DIR)/periodic_model
SOURCES = $(shell ls *.f90)
MOD_PART_PERI = $(addsuffix .o,$(basename $(SOURCES)) )

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
	@echo  MOD_PART_PERI=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_PERI) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PART_PERIDIR)' $(SOURCES)
