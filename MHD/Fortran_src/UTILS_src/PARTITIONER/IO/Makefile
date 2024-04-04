#
#
#

PART_IODIR =  $$(PARTITONER_DIR)/IO
SOURCES = $(shell ls *.f90)
MOD_PART_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_IODIR = $(PART_IODIR)' >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget


mod_list:
	@echo  MOD_PART_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_PART_IO) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PART_IODIR)' $(SOURCES)
