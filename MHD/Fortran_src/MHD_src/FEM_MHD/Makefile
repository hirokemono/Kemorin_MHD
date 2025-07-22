#
#
#

FEM_MHD_DIR = $$(MHD_SRCDIR)/FEM_MHD
SOURCES = $(shell ls *.f90)
MOD_FEM_MHD = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_MHD_DIR = $(FEM_MHD_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_FEM_MHD)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_FEM_MHD=  \\ >> $(MAKENAME)
	@echo  $(MOD_FEM_MHD) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_MHD_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
