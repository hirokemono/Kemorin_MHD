#
#
#

ITP_ELEMENT_DIR =    $$(ITP_PLUG_DIR)/interpolate_element
TARGET_ITP_ELEMENT = $$(FEM_UTIL_BINDIR)/interpolate_element

SOURCES = $(shell ls *.f90)
MOD_ITP_ELEMENT = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_ELEMENT_DIR = $(ITP_ELEMENT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ITP_MESH = $(TARGET_ITP_MESH)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ITP_ELEMENT)': \
	'$$(MOD_ITP_ELEMENT)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_ELEMENT)' \
	'$$(MOD_ITP_ELEMENT)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_ITP_ELEMENT=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_ELEMENT)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ITP_ELEMENT_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
