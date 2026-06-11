#
#
#

ITP_RESTART_DIR =    $$(ITP_PLUG_DIR)/interpolate_restart
TARGET_ITP_RESTART = $$(FEM_UTIL_BINDIR)/interpolate_restart

SOURCES = $(shell ls *.f90)
MOD_ITP_RESTART = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_RESTART_DIR = $(ITP_RESTART_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_ITP_RESTART = $(TARGET_ITP_RESTART)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ITP_RESTART)': \
	'$$(MOD_ITP_RESTART)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_RESTART)' \
	'$$(MOD_ITP_RESTART)' '-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_ITP_RESTART=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_RESTART)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ITP_RESTART_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
