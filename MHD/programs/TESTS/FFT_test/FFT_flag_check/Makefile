#
#
#
FFT_FLAG_CHECK_DIR = $$(FFT_TEST_DIR)/FFT_flag_check
TARGET_FFT_LABEL_CHECK = $$(TEST_BUILD_DIR)/check_FFT_labels
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_FFT_LABEL_CHECK = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_FLAG_CHECK_DIR = $(FFT_FLAG_CHECK_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FFT_LABEL_CHECK = $(TARGET_FFT_LABEL_CHECK)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_FFT_LABEL_CHECK)'           >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FFT_LABEL_CHECK)': \
	'$$(MOD_FFT_LABEL_CHECK) $$(LIB_MESH_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FFT_LABEL_CHECK)' \
	'$$(MOD_FFT_LABEL_CHECK)' \
	'-L. $$(LIB_MESH) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FFT_LABEL_CHECK=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_LABEL_CHECK)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_FLAG_CHECK_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean
