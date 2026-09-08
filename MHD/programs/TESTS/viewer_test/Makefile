#
#
#
GRAPHIC_TEST_DIR = $$(TESTS_DIR)/viewer_test

TARGET_CPSF_READ_TEST = test_read_psf_c

CURRENTDIR = $(shell pwd)
SRC_C = $(shell ls *.c)


OBJ_CPSF_READ_TEST = \
test_read_psf_data.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GRAPHIC_TEST_DIR =   $(GRAPHIC_TEST_DIR)'      >> $(MAKENAME)

target_list:
	@echo 'TARGET_CPSF_READ_TEST = $$(TEST_BUILD_DIR)/$(TARGET_CPSF_READ_TEST)' \
	>> $(MAKENAME)
	
	@echo ''>> $(MAKENAME)

target_task:
	@echo '$$(TARGET_CPSF_READ_TEST)': '$$(OBJ_CPSF_READ_TEST) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CPSF_READ_TEST)' \
	'$$(OBJ_CPSF_READ_TEST)' \
	'-L. $$(LIB_KEMO_C) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  OBJ_CPSF_READ_TEST=  \\  >> $(MAKENAME)
	@echo  $(OBJ_CPSF_READ_TEST)    >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS) $(C_INCLUDE) -I. $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)'@'$$\(MHDDIR\)'@g' $(MAKENAME).tmp >> $(MAKENAME).depends
	@rm $(MAKENAME).tmp


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
