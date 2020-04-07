#
#
#
GRAPHIC_TEST_DIR = $$(TESTS_DIR)/graphic_test

TARGET_BMP_TEST =      test_BMP_files
TARGET_C_PNG_TEST =    test_PNG_C

TARGET_CPSF_READ_TEST = test_read_psf_c
TARGET_CBMP_READ_TEST = test_read_bmp
TARGET_CPNG_READ_TEST = test_read_png
TARGET_PNG_2_BMP_F =    test_png_2_bmp_f

SOURCES = $(shell ls *.f90 *.F90)
SRC_C = $(shell ls *.c)


MOD_BMP_TEST = \
write_bmp_image.o \
test_bmp_images.o

MOD_C_PNG_TEST = \
test_wt_png_c.o \
write_image_2_png.o

OBJ_CPSF_READ_TEST = \
test_read_psf_data.o

OBJ_CPNG_READ_TEST = \
test_read_png_c.o

OBJ_CBMP_READ_TEST = \
test_read_bmp_c.o

OBJ_PNG_2_BMP_F = \
write_bmp_image.o \
png_to_bmp_f.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GRAPHIC_TEST_DIR =   $(GRAPHIC_TEST_DIR)'      >> $(MAKENAME)

target_list:
	@echo 'TARGET_BMP_TEST =      $$(BUILDDIR)/$(TARGET_BMP_TEST)'     \
	>> $(MAKENAME)
	@echo 'TARGET_C_PNG_TEST =    $$(BUILDDIR)/$(TARGET_C_PNG_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CPSF_READ_TEST = $$(BUILDDIR)/$(TARGET_CPSF_READ_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CBMP_READ_TEST = $$(BUILDDIR)/$(TARGET_CBMP_READ_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CPNG_READ_TEST = $$(BUILDDIR)/$(TARGET_CPNG_READ_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_PNG_2_BMP_F = $$(BUILDDIR)/$(TARGET_PNG_2_BMP_F)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo f_graphic_tests: \
	'$$(TARGET_BMP_TEST) $$(TARGET_PNG_2_BMP_F)'  \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo c_graphic_tests: \
	'$$(TARGET_C_PNG_TEST)  $$(TARGET_CPSF_READ_TEST)' \
	'$$(TARGET_CPNG_READ_TEST)  $$(TARGET_CBMP_READ_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_BMP_TEST)': '$$(MOD_BMP_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_BMP_TEST)' \
	'$$(MOD_BMP_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '$$(TARGET_C_PNG_TEST)': '$$(MOD_C_PNG_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_C_PNG_TEST)' \
	'$$(MOD_C_PNG_TEST)' \
	'$$(PNG_LIBS) $$(ZLIB_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CPSF_READ_TEST)': '$$(OBJ_CPSF_READ_TEST) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CPSF_READ_TEST)' \
	'$$(OBJ_CPSF_READ_TEST)' \
	'-L. $$(LIB_KEMO_C) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CBMP_READ_TEST)': '$$(OBJ_CBMP_READ_TEST) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CBMP_READ_TEST)' \
	'$$(OBJ_CBMP_READ_TEST)' \
	'-L. $$(LIB_KEMO_C)  $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CPNG_READ_TEST)': '$$(OBJ_CPNG_READ_TEST)  $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CPNG_READ_TEST)' \
	'$$(OBJ_CPNG_READ_TEST)' \
	'-L. $$(LIB_KEMO_C)  $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PNG_2_BMP_F)': '$$(OBJ_PNG_2_BMP_F) $$(LIB_MESH_FILE) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PNG_2_BMP_F)' \
	'$$(OBJ_PNG_2_BMP_F)' \
	'-L. $$(LIB_MESH) $$(LIB_KEMO_C)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_BMP_TEST=      \\    >> $(MAKENAME)
	@echo  $(MOD_BMP_TEST)          >> $(MAKENAME)
	@echo  MOD_C_PNG_TEST=    \\    >> $(MAKENAME)
	@echo  $(MOD_C_PNG_TEST)        >> $(MAKENAME)
	@echo  OBJ_CPSF_READ_TEST=  \\  >> $(MAKENAME)
	@echo  $(OBJ_CPSF_READ_TEST)    >> $(MAKENAME)
	@echo  OBJ_CBMP_READ_TEST=  \\  >> $(MAKENAME)
	@echo  $(OBJ_CBMP_READ_TEST)    >> $(MAKENAME)
	@echo  OBJ_CPNG_READ_TEST=  \\  >> $(MAKENAME)
	@echo  $(OBJ_CPNG_READ_TEST)    >> $(MAKENAME)
	@echo  OBJ_PNG_2_BMP_F=  \\     >> $(MAKENAME)
	@echo  $(OBJ_PNG_2_BMP_F)       >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(GRAPHIC_TEST_DIR)' $(SOURCES)
	
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS) $(C_INCLUDE) -I. $(MHDDIR)/programs/TESTS/graphic_test/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)'@'$$\(MHDDIR\)'@g' $(MAKENAME).tmp >> $(MHDDIR)//programs/TESTS/graphic_test/Makefile.depends
	@rm $(MAKENAME).tmp


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
