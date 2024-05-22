#
#
#
ZLIB_TEST_DIR = $$(TESTS_DIR)/zlib_test

TARGET_C_ZLIB_TEST =   test_zlib_c_IO
TARGET_F_ZLIB_TEST =   test_zlib_f_IO
TARGET_GETARG_TEST =   test_getarg

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90)
SRC_C = $(shell ls *.c)

MOD_C_ZLIB_TEST = \
comptest_zlib.o

MOD_F_ZLIB_TEST = \
test_zlib_lines.o

MOD_GETARG_TEST = \
getarg_test.o
#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ZLIB_TEST_DIR = $(ZLIB_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_C_ZLIB_TEST =   $$(TEST_BUILD_DIR)/$(TARGET_C_ZLIB_TEST)'  \
	>> $(MAKENAME)
	@echo 'TARGET_F_ZLIB_TEST =   $$(TEST_BUILD_DIR)/$(TARGET_F_ZLIB_TEST)'  \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo zlib_tests: \
	'$$(TARGET_C_ZLIB_TEST)  $$(TARGET_F_ZLIB_TEST)'  \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo getarg_tests: \
	'$$(TARGET_GETARG_TEST)'  \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_C_ZLIB_TEST)': '$$(MOD_C_ZLIB_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_C_ZLIB_TEST)' \
	'$$(MOD_C_ZLIB_TEST)' \
	'$$(ZLIB_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_F_ZLIB_TEST)': '$$(MOD_F_ZLIB_TEST) $$(LIB_KEMO_FILE) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_F_ZLIB_TEST)' \
	'$$(MOD_F_ZLIB_TEST)  -L. $$(LIB_KEMO_C) $$(LIB_KEMO) $$(LIB_KEMO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GETARG_TEST)': '$$(MOD_GETARG_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GETARG_TEST)' \
	'$$(MOD_GETARG_TEST)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_C_ZLIB_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_C_ZLIB_TEST)     >> $(MAKENAME)
	@echo  MOD_F_ZLIB_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_F_ZLIB_TEST)     >> $(MAKENAME)
	@echo  MOD_GETARG_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_GETARG_TEST)     >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(ZLIB_TEST_DIR)' $(SOURCES)
	
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS) $(C_INCLUDE) -I. $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)'@'$$\(MHDDIR\)'@g' $(MAKENAME).tmp >> $(CURRENTDIR)/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
