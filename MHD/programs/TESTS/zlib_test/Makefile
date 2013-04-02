#
#
#
TARGET_C_ZLIB_TEST =   test_zlib_c_IO
TARGET_F_ZLIB_TEST =   test_zlib_f_IO
TARGET_GETARG_TEST =   test_getarg

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
	@echo 'ZLIB_TEST_DIR =      $$(TESTS_DIR)/zlib_test'         >> $(MAKENAME)

target_list:
	@echo 'TARGET_C_ZLIB_TEST =   $$(BUILDDIR)/$(TARGET_C_ZLIB_TEST)'  \
	>> $(MAKENAME)
	@echo 'TARGET_F_ZLIB_TEST =   $$(BUILDDIR)/$(TARGET_F_ZLIB_TEST)'  \
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
	'$$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_F_ZLIB_TEST)': '$$(MOD_F_ZLIB_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_F_ZLIB_TEST)' \
	'$$(MOD_F_ZLIB_TEST)  -L. $$(LIB_KEMO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GETARG_TEST)': '$$(MOD_GETARG_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GETARG_TEST)' \
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
	@echo \
	comptest_zlib.o: \
	'$$(ZLIB_TEST_DIR)'/comptest_zlib.c \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(ZLIB_TEST_DIR)'/comptest_zlib.c \
	>> $(MAKENAME)
	
	@echo \
	test_zlib_lines.o: \
	'$$(ZLIB_TEST_DIR)'/test_zlib_lines.f90 \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -c '$$(F90FLAGS)' \
	'$$(ZLIB_TEST_DIR)'/test_zlib_lines.f90 \
	>> $(MAKENAME)
	
	@echo \
	getarg_test.o: \
	'$$(ZLIB_TEST_DIR)'/getarg_test.f90 \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -c '$$(F90FLAGS)' \
	'$$(ZLIB_TEST_DIR)'/getarg_test.f90 \
	>> $(MAKENAME)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
