#
#
#
SORT_TEST_DIR = $$(TESTS_DIR)/Sorting_test

TARGET_SORT_TEST =   test_sortings

CURRENTDIR = $(shell pwd)
SRC_C = $(shell ls *.c)

MOD_SORT_TEST = \
kemo_bitonic_sort.o \
\
array_for_sorting_test.o \
double_sorting_tests.o \
float_sorting_tests.o \
integer_sorting_tests.o \
\
bitonic_sort_omp.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SORT_TEST_DIR = $(SORT_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SORT_TEST =   $$(TEST_BUILD_DIR)/$(TARGET_SORT_TEST)'  \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sort_test: \
	'$$(TARGET_SORT_TEST)'  \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SORT_TEST)': '$$(MOD_SORT_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_SORT_TEST)' \
	'$$(MOD_SORT_TEST) -L. $$(LIB_KEMO_C)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SORT_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_SORT_TEST)     >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(OPTFLAGS) $(C_INCLUDE) -I. $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)'@'$$\(MHDDIR\)'@g' $(MAKENAME).tmp > $(CURRENTDIR)/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
