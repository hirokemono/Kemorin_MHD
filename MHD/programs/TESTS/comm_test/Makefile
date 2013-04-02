#
#
#

COMM_TEST_DIR =      $$(TESTS_DIR)/comm_test

TARGET_COMM_TEST =   comm_test
TARGET_F_COMM_TEST = filter_comm_test


LIBS_COMM_TEST =   \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_COMM_TEST = \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_COMM_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_COMM_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_COMM_TEST_CORE = \
m_geometry_4_comm_test.o \
\
set_diff_geom_comm_test.o \
collect_diff_4_comm_test.o \
write_diff_4_comm_test.o \

MOD_COMM_TEST = \
m_ctl_data_comm_test.o \
mesh_send_recv_test.o \
\
input_control_comm_test.o \
analyzer_comm_test.o \
main_comm_test.o

MOD_FILTER_COMM_TEST = \
m_geometry_filter_comm_test.o \
m_ctl_data_filter_comm_test.o \
input_ctl_filter_comm_test.o \
filter_send_recv_test.o \
set_diff_filter_comm_test.o \
collect_diff_filter_ctest.o \
write_diff_filter_comm_test.o \
\
analyzer_filter_comm_test.o \
main_filter_comm_test.o \

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'COMM_TEST_DIR =      $$(TESTS_DIR)/comm_test'         >> $(MAKENAME)

target_list:
	@echo 'TARGET_COMM_TEST =   $$(BUILDDIR)/$(TARGET_COMM_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_F_COMM_TEST = $$(BUILDDIR)/$(TARGET_F_COMM_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo comm_tests: \
	'$$(TARGET_COMM_TEST)       $$(TARGET_F_COMM_TEST)'     \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_COMM_TEST)': \
	'$$(LIB_FILES_COMM_TEST)   $$(MOD_COMM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_COMM_TEST)' \
	'$$(MOD_COMM_TEST)' \
	'-L. $$(LIBS_COMM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_F_COMM_TEST)': \
	'$$(LIB_FILES_COMM_TEST) $$(MOD_FILTER_COMM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_F_COMM_TEST)' \
	'$$(MOD_FILTER_COMM_TEST)' \
	'-L. $$(LIBS_COMM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_COMM_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_COMM_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_COMM_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_COMM_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_COMM_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_COMM_TEST) $(MOD_COMM_TEST_CORE) >> $(MAKENAME)
	@echo  MOD_FILTER_COMM_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_FILTER_COMM_TEST) $(MOD_COMM_TEST_CORE) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(COMM_TEST_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
