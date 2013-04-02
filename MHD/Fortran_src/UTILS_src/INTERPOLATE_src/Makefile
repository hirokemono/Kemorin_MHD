#
#
#

ITP_SRCDIR = $$(UTILS_SRCDIR)/INTERPOLATE_src

LIB_ITP_TABLE =       -lgen_itp_table
LIB_ITP_TABLE_FILE = libgen_itp_table.a

SRC_ITP_TABLE = $(shell ls *.f90)
MOD_ITP_TABLE = $(addsuffix .o,$(basename $(SRC_ITP_TABLE)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_SRCDIR = $(ITP_SRCDIR)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


lib_name:
	@echo  'LIB_ITP_TABLE =      $(LIB_ITP_TABLE)' >> $(MAKENAME)
	@echo  'LIB_ITP_TABLE_FILE = $(LIB_ITP_TABLE_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_ITP_TABLE_FILE): $$(MOD_ITP_TABLE)' >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_ITP_TABLE)'  >> $(MAKENAME)

mod_list:
	@echo MOD_ITP_TABLE=  \\ >> $(MAKENAME)
	@echo $(MOD_ITP_TABLE) >> $(MAKENAME)
	@echo >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(ITP_SRCDIR)' $(SRC_ITP_TABLE)
	@echo  '#' >> $(MAKENAME)


set_clean:

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a *.L
