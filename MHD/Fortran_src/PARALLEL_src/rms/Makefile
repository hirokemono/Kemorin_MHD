#
#
#

SR_DIR = $$(PARA_SRCDIR)/rms

LIB_SR = send_recv
SOURCES = $(shell ls *.f90)

MOD_RMS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_RMS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SR_DIR = $(SR_DIR)'  >> $(MAKENAME)


lib_name:
	@echo  'LIB_SR = ' $(LIB_SR) >> $(MAKENAME)
	@echo  'LIB_SR_FILE = lib$$(LIB_SR).a' \
	>> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB)' '$$@'  >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_SR_FILE): $$('$(MODULE_LIST)')' >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_RMS)'    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SR_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
