#
#
#

KEMO_PGDIR = $$(UTILS_SRCDIR)/draw_pg_f90

LIB_DRAW_PG =       -ldraw_by_pgplot
LIB_DRAW_PG_FILE = libdraw_by_pgplot.a

SOURCES = $(shell ls *.f90)
MOD_DRAW_PG = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_PGDIR = $(KEMO_PGDIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_DRAW_PG =      $(LIB_DRAW_PG)'      >> $(MAKENAME)
	@echo  'LIB_DRAW_PG_FILE = $(LIB_DRAW_PG_FILE)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_DRAW_PG_FILE): $$(MOD_DRAW_PG)' >> $(MAKENAME)

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_DRAW_PG)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_DRAW_PG=  \\ >> $(MAKENAME)
	@echo  $(MOD_DRAW_PG) >> $(MAKENAME)
	@echo   >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(KEMO_PGDIR)' $(SOURCES)


clean:
	rm -f *.o *.mod
