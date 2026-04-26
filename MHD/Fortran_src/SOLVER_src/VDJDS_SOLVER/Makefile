#
#
#

DJDS_SOLVER_DIR = $$(SOLVER_SRCDIR)/VDJDS_SOLVER

SUBDIRS = \
cal_4_each_solvers \
cal_matvecs \
orderings \
cal_vectors \
precondition \
set_precondition \
main_4_solvers

MODULE_LIST= 'MOD_DJDS_SOLVER'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DJDS_SOLVER_DIR =  $(DJDS_SOLVER_DIR)'       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo  '#' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\      >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES=$(MODULE_LIST) \
		mod_list \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		depends \
		) \
	done

clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
