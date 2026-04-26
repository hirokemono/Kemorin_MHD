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

MOD_DJDS_SOLVER = 'MOD_DJDS_SOLVER'

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
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_DJDS_SOLVER)' >> $(MAKENAME)


mod_list:
	@echo '$(MOD_SOLVERS)+= $$(MOD_DJDS_SOLVER)' >> $(MAKENAME)
	@echo 'MOD_DJDS_SOLVER= ' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MOD_DJDS_SOLVER)' \
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
