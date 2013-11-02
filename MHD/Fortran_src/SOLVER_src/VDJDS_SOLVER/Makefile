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

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_DJDS_SOLVER)' >> $(MAKENAME)


mod_list:
	@echo MOD_DJDS_SOLVER= \\             >> $(MAKENAME)
	@echo '$$(MOD_SOLVE_EACH)' \\         >> $(MAKENAME)
	@echo '$$(MOD_SOLVE_MATVEC)' \\       >> $(MAKENAME)
	@echo '$$(MOD_SOLVE_ORDERINGS)' \\    >> $(MAKENAME)
	@echo '$$(MOD_SOLVE_VECCALS)' \\      >> $(MAKENAME)
	@echo '$$(MOD_SOLVE_PRECONDS)' \\     >> $(MAKENAME)
	@echo '$$(MOD_SOLVE_SET_PRECONDS)' \\ >> $(MAKENAME)
	@echo '$$(MOD_SOLVER_MAINS)' \\       >> $(MAKENAME)
	@echo '$$(MOD_SOLVER_FL_MAINS)'       >> $(MAKENAME)
	@echo >> $(MAKENAME)
	
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module \
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
