#
#
#

CRS_SOLVER_DIR = $$(SOLVER_SRCDIR)/crs_solver

SUBDIRS = \
scalar \
block33 \
blockNN

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CRS_SOLVER_DIR =  $(CRS_SOLVER_DIR)'       >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_CRS_SOLVER)' >> $(MAKENAME)


mod_list:
	@echo MOD_CRS_SOLVER= \\ >> $(MAKENAME)
	@echo '$$(MOD_SOLVER_11)' \\ >> $(MAKENAME)
	@echo '$$(MOD_SOLVER_33)' \\ >> $(MAKENAME)
	@echo '$$(MOD_SOLVER_NN)'    >> $(MAKENAME)
	@echo >> $(MAKENAME)
	
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module \
		) \
	done

clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
