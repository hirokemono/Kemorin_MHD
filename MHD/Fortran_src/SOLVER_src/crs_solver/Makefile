#
#
#

CRS_SOLVER_DIR = $$(SOLVER_SRCDIR)/crs_solver

MOD_CRS_SOLVER= 'MOD_CRS_SOLVER'

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
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_CRS_SOLVER)' >> $(MAKENAME)


mod_list:
	@echo '$(MOD_SOLVERS)+= $$(MOD_CRS_SOLVER)' >> $(MAKENAME)
	@echo '$(MOD_CRS_SOLVER)= ' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		MOD_CRS_SOLVER='$(MOD_CRS_SOLVER)' \
		mod_list \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct compile list for ' $$dir ; \
		cd $${dir};  \
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
