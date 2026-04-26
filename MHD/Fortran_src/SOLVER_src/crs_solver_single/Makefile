#
#
#

CRS_SOLVER_S_DIR = $$(SOLVER_SRCDIR)/crs_solver_single

MOD_CRS_SOLVER_S = 'MOD_CRS_SOLVER_S'

SUBDIRS = \
scalar


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CRS_SOLVER_S_DIR =  $(CRS_SOLVER_S_DIR)'       >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list; \
		) \
	done
	@echo '#' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_CRS_SOLVER_S)' >> $(MAKENAME)


mod_list:
	@echo '$(MOD_SOLVERS)+= $$(MOD_CRS_SOLVER_S)' >> $(MAKENAME)
	@echo '$(MOD_CRS_SOLVER_S)= '  >> $(MAKENAME)
	@echo '#'>> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MOD_CRS_SOLVER_S)' \
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
