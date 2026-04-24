#
#
#

PARA_SRCDIR =   $$(F_SRCDIR)/PARALLEL_src

LIB_PARA =       -lcalypso_parallel
LIB_PARA_FILE = libcalypso_parallel.a

UPPER_MODULES = MOD_PARALLEL

SUBDIRS = \
COMM_src        \
MPI_IO          \
MPI_ZLIB_IO     \
INTERPOLATE     \
SPH_SHELL_src   \
CONST_SPH_GRID

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PARA_SRCDIR = $(PARA_SRCDIR)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list; \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_PARA =      $(LIB_PARA)' >> $(MAKENAME)
	@echo  'LIB_PARA_FILE = $(LIB_PARA_FILE)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_PARA_FILE):  $$(MOD_PARALLEL) ' >> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done

mod_list:
	@echo  '$(UPPER_MODULES)= ' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(UPPER_MODULES)' \
		mod_list; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate compile list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		depends; \
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
