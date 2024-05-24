#
#
#

PROG_DIR = $$(MHDDIR)/programs

SUBDIRS = \
VIEWER

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PROG_DIR = $(PROG_DIR)'     >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done

target_task:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		target_task \
		) \
	done


lib_name:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate library list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		lib_name \
		) \
	done

lib_tasks:


mod_list:
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
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct dependencies for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		CC='$(CC)'   \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MHDDIR='$(MHDDIR)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		DEPFLAGS='$(DEPFLAGS)' \
		OPTFLAGS='$(OPTFLAGS)' \
		C_INCLUDE='$(C_INCLUDE)' \
		FFMPEG_CFLAGS='$(FFMPEG_CFLAGS)' \
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
