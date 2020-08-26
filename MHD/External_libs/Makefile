#
#

SUBDIRS =  \
FFTPACK5.1D \
DCL_custum  \
\
mt_stream_f90-1.11

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'EXT_DIR =  $$(MHDDIR)/External_libs'      >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
		dir_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@for dir in  $(SUBDIRS) ; do \
		( \
		echo 'generate library list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		lib_name; \
		 ) \
	done

lib_tasks:
	@for dir in $(SUBDIRS) ; do \
		( cd $${dir};  \
		echo 'generate library commands for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		OPENGL_LIBS='$(OPENGL_LIBS)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		lib_tasks; \
		 ) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		SSE_FLAG="$(SSE_FLAG)" \
		SSE_CPU="$(SSE_CPU)" \
		SSE4="$(SSE4)" \
		SSE8="$(SSE8)" \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate compile commands for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		SSE_FLAG="$(SSE_FLAG)" \
		SSE_CPU="$(SSE_CPU)" \
		SSE4="$(SSE4)" \
		SSE8="$(SSE8)" \
		module; \
		) \
	done

depends:
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate dependencies for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
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
