#
#
#

PARTITONER_DIR =  $$(UTILS_SRCDIR)/PARTITIONER

LIB_PART_KEMO =       -lpart_kemo
LIB_PART_KEMO_FILE = libpart_kemo.a

SUBDIRS = \
part_kemo     \
decomposition \
IO \
periodic_model

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PARTITONER_DIR =  $(PARTITONER_DIR)' >> $(MAKENAME)
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

lib_name:
	@echo  'LIB_PART_KEMO =      $(LIB_PART_KEMO)'      >> $(MAKENAME)
	@echo  'LIB_PART_KEMO_FILE = $(LIB_PART_KEMO_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_PART_KEMO)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_PART_KEMO=  \\ >> $(MAKENAME)
	@echo  '$$(MOD_PART_COMMON)' \\ >> $(MAKENAME)
	@echo  '$$(MOD_PART_DECOMP)' \\ >> $(MAKENAME)
	@echo  '$$(MOD_PART_IO)'     \\ >> $(MAKENAME)
	@echo  '$$(MOD_PART_PERI)'      >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_PART_KEMO_FILE): $$(MOD_PART_KEMO)' >> $(MAKENAME)


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
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
