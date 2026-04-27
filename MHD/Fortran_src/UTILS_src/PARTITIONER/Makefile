#
#
#

PARTITONER_DIR =  $$(UTILS_SRCDIR)/PARTITIONER

LIB_PART_KEMO =       -lpart_kemo
LIB_PART_KEMO_FILE = libpart_kemo.a

MODULE_LIST=   'MOD_PART_KEMO'
UPPER_MODULES=

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
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_PART_KEMO_FILE): $$(MOD_PART_KEMO)' >> $(MAKENAME)


mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  $(MODULE_LIST)=  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MODULE_LIST)' \
		mod_list; \
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
