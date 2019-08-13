#
#
#

OBJ_GL2PS_C = \
gl2ps.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GL2PS_DIR =    $$(EXT_DIR)/gl2ps-1.3.6'      >> $(MAKENAME)

lib_name:

lib_archve:

mod_list:
	@echo  OBJ_GL2PS_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_GL2PS_C) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	gl2ps.o: \
	'$$(GL2PS_DIR)'/gl2ps.c \
	gl2ps.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GL2PS_DIR)'/gl2ps.c \
	>> $(MAKENAME)

includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(GL2PS_DIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(GL2PS_DIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done
