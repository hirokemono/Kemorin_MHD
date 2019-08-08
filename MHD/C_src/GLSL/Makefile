#
#
#

F90 = gfortran-mp-8

SHADERDIR = $$(C_SRCDIR)/GLSL

SHADER_PREFIX = shaders

OBJ_SHADERS = \
$(SHADER_PREFIX).o

SRC_SHADERS = $(shell ls *.vert *.frag)

MAKE_C_SHADER = shader_into_c
CURRENTDIR = $(shell pwd)

#
#  -------------------------------------------------------------------------
#

all: $(SHADER_PREFIX).c

$(MAKE_C_SHADER): ../../module_dependency/$(MAKE_C_SHADER).f90
	$(F90) -o $(MAKE_C_SHADER) $<

$(SHADER_PREFIX).c: $(MAKE_C_SHADER) $(SRC_SHADERS)
	./$(MAKE_C_SHADER) $(SHADER_PREFIX) $(CURRENTDIR) $(SRC_SHADERS)
	rm $(MAKE_C_SHADER) shader_into_c_source.*

dir_list: $(SHADER_PREFIX).c
	@echo 'SHADERDIR = $(SHADERDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(OBJ_SHADERS)' >> $(MAKENAME)

mod_list:
	@echo  OBJ_SHADERS=  \\ >> $(MAKENAME)
	@echo  $(OBJ_SHADERS)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

module:
	@echo \
	$(SHADER_PREFIX).o: \
	'$$(SHADERDIR)'/$(SHADER_PREFIX).c \
	$(SHADER_PREFIX).h \
	kemoviewer.h \
	skip_comment_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)

includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(SHADERDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(SHADERDIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
