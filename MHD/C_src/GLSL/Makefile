#
#
#

F90 = gfortran-mp-8

# SHADERDIR = $$(C_SRCDIR)/GLSL
SHADERDIR = ./
SRC_SHADERS = $(shell ls *.vert *.frag)
MOD_BASE = $(addsuffix .o,$(basename $(SRC_BASE)) )

MAKE_C_SHADER = shader_into_c
SHADER_PREFIX = shaders
#
#  -------------------------------------------------------------------------
#

all: $(SHADER_PREFIX).c

dir_list:
	@echo 'BASEDIR = $(BASEDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_BASE)' >> $(MAKENAME)


mod_list:

$(MAKE_C_SHADER): ../../module_dependency/$(MAKE_C_SHADER).f90
	$(F90) -o $(MAKE_C_SHADER) $<

$(SHADER_PREFIX).c: $(MAKE_C_SHADER) gouraud.vert gouraud.frag
	$(MAKE_C_SHADER) $(SHADER_PREFIX) $(SHADERDIR) $(SRC_SHADERS)
	rm $(MAKE_C_SHADER) shader_into_c_source.*

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
