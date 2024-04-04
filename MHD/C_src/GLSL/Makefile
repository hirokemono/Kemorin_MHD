#
#
#

F90_LOCAL = gfortran-mp-8

SHADERDIR = $$(C_SRCDIR)/GLSL
SRC_GLSL_C = $(shell ls *.c)
INC_GLSL_C = $(addsuffix .h,$(basename $(SRC_GLSL_C)) )
OBJ_SHADERS = $(addsuffix .o,$(basename $(SRC_GLSL_C)) )


SHADER_PREFIX = shaders

MAKE_C_SHADER = shader_into_c
CURRENTDIR = $(shell pwd)

SRC_MAKE_SHADER = $(MAKE_C_SHADER).f90
SRC_SHADERS = $(shell ls *.vert *.frag)

#
#  -------------------------------------------------------------------------
#

all: wrap_shader

wrap_shader: $(SHADER_PREFIX).c

$(SHADER_PREFIX).c: $(SRC_SHADERS) $(SRC_MAKE_SHADER)
	@echo 'Wrap shader sources in C source'
	$(F90_LOCAL) -o $(MAKE_C_SHADER) $(SRC_MAKE_SHADER)
	./$(MAKE_C_SHADER) $(SHADER_PREFIX) $(CURRENTDIR) $(SRC_SHADERS)
	rm $(MAKE_C_SHADER) shader_into_c_source.*

dir_list: $(SHADER_PREFIX).c
	@echo 'SHADERDIR = $(SHADERDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(OBJ_SHADERS)' >> $(MAKENAME)

mod_list:
	@echo  OBJ_SHADERS=  \\ >> $(MAKENAME)
	@echo  $(OBJ_SHADERS)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends: wrap_shader
	@echo 'Construct dependencies in C_src/GLSL'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_GLSL_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(MHDDIR)/C_src/GLSL/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp > $(MHDDIR)/C_src/GLSL/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
