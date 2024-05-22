#
#
#

KEMO_C_GLUTDIR = $$(C_SRCDIR)/KEMO_GLUT
CURRENTDIR = $(shell pwd)
SRC_GLUT_C = $(shell ls kemoview_*.c tree_view_*.c)
INC_GLUT_C = $(addsuffix .h,$(basename $(SRC_GLUT_C)) )
OBJ_GLUT_C = $(addsuffix .o,$(basename $(SRC_GLUT_C)) )

ifdef GLFW_CFLAGS
	SRC_GLUT_C+=  view_modifier_glfw.c
else
	SRC_GLUT_C+=  view_modifier_gtk.c
endif

OBJ_KEMOVIEW_GLUT= $(addsuffix .o,$(basename $(SRC_GLUT_C)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_GLUTDIR = $$(KEMO_C_GLUTDIR)'   >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget

mod_list:
	@echo  OBJ_KEMOVIEW_GLUT=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMOVIEW_GLUT)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@echo 'Construct dependencies in C_src/KEMO_GLUT'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_GLUT_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(GTK3_CFLAGS) $(GLFW_CFLAGS) $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$(GLFW_CFLAGS) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp > $(CURRENTDIR)/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
