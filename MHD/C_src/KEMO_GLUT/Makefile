#
#
#

KEMO_C_GLUTDIR = $$(C_SRCDIR)/KEMO_GLUT
SRC_GLUT_C = $(shell ls *.c)
INC_GLUT_C = $(addsuffix .h,$(basename $(SRC_GLUT_C)) )
OBJ_GLUT_C = $(addsuffix .o,$(basename $(SRC_GLUT_C)) )

ifdef GLFW_LIBS
	OBJ_KEMOVIEW_GLUT = view_modifier_glfw.o
else
	OBJ_KEMOVIEW_GLUT = view_modifier_gtk.o
endif

OBJ_KEMOVIEW_GLUT += \
kemoview_fileselector_gtk.o \
kemoview_gtk_PSF_surface_menu.o \
kemoview_gtk_PSF_isoline_menu.o \
kemoview_gtk_PSF_vector_menu.o \
kemoview_gtk_PSF_menu.o \
kemoview_gtk_mesh_menu.o \
kemoview_gtk_fline_selectors.o \
kemoview_gtk_fieldline_menu.o \
kemoview_gtk_axis_menu.o \
kemoview_gtk_rotation_menu.o \
kemoview_gtk_evolution_menu.o \
kemoview_gtk_quilt_menu.o \
kemoview_gtk_viewmatrix_menu.o \
kemoview_gtk_main_menu.o \
kemoview_gtk_preference_menu.o \
\
tree_view_4_light_position.o \
tree_view_4_domain_viewer.o \
tree_view_4_ele_group_viewer.o \
tree_view_4_surf_group_viewer.o \
tree_view_4_nod_group_viewer.o \
tree_view_4_viewer_mesh.o \
tree_view_kemoview_colormap.o

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
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(GTK3_CFLAGS) $(GLFW_CFLAGS) $(MHDDIR)/C_src/KEMO_GLUT/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$(GLFW_CFLAGS) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp > $(MHDDIR)/C_src/KEMO_GLUT/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
