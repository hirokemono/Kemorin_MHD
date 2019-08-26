#
#
#

OBJ_KEMOVIEW_GLUT = \
kemoview_glut_routines.o \
const_viewer_menu_glut.o \
view_modifier_glut.o \
\
kemoview_fileselector_gtk.o \
kemoview_gtk_PSF_surface_menu.o \
kemoview_gtk_PSF_vector_menu.o \
kemoview_gtk_colormap_menu.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_GLUTDIR = $$(C_SRCDIR)/KEMO_GLUT'   >> $(MAKENAME)

lib_name:

lib_archve:

mod_list:
	@echo  OBJ_KEMOVIEW_GLUT=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMOVIEW_GLUT)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	const_viewer_menu_glut.o: \
	'$$(KEMO_C_GLUTDIR)'/const_viewer_menu_glut.c \
	const_viewer_menu_glut.h \
	kemoviewer.h \
	view_modifier_glut.o \
	view_modifier_glut.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	view_modifier_glut.o: \
	'$$(KEMO_C_GLUTDIR)'/view_modifier_glut.c \
	view_modifier_glut.h \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_glut_routines.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_glut_routines.c \
	kemoview_glut_routines.h \
	kemoviewer.h \
	view_modifier_glut.o \
	view_modifier_glut.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_fileselector_gtk.o:  \
	'$$(KEMO_C_GLUTDIR)'/kemoview_fileselector_gtk.c  \
	kemoview_fileselector_gtk.h \
	kemoviewer.h \
	kemoview_glut_routines.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_PSF_surface_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_PSF_surface_menu.c \
	kemoview_gtk_PSF_surface_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.h.o \
	tree_view_4_pvr_colormap.o \
	kemoview_fileselector_gtk.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_PSF_vector_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_PSF_vector_menu.c \
	kemoview_gtk_PSF_vector_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.h.o \
	tree_view_4_pvr_colormap.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_colormap_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_colormap_menu.c \
	kemoview_gtk_colormap_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.h.o \
	tree_view_4_pvr_colormap.o \
	tree_view_4_light_position.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_PSF_surface_menu.o \
	kemoview_gtk_PSF_vector_menu.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)

includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMO_C_GLUTDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMO_C_GLUTDIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done
