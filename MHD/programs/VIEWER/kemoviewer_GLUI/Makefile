#
#
#

TARGET_KEMOVIEW_UI = 'kemoviewer_glui'

OBJ_KEMOVIEW_GLUI =     \
kemoviewer_glui.o            \
kemo_mesh_viewer_glui.o      \
kemoview_glui_window_input.o \
kemoview_fileselector_glui.o \
'$$(OBJ_KEMOVIEW_GLUT)'

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'KEMOVIEW_GLUIDIR = $$(KEMOVIEWER_DIR)/kemoviewer_GLUI' >> $(MAKENAME)

target_list:
	@echo 'TARGET_KEMOVIEW_UI = $$(BUILDDIR)/$(TARGET_KEMOVIEW_UI)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo viewer_glui: '$$(TARGET_KEMOVIEW_UI)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_UI)': '$$(OBJ_KEMOVIEW_GLUI)' \
	'$$(LIB_KEMO_C_FILE) $$(LIB_GLUI_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' '$$(CXXFLAGS)' ' $$(GLUI_INC)' -o '$$(TARGET_KEMOVIEW_UI)' \
	'$$(OBJ_KEMOVIEW_GLUI) -L. $$(LIB_KEMO_C) $$(LIB_GLUI)' \
	'$$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  OBJ_KEMOVIEW_GLUI =  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMOVIEW_GLUI) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	kemoviewer_glui.o:  \
	'$$(KEMOVIEW_GLUIDIR)'/kemoviewer_glui.cpp  \
	kemo_mesh_viewer_glui.o \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GLUI_INC)' \
	'$$(KEMOVIEW_GLUIDIR)'/kemoviewer_glui.cpp \
	>> $(MAKENAME)
	
	@echo \
	kemoview_glui_window_input.o: \
	'$$(KEMOVIEW_GLUIDIR)'/kemoview_glui_window_input.cpp \
	kemoview_glui_window_input.h \
	kemoview_glut_routines.o \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c '$$(CXXFLAGS) $$(C_INCLUDE) $$(GLUI_INC)' \
	'$$(KEMOVIEW_GLUIDIR)'/kemoview_glui_window_input.cpp \
	>> $(MAKENAME)
	
	@echo \
	kemoview_fileselector_glui.o: \
	'$$(KEMOVIEW_GLUIDIR)'/kemoview_fileselector_glui.cpp \
	kemoview_fileselector_glui.h \
	kemoview_glut_routines.o \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c '$$(CXXFLAGS) $$(C_INCLUDE) $$(GLUI_INC)' \
	'$$(KEMOVIEW_GLUIDIR)'/kemoview_fileselector_glui.cpp \
	>> $(MAKENAME)
	
	@echo \
	kemo_mesh_viewer_glui.o: \
	'$$(KEMOVIEW_GLUIDIR)'/kemo_mesh_viewer_glui.cpp \
	kemo_mesh_viewer_glui.h \
	view_modifier_glut.o \
	kemoview_fileselector_glui.o \
	kemoview_glui_window_input.o \
	const_viewer_menu_glut.o \
	kemoview_glut_routines.o \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c '$$(CXXFLAGS) $$(C_INCLUDE) $$(GLUI_INC)' \
	'$$(KEMOVIEW_GLUIDIR)'/kemo_mesh_viewer_glui.cpp \
	>> $(MAKENAME)

includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMOVIEW_GLUIDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMOVIEW_GLUIDIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o *~ *.mod *.diag *.L
