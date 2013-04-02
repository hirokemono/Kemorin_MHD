#
#
#

GLUI_DIR =     $$(EXT_DIR)/glui-2.36_custom

LIB_GLUI =        -lglui
LIB_GLUI_FILE =  libglui.a

GLUI_OBJS = \
glui_string.o \
glui_add_controls.o \
glui.o \
glui_bitmap_img_data.o \
glui_bitmaps.o \
glui_button.o \
glui_edittext.o \
glui_node.o \
glui_radio.o \
glui_statictext.o \
glui_panel.o \
glui_control.o \
glui_column.o \
glui_tree.o \
glui_rollout.o \
glui_window.o \
glui_textbox.o \
glui_scrollbar.o \
glui_listbox.o \
glui_list.o \
glui_filebrowser.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GLUI_DIR = $(GLUI_DIR)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'GLUI_SRCDIR = $$(GLUI_DIR)/src'              >> $(MAKENAME)
	@echo 'GLUI_INCDIR = $$(GLUI_SRCDIR)/include'       >> $(MAKENAME)
	@echo 'GLUI_INC = -I$$(GLUI_INCDIR) $$(OPENGL_INC)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_GLUI =      $(LIB_GLUI)'       >> $(MAKENAME)
	@echo  'LIB_GLUI_FILE = $(LIB_GLUI_FILE)'  >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve

libtarget:
	@echo  '$$(LIB_GLUI_FILE): $$(GLUI_OBJS)' >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(GLUI_OBJS)' >> $(MAKENAME)
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)


mod_list:
	@echo  GLUI_OBJS=  \\ >> $(MAKENAME)
	@echo  $(GLUI_OBJS) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	glui_button.o: \
	'$$(GLUI_SRCDIR)'/glui_button.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_column.o: \
	'$$(GLUI_SRCDIR)'/glui_column.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_control.o: \
	'$$(GLUI_SRCDIR)'/glui_control.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_edittext.o: \
	'$$(GLUI_SRCDIR)'/glui_edittext.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_listbox.o: \
	'$$(GLUI_SRCDIR)'/glui_listbox.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_node.o: \
	'$$(GLUI_SRCDIR)'/glui_node.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_panel.o: \
	'$$(GLUI_SRCDIR)'/glui_panel.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_radio.o: \
	'$$(GLUI_SRCDIR)'/glui_radio.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_rollout.o: \
	'$$(GLUI_SRCDIR)'/glui_rollout.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_window.o: \
	'$$(GLUI_SRCDIR)'/glui_window.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_bitmaps.o: \
	'$$(GLUI_SRCDIR)'/glui_bitmaps.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_statictext.o: \
	'$$(GLUI_SRCDIR)'/glui_statictext.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui.o: \
	'$$(GLUI_SRCDIR)'/glui.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_add_controls.o: \
	'$$(GLUI_SRCDIR)'/glui_add_controls.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_list.o: \
	'$$(GLUI_SRCDIR)'/glui_list.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_scrollbar.o: \
	'$$(GLUI_SRCDIR)'/glui_scrollbar.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_string.o: \
	'$$(GLUI_SRCDIR)'/glui_string.cpp \
	GL/glui.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_textbox.o: \
	'$$(GLUI_SRCDIR)'/glui_textbox.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_tree.o: \
	'$$(GLUI_SRCDIR)'/glui_tree.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_filebrowser.o: \
	'$$(GLUI_SRCDIR)'/glui_filebrowser.cpp \
	GL/glui.h \
	glui_internal.h \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glui_bitmap_img_data.o: \
	'$$(GLUI_SRCDIR)'/glui_bitmap_img_data.cpp \
	>> $(MAKENAME)
	@echo '	''$$(CXX)' -c' $$(CXXFLAGS)  -Wall $$(GLUI_INC) $$<' \
	>> $(MAKENAME)

includes:
	echo 'generate symbolic link commands for GLUI include'
	@echo \
	'GL/glui.h: $$(GLUI_INCDIR)/GL/glui.h'          >> $(MAKENAME)
	@echo '	ln -s $$(GLUI_INCDIR)/GL' >> $(MAKENAME)
	@cd src; \
	for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(GLUI_SRCDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(GLUI_SRCDIR)/'$$sources . \
		 >> $(MAKENAME) \
		) \
	done
