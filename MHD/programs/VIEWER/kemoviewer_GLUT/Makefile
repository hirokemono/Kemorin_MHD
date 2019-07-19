#
#
#

TARGET_KEMOVIEW_C =   'kemoviewer_glut'
TARGET_KEMOVIEW3_C =  'kemoviewer_glut3'
TARGET_KEMOVIEW_F =   'kemoviewer_f'

LIBS_KEMOVIEW = \
'$$(LIB_UTILS)' \
'$$(LIB_MESH)'

LIB_FILES_KEMOVIEW = \
'$$(LIB_UTILS_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_KEMOVIEW+=        '$$(LIB_KEMO_C)'
  LIB_FILES_KEMOVIEW+=   '$$(LIB_KEMO_C_FILE)'
endif

MOD_KEMOVIEW_MAIN = \
kemoviewer_f.o \
\
kemo_mesh_viewer_glut.o \
kemoview_glut_console_input.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

MOD_KEMOVIEW_GLUT = \
kemoviewer_glut.o \
\
kemo_mesh_viewer_glut.o \
kemoview_glut_console_input.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

MOD_KEMOVIEW_GLUT3 = \
kemoviewer_glut3.o \
\
kemo_mesh_viewer_glut.o \
kemoview_glut_console_input.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'KEMOVIEW_GLUTDIR = $$(KEMOVIEWER_DIR)/kemoviewer_GLUT' >> $(MAKENAME)

target_list:
	@echo 'TARGET_KEMOVIEW_C =  $$(BUILDDIR)/$(TARGET_KEMOVIEW_C)'  >> $(MAKENAME)
	@echo 'TARGET_KEMOVIEW3_C = $$(BUILDDIR)/$(TARGET_KEMOVIEW3_C)' >> $(MAKENAME)
	@echo 'TARGET_KEMOVIEW_F =  $$(BUILDDIR)/$(TARGET_KEMOVIEW_F)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo viewer_glut:  '$$(TARGET_KEMOVIEW_C)'   >> $(MAKENAME)
	@echo viewer_glut3: '$$(TARGET_KEMOVIEW3_C)'   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_C)': '$$(MOD_KEMOVIEW_GLUT)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL)' -o '$$(TARGET_KEMOVIEW_C)' \
	'$$(MOD_KEMOVIEW_GLUT) -L. $$(LIB_KEMO_C)' \
	'$$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW3_C)': '$$(MOD_KEMOVIEW_GLUT3)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL)' -o '$$(TARGET_KEMOVIEW3_C)' \
	'$$(MOD_KEMOVIEW_GLUT3) -L. $$(LIB_KEMO_C)' \
	'$$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_F)': '$$(MOD_KEMOVIEW_MAIN)' \
	'$$(LIB_FILES_KEMOVIEW)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_KEMOVIEW_F)' \
	'$$(MOD_KEMOVIEW_MAIN)' '-L. $$(LIBS_KEMOVIEW)' \
	'$$(OPENGL_LIBS)' '$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_KEMOVIEW=  \\          >> $(MAKENAME)
	@echo  $(LIBS_KEMOVIEW)            >> $(MAKENAME)
	@echo  LIB_FILES_KEMOVIEW=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_KEMOVIEW)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_KEMOVIEW_GLUT =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GLUT) >> $(MAKENAME)
	@echo  MOD_KEMOVIEW_GLUT3 =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GLUT3) >> $(MAKENAME)
	@echo  MOD_KEMOVIEW_MAIN =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_MAIN) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	kemoviewer_glut.o:  \
	'$$(KEMOVIEW_GLUTDIR)'/kemoviewer_glut.c  \
	kemo_mesh_viewer_glut.o \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(KEMOVIEW_GLUTDIR)'/kemoviewer_glut.c  \
	>> $(MAKENAME)
	
	@echo \
	kemoview_glut_console_input.o: \
	'$$(KEMOVIEW_GLUTDIR)'/kemoview_glut_console_input.c \
	kemoview_glut_routines.o \
	kemoview_glut_console_input.h \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemo_mesh_viewer_glut.o: \
	'$$(KEMOVIEW_GLUTDIR)'/kemo_mesh_viewer_glut.c \
	kemo_mesh_viewer_glut.h \
	kemoviewer.h \
	view_modifier_glut.o \
	const_viewer_menu_glut.o \
	kemoview_glut_routines.o \
	kemoview_glut_console_input.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoviewer_glut3.o:  \
	'$$(KEMOVIEW_GLUTDIR)'/kemoviewer_glut3.c  \
	kemo_mesh_viewer_glut.o \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(KEMOVIEW_GLUTDIR)'/kemoviewer_glut3.c  \
	>> $(MAKENAME)



includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMOVIEW_GLUTDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMOVIEW_GLUTDIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o *~ *.mod *.diag *.L
