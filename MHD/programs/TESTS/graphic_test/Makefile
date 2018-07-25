#
#
#
TARGET_BMP_TEST =      test_BMP_files
TARGET_FGL_EPS_TEST =  test_EPS_GL_F
TARGET_FGL_PNG_TEST =  test_PNG_GL_F
TARGET_CGL_EPS_TEST =  test_EPS_GL_C
TARGET_CGL_PNG_TEST =  test_PNG_GL_C
TARGET_C_PNG_TEST =    test_PNG_C
TARGET_CGL_FONT_TEST = test_FONT_GL_C

TARGET_CPSF_READ_TEST = test_read_psf_c
TARGET_CVIEW_READ_TEST = test_read_viewer_c
TARGET_CBMP_READ_TEST = test_read_bmp
TARGET_CPNG_READ_TEST = test_read_png
TARGET_PNG_2_BMP_F =    test_png_2_bmp_f


MOD_BMP_TEST = \
write_bmp_image.o \
test_bmp_images.o

MOD_EPS_TEST = \
rendereps.o \
epstest.o

MOD_FGL_PNG_TEST = \
test_gl_2_png_f.o \
gl_buffer_2_png_f.o \
gl_buffer_2_png.o \
write_image_2_png.o

MOD_CGL_PNG_TEST = \
test_gl_2_png_c.o \
gl_buffer_2_png.o \
write_image_2_png.o

MOD_C_PNG_TEST = \
test_wt_png_c.o \
write_image_2_png.o

MOD_CGL_EPS_TEST = \
test_rendereps_c.o \
rendereps_c.o

MOD_CGL_FONT_TEST = \
glut_fontsmpl.o \
ysglusefontbitmap.o \
ysglfontdata.o

OBJ_CPSF_READ_TEST = \
test_read_psf_data.o

OBJ_CVIEW_READ_TEST = \
test_read_viewermesh.o

OBJ_CPNG_READ_TEST = \
test_read_png_c.o

OBJ_CBMP_READ_TEST = \
test_read_bmp_c.o

OBJ_PNG_2_BMP_F = \
write_bmp_image.o \
png_to_bmp_f.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GRAPHIC_TEST_DIR =   $$(TESTS_DIR)/graphic_test'      >> $(MAKENAME)

target_list:
	@echo 'TARGET_BMP_TEST =      $$(BUILDDIR)/$(TARGET_BMP_TEST)'     \
	>> $(MAKENAME)
	@echo 'TARGET_CGL_EPS_TEST =  $$(BUILDDIR)/$(TARGET_CGL_EPS_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CGL_PNG_TEST =  $$(BUILDDIR)/$(TARGET_CGL_PNG_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_C_PNG_TEST =    $$(BUILDDIR)/$(TARGET_C_PNG_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CGL_FONT_TEST = $$(BUILDDIR)/$(TARGET_CGL_FONT_TEST)' \
	>> $(MAKENAME)
#	@echo 'TARGET_FGL_EPS_TEST =  $$(BUILDDIR)/$(TARGET_FGL_EPS_TEST)' \
#	>> $(MAKENAME)
#	@echo 'TARGET_FGL_PNG_TEST =  $$(BUILDDIR)/$(TARGET_FGL_PNG_TEST)' \
#	>> $(MAKENAME)
	@echo 'TARGET_CPSF_READ_TEST = $$(BUILDDIR)/$(TARGET_CPSF_READ_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CVIEW_READ_TEST = $$(BUILDDIR)/$(TARGET_CVIEW_READ_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CBMP_READ_TEST = $$(BUILDDIR)/$(TARGET_CBMP_READ_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CPNG_READ_TEST = $$(BUILDDIR)/$(TARGET_CPNG_READ_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_PNG_2_BMP_F = $$(BUILDDIR)/$(TARGET_PNG_2_BMP_F)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo f_graphic_tests: \
	'$$(TARGET_BMP_TEST) $$(TARGET_PNG_2_BMP_F)'  \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo c_graphic_tests: \
	'$$(TARGET_C_PNG_TEST)  $$(TARGET_CPSF_READ_TEST)' \
	'$$(TARGET_CPNG_READ_TEST)  $$(TARGET_CBMP_READ_TEST)' \
	'$$(TARGET_CVIEW_READ_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo gl_tests: \
	'$$(TARGET_CGL_PNG_TEST) $$(TARGET_CGL_FONT_TEST) '\
	'$$(TARGET_CGL_EPS_TEST)   '\
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
#	@echo f90gl_tests: \
#	'$$(TARGET_FGL_EPS_TEST) $$(TARGET_FGL_PNG_TEST)   '\
#	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_BMP_TEST)': '$$(MOD_BMP_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_BMP_TEST)' \
	'$$(MOD_BMP_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '$$(TARGET_CGL_PNG_TEST)': '$$(MOD_CGL_PNG_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CGL_PNG_TEST)' \
	'$$(MOD_CGL_PNG_TEST)' \
	'$$(OPENGL_LIBS)' '$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_C_PNG_TEST)': '$$(MOD_C_PNG_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_C_PNG_TEST)' \
	'$$(MOD_C_PNG_TEST)' \
	'$$(PNG_LIBS) $$(ZLIB_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CGL_EPS_TEST)': '$$(MOD_CGL_EPS_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CGL_EPS_TEST)' \
	'$$(MOD_CGL_EPS_TEST)' \
	'$$(OPENGL_LIBS)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CGL_FONT_TEST)': '$$(MOD_CGL_FONT_TEST)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CGL_FONT_TEST)' \
	'$$(MOD_CGL_FONT_TEST)' \
	'$$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CPSF_READ_TEST)': '$$(OBJ_CPSF_READ_TEST) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CPSF_READ_TEST)' \
	'$$(OBJ_CPSF_READ_TEST)' \
	'-L. $$(LIB_KEMO_C) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CVIEW_READ_TEST)': '$$(OBJ_CVIEW_READ_TEST) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CVIEW_READ_TEST)' \
	'$$(OBJ_CVIEW_READ_TEST)' \
	'-L. $$(LIB_KEMO_C) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CBMP_READ_TEST)': '$$(OBJ_CBMP_READ_TEST) $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CBMP_READ_TEST)' \
	'$$(OBJ_CBMP_READ_TEST)' \
	'-L. $$(LIB_KEMO_C)  $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CPNG_READ_TEST)': '$$(OBJ_CPNG_READ_TEST)  $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS)' -o '$$(TARGET_CPNG_READ_TEST)' \
	'$$(OBJ_CPNG_READ_TEST)' \
	'-L. $$(LIB_KEMO_C)  $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PNG_2_BMP_F)': '$$(OBJ_PNG_2_BMP_F)  $$(LIB_KEMO_C_FILE)' \
	 >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PNG_2_BMP_F)' \
	'$$(OBJ_PNG_2_BMP_F)' \
	'-L. $$(LIB_KEMO_C)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_BMP_TEST=      \\    >> $(MAKENAME)
	@echo  $(MOD_BMP_TEST)          >> $(MAKENAME)
	@echo  MOD_CGL_PNG_TEST=  \\    >> $(MAKENAME)
	@echo  $(MOD_CGL_PNG_TEST)      >> $(MAKENAME)
	@echo  MOD_C_PNG_TEST=    \\    >> $(MAKENAME)
	@echo  $(MOD_C_PNG_TEST)        >> $(MAKENAME)
	@echo  MOD_CGL_EPS_TEST=  \\    >> $(MAKENAME)
	@echo  $(MOD_CGL_EPS_TEST)      >> $(MAKENAME)
	@echo  MOD_CGL_FONT_TEST=  \\   >> $(MAKENAME)
	@echo  $(MOD_CGL_FONT_TEST)     >> $(MAKENAME)
	@echo  OBJ_CPSF_READ_TEST=  \\  >> $(MAKENAME)
	@echo  $(OBJ_CPSF_READ_TEST)    >> $(MAKENAME)
	@echo  OBJ_CVIEW_READ_TEST=  \\ >> $(MAKENAME)
	@echo  $(OBJ_CVIEW_READ_TEST)   >> $(MAKENAME)
	@echo  OBJ_CBMP_READ_TEST=  \\  >> $(MAKENAME)
	@echo  $(OBJ_CBMP_READ_TEST)    >> $(MAKENAME)
	@echo  OBJ_CPNG_READ_TEST=  \\  >> $(MAKENAME)
	@echo  $(OBJ_CPNG_READ_TEST)    >> $(MAKENAME)
	@echo  OBJ_PNG_2_BMP_F=  \\     >> $(MAKENAME)
	@echo  $(OBJ_PNG_2_BMP_F)       >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	test_bmp_images.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_bmp_images.f90 \
	write_bmp_image.o \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -c '$$(F90FLAGS)' \
	'$$(GRAPHIC_TEST_DIR)'/test_bmp_images.f90 \
	>> $(MAKENAME)
	
	@echo \
	png_to_bmp_f.o: \
	'$$(GRAPHIC_TEST_DIR)'/png_to_bmp_f.F90 \
	write_bmp_image.o \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -c '$$(F90FLAGS)' \
	'$$(GRAPHIC_TEST_DIR)'/png_to_bmp_f.F90 \
	>> $(MAKENAME)
	
	@echo \
	epstest.o: \
	'$$(GRAPHIC_TEST_DIR)'/epstest.f90 \
	rendereps.o \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -c '$$(F90FLAGS)' \
	'$$(GRAPHIC_TEST_DIR)'/epstest.f90 \
	>> $(MAKENAME)
	
	@echo \
	test_gl_2_png_f.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_gl_2_png_f.f90 \
	gl_buffer_2_png_f.o \
	gl_buffer_2_png.o \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -c '$$(F90FLAGS)' \
	'$$(GRAPHIC_TEST_DIR)'/test_gl_2_png_f.f90 \
	>> $(MAKENAME)
	
	@echo \
	test_gl_2_png_c.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_gl_2_png_c.c \
	gl_buffer_2_png.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/test_gl_2_png_c.c \
	>> $(MAKENAME)
	
	@echo \
	test_wt_png_c.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_wt_png_c.c \
	write_image_2_png.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/test_wt_png_c.c \
	>> $(MAKENAME)
	
	@echo \
	test_rendereps_c.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_rendereps_c.c \
	rendereps_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/test_rendereps_c.c \
	>> $(MAKENAME)
	
	@echo \
	test_read_png_c.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_read_png_c.c \
	read_image_2_png.o \
	write_image_2_png.o \
	write_image_2_bmp.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/test_read_png_c.c \
	>> $(MAKENAME)
	
	@echo \
	test_read_bmp_c.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_read_bmp_c.c \
	read_image_2_bmp.o \
	write_image_2_png.o \
	write_image_2_bmp.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/test_read_bmp_c.c \
	>> $(MAKENAME)
	
	@echo \
	glut_fontsmpl.o: \
	'$$(GRAPHIC_TEST_DIR)'/glut_fontsmpl.c \
	ysglusefontbitmap.o \
	ysglfontdata.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/glut_fontsmpl.c \
	>> $(MAKENAME)
	
	@echo \
	test_read_psf_data.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_read_psf_data.c \
	'$$(KEMO_C_DIR)'/m_psf_data_4_viewer_c.h \
	'$$(KEMO_C_DIR)'/read_psf_data_viewer_c.h \
	'$$(KEMO_C_DIR)'/take_normal_psf_c.h \
	m_psf_data_4_viewer_c.o \
	read_psf_data_viewer_c.o \
	take_normal_psf_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/test_read_psf_data.c \
	>> $(MAKENAME)
	
	@echo \
	test_read_viewermesh.o: \
	'$$(GRAPHIC_TEST_DIR)'/test_read_viewermesh.c \
	'$$(KEMO_C_DIR)'/m_surface_mesh_4_viewer_c.h \
	'$$(KEMO_C_DIR)'/read_viewer_mesh_c.h \
	'$$(KEMO_C_DIR)'/check_viewer_mesh_c.h \
	m_surface_mesh_4_viewer_c.o \
	read_viewer_mesh_c.o \
	check_viewer_mesh_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE)' \
	'$$(GRAPHIC_TEST_DIR)'/test_read_viewermesh.c \
	>> $(MAKENAME)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
