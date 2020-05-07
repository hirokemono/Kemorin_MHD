mpicc -O4 -Wall -g -fopenmp  -D_REENTRANT \
-o control_GTK \
-I. -I/Users/matsui/src_kemo/MHD/C_src/CORE_C -I/Users/matsui/src_kemo/MHD/C_src/CONTROLS -I/Users/matsui/src_kemo/MHD/C_src/KEMO_GL -I/Users/matsui/src_kemo/MHD/C_src/GLSL -I/Users/matsui/src_kemo/MHD/C_src/GTK -I/Users/matsui/src_kemo/MHD/C_src/KEMO_GLUT \
-I/usr/local/include -I/opt/local/include/gtk-3.0 -I/opt/local/include/gio-unix-2.0/ -I/opt/local/include/cairo -I/opt/local/include -I/opt/local/include/pango-1.0 -I/opt/local/include/harfbuzz -I/opt/local/include -I/opt/local/include/pango-1.0 -I/opt/local/include/fribidi -I/opt/local/include/atk-1.0 -I/opt/local/include/cairo -I/opt/local/include/pixman-1 -I/opt/local/include -I/opt/local/include/ossp -I/opt/local/include/freetype2 -I/opt/local/include/libpng16 -I/opt/local/include -I/opt/local/include/gdk-pixbuf-2.0 -I/opt/local/include/glib-2.0 -I/opt/local/lib/glib-2.0/include -I/opt/local/include  \
/Users/matsui/git/Kemorin_MHD/MHD/programs/TESTS/GTK3_test/file_dialog_8.c \
 -L/Users/matsui/src_kemo/work -lkemo_c \
 -L/opt/local/lib -lgtk-3 -lgdk-3 -Wl,-framework,Cocoa -lpangocairo-1.0 -lpango-1.0 -latk-1.0 -lcairo-gobject -lcairo -lgdk_pixbuf-2.0 -lgio-2.0 -lgobject-2.0 -lglib-2.0 -lintl -Wl,-framework -Wl,CoreFoundation -L/usr/X11/lib -lX11 -lm \
 -framework OpenGL  -framework CoreVideo  -framework IOKit -framework Cocoa -L/opt/local/lib -lpng16  -L/opt/local/lib -lz


 $(CC) $(OPTFLAGS_GL) $(GTK3_CFLAGS) -o $(TARGET_KEMOVIEW_GTK) $(MOD_KEMOVIEW_GTK) -L. $(LIB_KEMO_C) $(GTK3_LIBS) $(GLFW_LIBS) $(OPENGL_LIBS)