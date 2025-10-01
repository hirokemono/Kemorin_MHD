#
#
#

KEMO_C_GLUTDIR = $$(C_SRCDIR)/KEMO_GLUT
CURRENTDIR = $(shell pwd)
SRC_GLUT_C = $(shell ls kemoview_gtk*.c tree_view_*.c)
INC_GLUT_C = $(addsuffix .h,$(basename $(SRC_GLUT_C)) )
OBJ_GLUT_C = $(addsuffix .o,$(basename $(SRC_GLUT_C)) )

GLUT_INCLUDES= \
  $$(C_INCLUDE)   \
  $$(GTK3_CFLAGS) \
  $$(GLFW_CFLAGS)

SRC_GLUT_C+= \
  view_modifier_glfw.c  \
  render_on_GLFW.c      \
  movie_from_GLFW_by_FFMPEG.c

ifdef FFMPEG_CFLAGS
  GLUT_INCLUDES+=  $$(FFMPEG_CFLAGS)
  SRC_GLUT_C+= \
    kemoview_FFMPEG_encoder.c
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


depends:
	@echo 'Construct compile list in C_src/KEMO_GLUT'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_GLUT_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	$$(CC) -c  $$(OPTFLAGS_GL) $(GLUT_INCLUDES)  $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp >> $(MAKENAME).depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
