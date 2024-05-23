#
#
#
#
#

FFMPEG_TEST_DIR =       $$(TESTS_DIR)/FFT_test

TARGET_FFMPEG_TEST =   png2mp4

CURRENTDIR = $(shell pwd)
C_SRCDIR= $(CURRENTDIR)/../../../../MHD/C_src


SOURCE = \
png2mp4_test.c

MORE_SOURCES = \
$(C_SRCDIR)/CORE_C/read_image_2_png.c \
kemoview_FFMPEG_encoder.c

#
#  ------------------------------------------------------------------
#

all:$(TARGET_FFMPEG_TEST)

$(TARGET_FFMPEG_TEST): $(SOURCE) $(MORE_SOURCES) $(C_SRCDIR)/CORE_C/read_image_2_png.h
	$(CC) -o $(TARGET_FFMPEG_TEST) -I. -I$(C_SRCDIR)/CORE_C \
	`pkg-config --cflags --libs libpng libavcodec libavformat libswscale` \
	$(MORE_SOURCES) $<

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a $(TARGET_FFMPEG_TEST)
