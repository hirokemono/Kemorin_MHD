#
#
#

KEMO_GTK_DIR = $$(C_SRCDIR)/GTK
CURRENTDIR = $(shell pwd)
SRC_GTK_C = $(shell ls *.c)
INC_GTK_C = $(addsuffix .h,$(basename $(SRC_GTK_C)) )
OBJ_GTK_C = $(addsuffix .o,$(basename $(SRC_GTK_C)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_GTK_DIR =  $(KEMO_GTK_DIR)'      >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_GTK_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_GTK_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_GTK_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


depends:
	@echo 'Construct compile list in C_src/GTK'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_GTK_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(GTK3_CFLAGS) $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp >> $(MAKENAME).depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
