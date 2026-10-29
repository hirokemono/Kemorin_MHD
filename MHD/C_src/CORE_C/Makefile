#
#
#

KEMO_C_DIR =     $$(C_SRCDIR)/CORE_C

CURRENTDIR = $(shell pwd)
SRC_KEMO_C = $(shell ls *.c)
INC_KEMO_C = $(addsuffix .h,$(basename $(SRC_KEMO_C)) )
OBJ_KEMO_C = $(addsuffix .o,$(basename $(SRC_KEMO_C)) )

OBJS_LIST= OBJS_C

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_DIR =  $(KEMO_C_DIR)' >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_KEMO_C)' >> $(MAKENAME)


mod_list:
	@echo  '$(OBJS_LIST)+= $$(OBJ_KEMO_C)' >> $(MAKENAME)
	@echo  OBJ_KEMO_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMO_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


depends:
	@echo 'Construct compile list in C_src/CORE_C'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_KEMO_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(CURRENTDIR)/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp >> $(MAKENAME).depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
