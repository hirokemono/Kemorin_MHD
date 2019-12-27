#
#
#

KEMO_C_GLDIR =   $$(C_SRCDIR)/KEMO_GL
SRC_C_GL = $(shell ls *.c)
INC_C_GL = $(addsuffix .h,$(basename $(SRC_C_GL)) )
OBJ_C_GL = $(addsuffix .o,$(basename $(SRC_C_GL)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_GLDIR =   $(KEMO_C_GLDIR)'     >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_C_GL)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_C_GL=  \\ >> $(MAKENAME)
	@echo  $(OBJ_C_GL) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@echo 'Construct dependencies in C_src/KEMO_GL'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_C_GL) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(MHDDIR)/C_src/KEMO_GL/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp > $(MHDDIR)/C_src/KEMO_GL/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
