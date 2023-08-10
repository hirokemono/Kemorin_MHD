#
#
#

FFTW_C_DIR =     $$(C_SRCDIR)/FFTW_wrapper

SOURCES = $(shell ls *.c)
INCLUDES = $(addsuffix .h,$(basename $(SOURCES)) )

OBJ_FFTW_C = $(addsuffix .o,$(basename $(SOURCES)) )


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FFTW_C_DIR =  $(FFTW_C_DIR)'      >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_FFTW_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_FFTW_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_FFTW_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@echo 'Construct dependencies in C_src/FFTW_wrapper'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SOURCES) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS) $(C_INCLUDE) $(MHDDIR)/C_src/FFTW_wrapper/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp > $(MHDDIR)/C_src/FFTW_wrapper/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
