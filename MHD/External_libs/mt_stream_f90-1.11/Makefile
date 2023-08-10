#
#
#

MT_STREAM_DIR = $$(EXT_DIR)/mt_stream_f90-1.11

LIB_MT_STREAM =       -lmtstream_f-1.11
LIB_MT_STREAM_FILE = libmtstream_f-1.11.a

MTSTREAMFLAG = '-fno-range-check $$(F90OPTFLAGS)'

MOD_MT_STREAM = \
mt_kind_defs.o \
mt_stream.o \
\
gf2xe.o \
f_get_coeff.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo MTSTREAMFLAG = $(MTSTREAMFLAG) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'MT_STREAM_DIR =  $(MT_STREAM_DIR)'   >> $(MAKENAME)

lib_name:
	@echo  'LIB_MT_STREAM =      $(LIB_MT_STREAM)'      >> $(MAKENAME)
	@echo  'LIB_MT_STREAM_FILE = $(LIB_MT_STREAM_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_MT_STREAM_FILE): $$(MOD_MT_STREAM)'>> $(MAKENAME)

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MT_STREAM)' >> $(MAKENAME)

mod_list:
	@echo  MOD_MT_STREAM=  \\ >> $(MAKENAME)
	@echo  $(MOD_MT_STREAM) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:

module:
	@echo 'mt_kind_defs.o: $$(MT_STREAM_DIR)/mt_kind_defs.F90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(MTSTREAMFLAG) $$<' >> $(MAKENAME)
	@echo 'mt_stream.o: $$(MT_STREAM_DIR)/mt_stream.F90 mt_kind_defs.o' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(MTSTREAMFLAG) $$<' >> $(MAKENAME)
	@echo 'gf2xe.o: $$(MT_STREAM_DIR)/f_jump_ahead_coeff/gf2xe.F90 mt_kind_defs.o' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(MTSTREAMFLAG) $$<' >> $(MAKENAME)
	@echo 'f_get_coeff.o: $$(MT_STREAM_DIR)/f_jump_ahead_coeff/f_get_coeff.F90 gf2xe.o mt_kind_defs.o' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(MTSTREAMFLAG) $$<' >> $(MAKENAME)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
