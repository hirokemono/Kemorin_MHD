!read_psf_select_4_zlib.F90
!      module read_psf_select_4_zlib
!
!      Written by H. Matsui
!
!      subroutine sel_read_alloc_psf_file(itype_psf, istep)
!
      module read_psf_select_4_zlib
!
      use m_precision
      use m_field_file_format
      use set_ucd_file_names
!
      implicit none
!
      private :: sel_read_alloc_psf_udt_file
      private :: sel_read_alloc_psf_ucd_file
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_psf_file(itype_psf, istep)
!
      integer(kind = kint), intent(in) :: itype_psf, istep
!
!
      if(   mod(itype_psf,100)/10 .eq. iflag_udt/10) then
        call sel_read_alloc_psf_udt_file(itype_psf, istep)
      else
        call sel_read_alloc_psf_ucd_file(itype_psf, istep)
      end if
!
      end subroutine sel_read_alloc_psf_file
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_psf_udt_file(itype_psf, istep)
!
      use read_psf_result
      use gz_read_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf, istep
!
      character(len=kchara) :: psf_name, grd_name
!
!
      call set_single_grd_file_name(psf_file_header, itype_psf,         &
     &    grd_name)
      call set_single_ucd_file_name(psf_file_header, itype_psf, istep,  &
     &    psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call read_alloc_psf_grd_gz(grd_name)
        call read_allocate_psf_udt_gz(psf_name)
        return
      end if
#endif
!
      call read_psf_grd(grd_name)
      call read_allocate_psf_udt(psf_name)
!
      end subroutine sel_read_alloc_psf_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_psf_ucd_file(itype_psf, istep)
!
      use read_psf_result
      use gz_read_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf, istep
!
      character(len=kchara) :: psf_name
!
!
      call set_single_ucd_file_name(psf_file_header, itype_psf, istep,  &
     &    psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call read_allocate_psf_ucd_gz(psf_name)
        return
      end if
#endif
!
      call read_allocate_psf_ucd(psf_name)
!
      end subroutine sel_read_alloc_psf_ucd_file
!
!------------------------------------------------------------------
!
      end module read_psf_select_4_zlib
