!read_psf_select_4_zlib.F90
!      module read_psf_select_4_zlib
!
!      Written by H. Matsui
!
!      subroutine sel_read_psf_grid_file(itype_psf, psf_head)
!      subroutine sel_read_alloc_psf_udt_file                           &
!     &         (itype_psf, psf_head, istep)
!      subroutine sel_read_alloc_psf_ucd_file                           &
!     &          (itype_psf, psf_head, istep)
!      subroutine sel_read_alloc_psf_ucd_file(itype_psf, psf_head, istep)
!      subroutine sel_read_psf_udt_file(itype_psf, psf_head, istep)
!
!      subroutine sel_write_psf_ucd_file(itype_psf, psf_head, istep)
!      subroutine sel_write_psf_grd_file(itype_psf, psf_head)
!      subroutine sel_write_psf_udt_file(itype_psf, psf_head, istep)
!
      module read_psf_select_4_zlib
!
      use m_precision
      use m_field_file_format
      use set_ucd_file_names
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_psf_grid_file(itype_psf, psf_head)
!
      use read_psf_result
      use gz_read_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf
      character(len=kchara), intent(in) :: psf_head
!
      character(len=kchara) :: psf_name
!
!
      call set_single_grd_file_name(psf_head, itype_psf, psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call read_psf_grd_gz(psf_head)
        return
      end if
#endif
!
      call read_psf_grd(psf_head)
!
      end subroutine sel_read_psf_grid_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_psf_udt_file                            &
     &         (itype_psf, psf_head, istep)
!
      use read_psf_result
      use gz_read_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf, istep
      character(len=kchara), intent(in) :: psf_head
!
      character(len=kchara) :: psf_name
!
!
      call set_single_ucd_file_name(psf_head, itype_psf, istep,         &
     &    psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call read_allocate_psf_udt_gz(psf_name)
        return
      end if
#endif
!
      call read_allocate_psf_udt(psf_name)
!
      end subroutine sel_read_alloc_psf_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_psf_ucd_file                            &
     &          (itype_psf, psf_head, istep)
!
      use read_psf_result
      use gz_read_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf, istep
      character(len=kchara), intent(in) :: psf_head
!
      character(len=kchara) :: psf_name
!
!
      call set_single_ucd_file_name(psf_head, itype_psf, istep,         &
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
      subroutine sel_read_psf_udt_file(itype_psf, psf_head, istep)
!
      use read_psf_result
      use gz_read_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf, istep
      character(len=kchara), intent(in) :: psf_head
!
      character(len=kchara) :: psf_name
!
!
      call set_single_ucd_file_name(psf_head, itype_psf, istep,         &
     &    psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call read_psf_data_udt_gz(psf_name)
        return
      end if
#endif
!
      call read_psf_data_udt(psf_name)
!
      end subroutine sel_read_psf_udt_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_psf_ucd_file(itype_psf, psf_head, istep)
!
      use write_psf_result
      use gz_write_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf, istep
      character(len=kchara), intent(in) :: psf_head
!
      character(len=kchara) :: psf_name
!
!
      call set_single_ucd_file_name(psf_head, itype_psf, istep,         &
     &    psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call write_gz_psf_ucd(psf_name)
        return
      end if
#endif
!
      call write_psf_ucd(psf_name)
!
      end subroutine sel_write_psf_ucd_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_psf_grd_file(itype_psf, psf_head)
!
      use write_psf_result
      use gz_write_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf
      character(len=kchara), intent(in) :: psf_head
!
      character(len=kchara) :: psf_name
!
!
      call set_single_grd_file_name(psf_head, itype_psf, psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call write_gz_psf_grd(psf_name)
        return
      end if
#endif
!
      call write_psf_grd(psf_name)
!
      end subroutine sel_write_psf_grd_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_psf_udt_file(itype_psf, psf_head, istep)
!
      use write_psf_result
      use gz_write_psf_result
!
      integer(kind = kint), intent(in) :: itype_psf, istep
      character(len=kchara), intent(in) :: psf_head
!
      character(len=kchara) :: psf_name
!
!
      call set_single_ucd_file_name(psf_head, itype_psf, istep,         &
     &    psf_name)
!
#ifdef ZLIB_IO
      if (   mod(itype_psf,10) .eq. iflag_gzip) then
        call write_gz_psf_udt(psf_name)
        return
      end if
#endif
!
      call write_psf_udt(psf_name)
!
      end subroutine sel_write_psf_udt_file
!
!------------------------------------------------------------------
!
      end module read_psf_select_4_zlib
