!field_2nd_IO_select.F90
!      module field_2nd_IO_select
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine sel_write_step_sph_spec_file(my_rank, istep_fld)
!      subroutine sel_read_step_SPH2_field_file(my_rank, istep_fld)
!
!      subroutine check_step_sph_file(my_rank, istep_fld, ierr)
!      subroutine sel_read_alloc_step_SPH2_file(my_rank, istep_fld)
!
!      subroutine sel_read_alloc_SPH_fld2_head(my_rank, istep_fld)
!
      module field_2nd_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use m_2nd_field_data_IO
      use set_field_file_names
      use field_2nd_IO
      use field_2nd_IO_b
!
#ifdef ZLIB_IO
      use gz_2nd_field_file_IO
#endif
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_write_step_sph_spec_file(my_rank, istep_fld)
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(field2_file_head,                      &
     &    iflag_field2_data_fmt, my_rank, istep_fld, file_name)
!
      if(iflag_field2_data_fmt .eq. id_binary_file_fmt) then
        call write_step_field2_file_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field2_data_fmt .eq. id_gzip_txt_file_fmt) then
        call write_gz_step_field2_file(file_name, my_rank)
#endif
!
      else
        call write_step_field2_file(file_name, my_rank)
      end if
!
      end subroutine sel_write_step_sph_spec_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_step_SPH2_field_file(my_rank, istep_fld)
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(field2_file_head,                      &
     &    iflag_field2_data_fmt, my_rank, istep_fld, file_name)
!
      if (iflag_field2_data_fmt .eq. id_binary_file_fmt) then
        call read_step_field2_file_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field2_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_gz_step_field2_file(file_name, my_rank)
#endif
!
      else
        call read_step_field2_file(file_name, my_rank)
      end if
!
      end subroutine sel_read_step_SPH2_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_step_sph_file(my_rank, istep_fld, ierr)
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
      integer(kind=kint), intent(inout) :: ierr
!
!
      call set_SPH_fld_file_name(field2_file_head,                      &
     &    iflag_field2_data_fmt, my_rank, istep_fld, file_name)
!
      open (id_fld2_file, file=file_name, status='old', err=99)
      close(id_fld2_file)
!
      ierr = 0
      return
!
  99  continue
      ierr = 1
      return
!
      end subroutine check_step_sph_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_SPH2_file(my_rank, istep_fld)
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(field2_file_head,                      &
     &    iflag_field2_data_fmt, my_rank, istep_fld, file_name)
!
      if (iflag_field2_data_fmt .eq. id_binary_file_fmt) then
        call read_and_alloc_step_field2_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field2_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_and_alloc_gz_step_field2(file_name, my_rank)
#endif
!
      else
        call read_and_alloc_step_field2(file_name, my_rank)
      end if
!
      end subroutine sel_read_alloc_step_SPH2_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_SPH_fld2_head(my_rank, istep_fld)
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(field2_file_head,                      &
     &    iflag_field2_data_fmt, my_rank, istep_fld, file_name)
!
      if (iflag_field2_data_fmt .eq. id_binary_file_fmt) then
        call read_alloc_step_field2_head_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field2_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_step_field2_head(file_name, my_rank)
#endif
!
      else
        call read_alloc_step_field2_head(file_name, my_rank)
      end if
!
      end subroutine sel_read_alloc_SPH_fld2_head
!
!------------------------------------------------------------------
!
      end module field_2nd_IO_select
