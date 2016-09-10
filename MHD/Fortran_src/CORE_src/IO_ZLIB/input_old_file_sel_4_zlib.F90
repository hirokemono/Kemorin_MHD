!> @file  input_old_file_sel_4_zlib.f90
!!      module input_old_file_sel_4_zlib
!!
!! @author  H. Matsui
!! @date Programmed in July, 2006
!!@n     Modified in May, 2009
!!@n     Modified in May, 2015
!
!> @brief Selector for old restart data reading
!!
!!@verbatim
!!      subroutine sel_read_alloc_field_file(my_rank, istep_fld, fld_IO)
!!
!!      subroutine sel_read_rst_file(my_rank, istep_fld, fld_IO)
!!      subroutine sel_read_rst_comps(my_rank, istep_fld, fld_IO)
!!@endverbatim
!
      module input_old_file_sel_4_zlib
!
      use m_precision
!
      use m_file_format_switch
      use t_field_data_IO
!
#ifdef ZLIB_IO
      use gz_rst_data_IO_by_fld
      use gz_field_file_IO
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
      subroutine sel_read_alloc_field_file(my_rank, istep_fld, fld_IO)
!
      use field_file_IO
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
#ifdef ZLIB_IO
      if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_field_file(file_name, my_rank, fld_IO)
        return
      end if
#endif
!
      call read_and_allocate_field_file(file_name, my_rank, fld_IO)
!
      end subroutine sel_read_alloc_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_rst_file(my_rank, istep_fld, fld_IO)
!
      use rst_data_IO_by_fld
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
      character(len=kchara) :: file_name, fname_tmp
!
!
      if(istep_fld .lt. 0) then
        call add_elaps_postfix(fld_IO%file_prefix, fname_tmp)
      else
        call add_int_suffix(istep_fld, fld_IO%file_prefix, fname_tmp)
      end if
      call add_int_suffix(my_rank, fname_tmp, file_name)
!
!
#ifdef ZLIB_IO
      if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_gz_rst_file(my_rank, file_name, fld_IO)
        return
      end if
#endif
!
      call read_rst_file(my_rank, file_name, fld_IO)
!
!
      end subroutine sel_read_rst_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_rst_comps(my_rank, istep_fld, fld_IO)
!
      use rst_data_IO_by_fld
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name, fname_tmp
!
!
      if(istep_fld .lt. 0) then
        call add_elaps_postfix(fld_IO%file_prefix, fname_tmp)
      else
        call add_int_suffix(istep_fld, fld_IO%file_prefix, fname_tmp)
      end if
      call add_int_suffix(my_rank, fname_tmp, file_name)
!
#ifdef ZLIB_IO
      if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_gz_rst_comps(my_rank, file_name, fld_IO)
        return
      end if
#endif
!
      call read_rst_data_comps(my_rank, file_name, fld_IO)
!
      end subroutine sel_read_rst_comps
!
!------------------------------------------------------------------
!
      end module input_old_file_sel_4_zlib
