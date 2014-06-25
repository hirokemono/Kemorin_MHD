!input_old_file_sel_4_zlib.F90
!      module input_old_file_sel_4_zlib
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine sel_read_alloc_field_file(my_rank, istep_fld)
!
!      subroutine sel_read_rst_file(my_rank, istep_fld)
!      subroutine sel_read_rst_comps(my_rank, istep_fld)
!
      module input_old_file_sel_4_zlib
!
      use m_precision
!
      use m_file_format_switch
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
      subroutine sel_read_alloc_field_file(my_rank, istep_fld)
!
      use m_field_data_IO
      use field_file_IO
      use field_file_IO_b
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      if (iflag_field_data_fmt .eq. id_binary_file_fmt) then
        call read_and_allocate_field_file_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_field_file(file_name, my_rank)
#endif
!
      else
        call read_and_allocate_field_file(file_name, my_rank)
      end if
!
      end subroutine sel_read_alloc_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_rst_file(my_rank, istep_fld)
!
      use m_field_data_IO
      use rst_data_IO_by_fld
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name, fname_tmp
!
!
      if(istep_fld .lt. 0) then
        call add_elaps_postfix(phys_file_head, fname_tmp)
      else
        call add_int_suffix(istep_fld, phys_file_head, fname_tmp)
      end if
      call add_int_suffix(my_rank, fname_tmp, file_name)
!
!
      if(iflag_field_data_fmt .eq. id_binary_file_fmt) then
        call read_rst_file_b(my_rank, file_name)
!
#ifdef ZLIB_IO
      else if(iflag_field_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_gz_rst_file(my_rank, file_name)
#endif
!
      else
        call read_rst_file(my_rank, file_name)
      end if
!
!
      end subroutine sel_read_rst_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_rst_comps(my_rank, istep_fld)
!
      use m_field_data_IO
      use rst_data_IO_by_fld
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name, fname_tmp
!
!
      if(istep_fld .lt. 0) then
        call add_elaps_postfix(phys_file_head, fname_tmp)
      else
        call add_int_suffix(istep_fld, phys_file_head, fname_tmp)
      end if
      call add_int_suffix(my_rank, fname_tmp, file_name)
!
      if(iflag_field_data_fmt .eq. id_binary_file_fmt) then
        call read_rst_data_comps_b(my_rank, file_name)
!
#ifdef ZLIB_IO
      else if(iflag_field_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_gz_rst_comps(my_rank, file_name)
#endif
!
      else
        call read_rst_data_comps(my_rank, file_name)
      end if
!
      end subroutine sel_read_rst_comps
!
!------------------------------------------------------------------
!
      end module input_old_file_sel_4_zlib
