!element_IO_select.F90
!      module element_IO_select
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine sel_input_element_comm_table(my_rank)
!      subroutine sel_input_element_geometries(my_rank)
!
!      subroutine sel_output_ele_comm_table(my_rank)
!      subroutine sel_output_element_file(my_rank)
!      subroutine sel_output_element_sph_file(my_rank)
!      subroutine sel_output_element_cyl_file(my_rank)
!
      module element_IO_select
!
      use m_precision
!
      use m_read_mesh_data
      use m_file_format_switch
      use element_file_IO
      use gz_element_file_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine sel_input_element_comm_table(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call input_element_comm_table_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call input_element_comm_table_gz(my_rank)
#endif
!
      else
        call input_element_comm_table(my_rank)
      end if
!
      end subroutine sel_input_element_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_input_element_geometries(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call input_element_geometries_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call input_element_geometries_gz(my_rank)
#endif
!
      else
        call input_element_geometries(my_rank)
      end if
!
      end subroutine sel_input_element_geometries
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_ele_comm_table(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_ele_comm_table_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_ele_comm_table_gz(my_rank)
#endif
!
      else
        call output_ele_comm_table(my_rank)
      end if
!
      end subroutine sel_output_ele_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_element_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_element_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_element_file_gz(my_rank)
#endif
!
      else
        call output_element_file(my_rank)
      end if
!
      end subroutine sel_output_element_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_element_sph_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_element_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_element_sph_file_gz(my_rank)
#endif
!
      else
        call output_element_sph_file(my_rank)
      end if
!
      end subroutine sel_output_element_sph_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_element_cyl_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_element_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_element_cyl_file_gz(my_rank)
#endif
!
      else
        call output_element_cyl_file(my_rank)
      end if
!
      end subroutine sel_output_element_cyl_file
!
!------------------------------------------------------------------
!
      end module element_IO_select
