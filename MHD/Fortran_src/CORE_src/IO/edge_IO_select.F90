!edge_IO_select.F90
!      module edge_IO_select
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine sel_output_edge_connect(my_rank)
!      subroutine sel_input_edge_connect(my_rank)
!
!      subroutine sel_output_edge_geometries(my_rank)
!      subroutine sel_input_edge_geometries(my_rank)
!
!      subroutine sel_output_edge_geometries_sph(my_rank)
!      subroutine sel_output_edge_geometries_cyl(my_rank)
!
!
      module edge_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use edge_file_IO
      use gz_edge_file_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_edge_connect(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_edge_connect_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_edge_connect_b
      else
        call output_edge_connect
      end if
!
      end subroutine sel_output_edge_connect
!
!------------------------------------------------------------------
!
      subroutine sel_input_edge_connect(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call input_edge_connect_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call input_edge_connect_b
      else
        call input_edge_connect
      end if
!
      end subroutine sel_input_edge_connect
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_edge_geometries(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_edge_geometries_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_edge_geometries_b
      else
        call output_edge_geometries
      end if
!
      end subroutine sel_output_edge_geometries
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_input_edge_geometries(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call input_edge_geometries_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call input_edge_geometries_b
      else
        call input_edge_geometries
      end if
!
      end subroutine sel_input_edge_geometries
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_edge_geometries_sph(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_edge_geometries_sph_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_edge_geometries_sph_b
      else
        call output_edge_geometries_sph
      end if
!
      end subroutine sel_output_edge_geometries_sph
!
!------------------------------------------------------------------
!
      subroutine sel_output_edge_geometries_cyl(my_rank)
!
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_edge_geometries_cyl_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_edge_geometries_cyl_b
      else
        call output_edge_geometries_cyl
      end if
!
      end subroutine sel_output_edge_geometries_cyl
!
!------------------------------------------------------------------
!
      end module edge_IO_select
