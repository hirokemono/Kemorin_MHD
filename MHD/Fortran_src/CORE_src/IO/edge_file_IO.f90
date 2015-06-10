!edge_file_IO.f90
!      module edge_file_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine set_edge_fname(my_rank)
!
!      subroutine output_edge_geometries
!      subroutine output_edge_geometries_sph
!      subroutine output_edge_geometries_cyl
!
!
      module edge_file_IO
!
      use m_precision
!
      use m_read_mesh_data
      use set_parallel_file_name
      use edge_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_edge_fname(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      if(iflag_mesh_file_ext .gt. 0) then
        call add_int_suffix(my_rank, mesh_edge_file_head, fname_tmp)
        call add_gfm_extension(fname_tmp, mesh_file_name)
      else
        call add_int_suffix(my_rank, mesh_edge_file_head,               &
     &      mesh_file_name)
      end if
!
      end subroutine set_edge_fname
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_edge_geometries
!
!
      open (input_file_code, file = mesh_file_name, form = 'formatted')
      call write_edge_connection
      call write_edge_geometry
      close (input_file_code)
!
      end subroutine output_edge_geometries
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_edge_geometries_sph
!
!
      open (input_file_code, file = mesh_file_name, form = 'formatted')
      call write_edge_connection
      call write_edge_geometry_sph
      close (input_file_code)
!
      end subroutine output_edge_geometries_sph
!
!------------------------------------------------------------------
!
      subroutine output_edge_geometries_cyl
!
!
      open (input_file_code, file = mesh_file_name, form = 'formatted')
      call write_edge_connection
      call write_edge_geometry_cyl
      close (input_file_code)
!
      end subroutine output_edge_geometries_cyl
!
!------------------------------------------------------------------
!
      end module edge_file_IO
