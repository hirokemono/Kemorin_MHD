!>@file  edge_file_IO.f90
!!      module edge_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for edge data
!!
!!@verbatim
!!      subroutine set_edge_fname(my_rank)
!!      subroutine output_edge_geometries
!!      subroutine output_edge_geometries_sph
!!      subroutine output_edge_geometries_cyl
!!@endverbatim
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
      private :: set_edge_fname
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_edge_fname(my_rank)
!
      use m_file_format_switch
      use set_mesh_file_names
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call set_mesh_file_name(mesh_edge_file_head, id_ascii_file_fmt,   &
     &    my_rank, mesh_file_name)
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
