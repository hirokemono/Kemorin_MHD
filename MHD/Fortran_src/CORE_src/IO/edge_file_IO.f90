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
      use m_comm_data_IO
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
      subroutine output_edge_geometries
!
!
      open (input_file_code, file = mesh_file_name, form = 'formatted')
      call write_edge_connection                                        &
     &  (input_file_code, my_rank_IO, comm_IO, nod_IO, ele_IO, sfed_IO)
      call write_edge_geometry(input_file_code, nod_IO, sfed_IO)
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
      call write_edge_connection                                        &
     &  (input_file_code, my_rank_IO, comm_IO, nod_IO, ele_IO, sfed_IO)
      call write_edge_geometry_sph(input_file_code, nod_IO, sfed_IO)
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
      call write_edge_connection                                        &
     &  (input_file_code, my_rank_IO, comm_IO, nod_IO, ele_IO, sfed_IO)
      call write_edge_geometry_cyl(input_file_code, nod_IO, sfed_IO)
      close (input_file_code)
!
      end subroutine output_edge_geometries_cyl
!
!------------------------------------------------------------------
!
      end module edge_file_IO
