!surface_file_IO.f90
!      module surface_file_IO
!
!     Written by H. Matsui on Aug., 2006
!
!>@file  surface_file_IO.F90
!!       module surface_file_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief surface mesh file IO
!!
!!@verbatim
!!      subroutine output_surface_file
!!      subroutine output_surface_sph_file
!!      subroutine output_surface_cyl_file
!!@endverbatim
!!
!!@param my_rank  process ID
!
      module surface_file_IO
!
      use m_precision
!
      use m_read_mesh_data
      use surface_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine output_surface_file
!
!
      open (input_file_code, file = mesh_file_name, form = 'formatted')
      call write_surface_connection
      call write_surface_geometry
      close (input_file_code)
!
      end subroutine output_surface_file
!
!------------------------------------------------------------------
!
      subroutine output_surface_sph_file
!
!
      open (input_file_code, file = mesh_file_name, form = 'formatted')
      call write_surface_connection
      call write_surface_geometry_sph
      close (input_file_code)
!
      end subroutine output_surface_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_surface_cyl_file
!
!
      open (input_file_code, file = mesh_file_name, form = 'formatted')
      call write_surface_connection
      call write_surface_geometry_cyl
      close (input_file_code)
!
      end subroutine output_surface_cyl_file
!
!------------------------------------------------------------------
!
      end module surface_file_IO
