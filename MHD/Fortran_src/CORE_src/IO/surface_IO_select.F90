!>@file  surface_IO_select.F90
!!       module surface_IO_select
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief data IO selector for surface mesh
!!
!!@verbatim
!!      subroutine sel_output_surface_file(my_rank)
!!      subroutine sel_output_surface_sph_file(my_rank)
!!      subroutine sel_output_surface_cyl_file(my_rank)
!!@endverbatim
!!
!!@param my_rank  process ID
!
      module surface_IO_select
!
      use m_precision
!
      use m_read_mesh_data
      use m_file_format_switch
      use surface_file_IO
!
      implicit none
!
      private :: set_surface_fname
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_surface_fname(my_rank)
!
      use m_file_format_switch
      use set_mesh_file_names
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_mesh_file_name(mesh_surf_file_head, id_ascii_file_fmt,   &
     &    my_rank, mesh_file_name)
!
      end subroutine set_surface_fname
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
      call output_surface_file
!
      end subroutine sel_output_surface_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_sph_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
      call output_surface_sph_file
!
      end subroutine sel_output_surface_sph_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_cyl_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
      call output_surface_cyl_file
!
      end subroutine sel_output_surface_cyl_file
!
!------------------------------------------------------------------
!
      end module surface_IO_select
