!
!      module m_grp_data_cub_kemo
!
!      written by Kemorin
!
!!      subroutine count_node_group                                     &
!!     &         (c_size, elm_type, nx, ny, ipe, jpe, kpe)
!!      subroutine count_surface_group(c_size, nx, ny, kpe)
!!        type(size_of_cube), intent(in) :: c_size
!
      module m_grp_data_cub_kemo
!
      use m_precision
      use t_group_data
      use t_size_of_cube
!
      implicit none
!
!>     Structure for node group
        type (group_data), save ::         cube_nod_grp
!>     Structure for element group
        type (group_data), save ::         cube_ele_grp
!>     Structure for surface group
        type (surface_group_data), save :: cube_surf_grp
!
!
      end module m_grp_data_cub_kemo
