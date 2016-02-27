!>@file   m_group_data.f90
!!@brief  module m_group_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2011
!
!>@brief group data from structure to 1st mesh modules
!!
!!@verbatim
!!      subroutine group_data_from_type(group)
!!        type(mesh_groups), intent(inout) :: group
!!
!!      subroutine dealloc_grp_connectivity_1st
!!      subroutine deasf_grp_nod1llocate_surf_grp_geometry
!!
!!      subroutine deallocate_surf_infinity
!!@endverbatim
!
      module m_group_data
!
      use m_precision
      use t_group_data
      use t_group_connects
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_surface_boundary
!
      implicit  none
!
!
!>   Structure of connectivity data for surface group items
      type(surface_node_grp_data), save :: sf_grp_nod1
!>   Structure of geometry data for surface group
      type(surface_group_geometry), save :: sf_grp_v1
!
!>   Structure of connectivities for element group
      type(element_group_table), save :: ele_grp_tbl1
!>   Structure of connectivities for surface group
      type(surface_group_table), save :: sf_grp_tbl1
!
!
!>      Structure for scalar's boundary condition on surface
      type(scalar_surf_BC_list), save :: infty_list
!
      end module m_group_data
