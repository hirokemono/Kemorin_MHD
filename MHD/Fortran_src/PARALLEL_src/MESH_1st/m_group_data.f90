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
!!      subroutine deallocate_surf_grp_geometry
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
!>  Structure for node and node group
      type(group_data), save :: nod_grp1
!>  Structure for element group
      type(group_data), save :: ele_grp1
!>  Structure for surfacet group
      type(surface_group_data), save :: sf_grp1
!
!
!>   Structure of connectivities for element group
      type(element_group_table), save :: ele_grp_tbl1
!
!>   Structure of connectivities for surface group
      type(surface_group_table), save :: sf_grp_tbl1
!> Structure of connectivity data for surface group items
      type(surface_node_grp_data), save :: sf_grp_nod1
!
!
!>   Structure of geometry data for surface group
      type(surface_group_geometry), save :: sf_grp_v1
!
!
!>      Structure for scalar's boundary condition on surface
      type(scalar_surf_BC_list), save :: infty_list
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine group_data_from_type(group)
!
      use t_mesh_data
      use t_group_data
      use copy_mesh_structures
!
      type(mesh_groups), intent(inout) :: group
!
!
      call copy_group_data(group%nod_grp, nod_grp1)
      call copy_group_data(group%ele_grp, ele_grp1)
      call copy_surface_group(group%surf_grp, sf_grp1)
!
      call dealloc_groups_data(group)
!
      end subroutine group_data_from_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_grp_connectivity_1st
!
!
      call dealloc_grp_connect(sf_grp_tbl1%edge)
      call dealloc_surf_item_sf_grp_type(sf_grp_tbl1)
      call dealloc_num_surf_grp_nod_smp(sf_grp_nod1)
      call dealloc_surf_grp_nod(sf_grp_nod1)
!
      call dealloc_grp_connect(ele_grp_tbl1%surf)
      call dealloc_grp_connect(ele_grp_tbl1%edge)
      call dealloc_grp_connect(ele_grp_tbl1%node)
!
      end subroutine dealloc_grp_connectivity_1st
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_geometry
!
      call dealloc_surf_grp_type_geom(sf_grp_v1)
!
      end subroutine deallocate_surf_grp_geometry
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_infinity
!
      call dealloc_scalar_surf_BC(infty_list)
!
      end subroutine deallocate_surf_infinity
!
!-----------------------------------------------------------------------
!
      end module m_group_data
