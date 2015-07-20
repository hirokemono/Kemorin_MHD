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
!!      subroutine compare_group_type_vs_1st(my_rank, group)
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module m_group_data
!
      use m_precision
!
      use m_node_group
      use m_element_group
      use m_surface_group
!
      implicit  none
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
      subroutine compare_group_type_vs_1st(my_rank, group_ref)
!
      use t_mesh_data
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_groups), intent(in) :: group_ref
!
!
      call compare_nod_grp_type_vs_1st                                  &
     &   (my_rank, group_ref%nod_grp, nod_grp1)
      call compare_nod_grp_type_vs_1st                                  &
     &   (my_rank, group_ref%ele_grp, ele_grp1)
      call compare_surf_grp_type_vs_1st                                 &
     &   (my_rank, group_ref%surf_grp, sf_grp1)
!
      end subroutine compare_group_type_vs_1st
!
!-----------------------------------------------------------------------
!
      end module m_group_data
