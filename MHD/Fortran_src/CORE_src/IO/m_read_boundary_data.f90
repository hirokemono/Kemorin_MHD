!> @file  m_read_boundary_data.f90
!!      module m_read_boundary_data
!!
!! @author  H. Matsui and H. Okuda
!! @date Written in 2001
!
!> @brief Array for group data IO
!!
!!@verbatim
!!      subroutine set_grp_data_from_IO(nod_grp, ele_grp, surf_grp)
!!      subroutine set_grp_data_to_IO(nod_grp, ele_grp, surf_grp)
!!
!!      subroutine allocate_para_mesh_groups_IO(nprocs)
!!
!!      subroutine deallocate_mesh_groups_IO
!!      subroutine deallocate_para_mesh_groups_IO
!!@endverbatim
!
      module m_read_boundary_data
!
      use m_precision
      use t_group_data
!
      implicit  none
!
!>      Structure for node group IO
      type(group_data), save :: bc_grp_IO
!
!>      Structure for element group IO
      type(group_data), save :: mat_grp_IO
!
!>      Structure for surface group IO
      type(surface_group_data), save :: surf_grp_IO
!
!>      Stack list for number of node group item over domains
      integer(kind = kint), allocatable :: istack_bc_item_IO(:)
!>      Stack list for number of element group item over domains
      integer(kind = kint), allocatable :: istack_mat_item_IO(:)
!>      Stack list for number of surface group item over domains
      integer(kind = kint), allocatable :: istack_surf_item_IO(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_grp_data_from_IO(nod_grp, ele_grp, surf_grp)
!
      use set_group_types_4_IO
!
      type(group_data), intent(inout) :: nod_grp, ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call set_gruop_stracture(bc_grp_IO, nod_grp)
      call set_gruop_stracture(mat_grp_IO, ele_grp)
      call set_surf_grp_stracture(surf_grp_IO, surf_grp)
!
      call deallocate_mesh_groups_IO
!
      end subroutine set_grp_data_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_grp_data_to_IO(nod_grp, ele_grp, surf_grp)
!
      use set_group_types_4_IO
!
      type(group_data), intent(inout) :: nod_grp, ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call set_gruop_stracture(nod_grp, bc_grp_IO)
      call set_gruop_stracture(ele_grp, mat_grp_IO)
      call set_surf_grp_stracture(surf_grp, surf_grp_IO)
!
      end subroutine set_grp_data_to_IO
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_para_mesh_groups_IO(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_bc_item_IO(0:nprocs))
      allocate(istack_mat_item_IO(0:nprocs))
      allocate(istack_surf_item_IO(0:nprocs))
      istack_bc_item_IO =   0
      istack_mat_item_IO =  0
      istack_surf_item_IO = 0
!
      end subroutine allocate_para_mesh_groups_IO
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_mesh_groups_IO
!
!
      call deallocate_grp_type(bc_grp_IO)
      call deallocate_grp_type(mat_grp_IO)
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine deallocate_mesh_groups_IO
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_para_mesh_groups_IO
!
!
      deallocate(istack_bc_item_IO, istack_mat_item_IO)
      deallocate(istack_surf_item_IO)
!
      end subroutine deallocate_para_mesh_groups_IO
!
! ----------------------------------------------------------------------
!
      end module m_read_boundary_data
