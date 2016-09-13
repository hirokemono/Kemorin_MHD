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
      use t_mesh_data
      use t_group_data
!
      implicit  none
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
