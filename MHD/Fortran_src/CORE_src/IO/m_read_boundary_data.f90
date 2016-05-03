!> @file  m_read_boundary_data.f90
!!      module m_read_boundary_data
!!
!! @author  H. Matsui and H. Okuda
!! @date Written in 2001
!
!> @brief Array for group data IO
!!
!!@verbatim
!!      subroutine deallocate_boundary_arrays
!!@endverbatim
!
      module m_read_boundary_data
!
      use m_precision
      use t_group_data
!
      implicit  none
!
!   node group
      type(group_data), save :: bc_grp_IO
!
!   element group
      type(group_data), save :: mat_grp_IO
!
!   surface group
      type(surface_group_data), save :: surf_grp_IO
!surf_grp_IO%num_grp
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
      call deallocate_grp_type(bc_grp_IO)
      call deallocate_grp_type(mat_grp_IO)
      call deallocate_sf_grp_type(surf_grp_IO)
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
!
      subroutine deallocate_boundary_arrays
!
!
      call deallocate_grp_type(bc_grp_IO)
      call deallocate_grp_type(mat_grp_IO)
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine deallocate_boundary_arrays
!
! ----------------------------------------------------------------------
!
      end module m_read_boundary_data
