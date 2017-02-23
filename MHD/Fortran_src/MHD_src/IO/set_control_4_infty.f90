!>@file   set_control_4_infty.f90
!!@brief  module set_control_4_infty
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set infinite radius boundary from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_infty(surf_bc_INF_ctl)
!!        type(ctl_array_c2r), intent(inout) :: surf_bc_INF_ctl
!!@endverbatim
!
      module set_control_4_infty
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_infty(surf_bc_INF_ctl)
!
      use calypso_mpi
      use t_read_control_arrays
      use const_bc_infinity_surf
      use set_surface_group_types
!
      type(ctl_array_c2r), intent(inout) :: surf_bc_INF_ctl
!
      integer (kind = kint) :: i
!
!
!   set boundary_conditons for infinity
!
      if (num_bc_infty .lt. 0) then
        num_bc_infty = 0
      else
        num_bc_infty = surf_bc_INF_ctl%num
      end if
!
!
      if (num_bc_infty .gt. 0) then
!
        call allocate_infty_surf_ctl
!
        bc_infty_name(1:num_bc_infty)                                   &
     &        =  surf_bc_INF_ctl%c2_tbl(1:num_bc_infty)
        bc_infty_magnitude(1:num_bc_infty)                              &
     &        = surf_bc_INF_ctl%vect(1:num_bc_infty)
!
        do i = 1, num_bc_infty
         call set_surf_infty_group_types(surf_bc_INF_ctl%c1_tbl(i),     &
     &       ibc_infty_type(i))
        end do
!
        call dealloc_control_array_c2_r(surf_bc_INF_ctl)
      end if
!
      end subroutine s_set_control_4_infty
!
! -----------------------------------------------------------------------
!
      end module set_control_4_infty
