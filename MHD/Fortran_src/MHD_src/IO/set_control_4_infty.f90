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
!!     subroutine s_set_control_4_infty(surf_bc_INF_ctl, infty_BC)
!!        type(ctl_array_c2r), intent(in) :: surf_bc_INF_ctl
!!        type(boundary_condition_list), intent(inout) :: infty_BC
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
      subroutine s_set_control_4_infty(surf_bc_INF_ctl, infty_BC)
!
      use calypso_mpi
      use t_control_array_chara2real
      use t_bc_data_list
      use const_bc_infinity_surf
      use set_surface_group_types
!
      type(ctl_array_c2r), intent(in) :: surf_bc_INF_ctl
      type(boundary_condition_list), intent(inout) :: infty_BC
!
      integer (kind = kint) :: i
!
!
!   set boundary_conditons for infinity
!
      if(surf_bc_INF_ctl%num .lt. 0) then
        infty_BC%num_bc = 0
      else
        infty_BC%num_bc = surf_bc_INF_ctl%num
      end if
!
!
      if (infty_BC%num_bc .gt. 0) then
        call alloc_bc_type_ctl(infty_BC)
!
        infty_BC%bc_name(1:infty_BC%num_bc)                             &
     &        =  surf_bc_INF_ctl%c2_tbl(1:infty_BC%num_bc)
        infty_BC%bc_magnitude(1:infty_BC%num_bc)                        &
     &        = surf_bc_INF_ctl%vect(1:infty_BC%num_bc)
!
        do i = 1, infty_BC%num_bc
         call set_surf_infty_group_types(surf_bc_INF_ctl%c1_tbl(i),     &
     &       infty_BC%ibc_type(i))
        end do
      end if
!
      end subroutine s_set_control_4_infty
!
! -----------------------------------------------------------------------
!
      end module set_control_4_infty
