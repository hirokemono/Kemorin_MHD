!
!      module set_control_4_infty
!
!        programmed by H.Matsui
!        modified by H.Matsui on Aug., 2007
!
!     subroutine s_set_control_4_infty
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
      subroutine s_set_control_4_infty
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_ctl_data_surf_boundary
      use m_node_phys_address
      use m_node_group
      use const_bc_infinity_surf
      use set_surface_group_types
!
      integer (kind = kint) :: i
!
!
!   set boundary_conditons for infinity
!
      if (num_bc_infty .lt. 0) then
        num_bc_infty = 0
      else
        num_bc_infty = num_bc_infinity_ctl
      end if
!
!
      if (num_bc_infty .gt. 0) then
!
        call allocate_infty_surf_ctl
!
        bc_infty_name     =  bc_infinity_name_ctl
        bc_infty_magnitude = bc_infinity_magnitude_ctl
!
        do i = 1, num_bc_infty
         call set_surf_infty_group_types(bc_infinity_type_ctl(i),       &
     &       ibc_infty_type(i))
        end do
!
      end if
!
      end subroutine s_set_control_4_infty
!
! -----------------------------------------------------------------------
!
      end module set_control_4_infty
