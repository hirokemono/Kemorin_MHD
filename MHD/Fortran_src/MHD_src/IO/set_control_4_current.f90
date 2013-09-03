!>@file   set_control_4_current.f90
!!@brief  module set_control_4_current
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for current density from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_current
!!@endverbatim
!
      module set_control_4_current
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
      subroutine s_set_control_4_current
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_phys_address
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_surface_group_types
!
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i
!
!
       if ( iflag_t_evo_4_magne .eq. id_no_evolution                    &
      &     .and.  iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        num_bc_j = 0
        num_bc_js = 0
      else
        num_bc_j = num_bc_j_ctl
        num_bc_js = num_bc_grad_j_ctl
      end if
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'num_bc_j ',num_bc_j
      if (num_bc_j .gt. 0) then
!
        call allocate_nod_bc_list_j
!
        bc_j_name      = bc_j_name_ctl
        bc_j_magnitude = bc_j_magnitude_ctl
!
        if (iflag_debug.eq.1) then
          write(*,*) 'bc_j_magnitude ',bc_j_magnitude
        end if
!
        do i = 1, num_bc_j
          tmpchara = bc_j_type_ctl(i)
          if ( tmpchara .eq. 'fix_x' ) then
            ibc_j_type(i) = 1
          else if ( tmpchara .eq. 'fix_y' ) then
            ibc_j_type(i) = 2
          else if ( tmpchara .eq. 'fix_z' ) then
            ibc_j_type(i) = 3
          else if ( tmpchara .eq. 'file_x' ) then
            ibc_j_type(i) = -1
          else if ( tmpchara .eq. 'file_y' ) then
            ibc_j_type(i) = -2
          else if ( tmpchara .eq. 'file_z' ) then
            ibc_j_type(i) = -3
          end if
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'i, ibc_j_type, bc_j_magnitude, bc_j_name'
          do i = 1, num_bc_j
            write(*,*) i, ibc_j_type(i), bc_j_magnitude(i),             &
     &                 trim(bc_j_name(i))
          end do
        end if
      end if
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'num_bc_js ',num_bc_js
      if (num_bc_js .gt. 0) then
!
        call allocate_current_surf_ctl
!
        bc_js_name     =   bc_grad_j_name_ctl
        bc_js_magnitude =  bc_grad_j_magnitude_ctl
!
        do i = 1, num_bc_js
          call set_surf_group_types_vector(bc_grad_j_type_ctl(i),       &
     &        ibc_js_type(i))
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'i, ibc_js_type, bc_js_magnitude, bc_js_name'
          do i = 1, num_bc_js
            write(*,*) i, ibc_js_type(i), bc_js_magnitude(i),           &
     &                 trim(bc_js_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_current
!
! -----------------------------------------------------------------------
!
      end module set_control_4_current
