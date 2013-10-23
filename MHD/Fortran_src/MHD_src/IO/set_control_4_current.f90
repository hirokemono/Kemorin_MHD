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
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_phys_address
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_node_group_types
      use set_surface_group_types
!
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i
!
!
       if ( iflag_t_evo_4_magne .eq. id_no_evolution                    &
      &     .and.  iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        current_nod%num_bc =  0
        current_surf%num_bc = 0
      else
        current_nod%num_bc =  num_bc_j_ctl
        current_surf%num_bc = num_bc_grad_j_ctl
      end if
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'current_nod%num_bc ',current_nod%num_bc
      if (current_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_j
!
        current_nod%bc_name =      bc_j_name_ctl
        current_nod%bc_magnitude = bc_j_magnitude_ctl
!
        if (iflag_debug.eq.1) write(*,*) 'current_nod%bc_magnitude ',  &
     &                                    current_nod%bc_magnitude
!
        do i = 1, current_nod%num_bc
         call set_nod_group_types_vector(bc_j_type_ctl(i),             &
     &       current_nod%ibc_type(i))
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'i, current_nod'
          do i = 1, current_nod%num_bc
            write(*,*) i, current_nod%ibc_type(i),                      &
     &                 current_nod%bc_magnitude(i),                     &
     &                 trim(current_nod%bc_name(i))
          end do
        end if
      end if
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'current_surf%num_bc ',current_surf%num_bc
      if (current_surf%num_bc .gt. 0) then
!
        call allocate_current_surf_ctl
!
        current_surf%bc_name     =   bc_grad_j_name_ctl
        current_surf%bc_magnitude =  bc_grad_j_magnitude_ctl
!
        do i = 1, current_surf%num_bc
          call set_surf_group_types_vector(bc_grad_j_type_ctl(i),       &
     &        current_surf%ibc_type(i))
        end do
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'i, current_surf'
          do i = 1, current_surf%num_bc
            write(*,*) i, current_surf%ibc_type(i),                     &
     &                current_surf%bc_magnitude(i),                     &
     &                trim(current_surf%bc_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_current
!
! -----------------------------------------------------------------------
!
      end module set_control_4_current
