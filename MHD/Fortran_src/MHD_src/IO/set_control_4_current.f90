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
!!      subroutine s_set_control_4_current                              &
!!     &         (cd_prop, node_bc_J_ctl, surf_bc_JN_ctl,               &
!!     &          current_nod, current_surf)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(ctl_array_c2r), intent(in) :: node_bc_J_ctl
!!        type(ctl_array_c2r), intent(in) :: surf_bc_JN_ctl
!!        type(boundary_condition_list), intent(inout) :: current_nod
!!        type(boundary_condition_list), intent(inout) :: current_surf
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
      subroutine s_set_control_4_current                                &
     &         (cd_prop, node_bc_J_ctl, surf_bc_JN_ctl,                 &
     &          current_nod, current_surf)
!
      use calypso_mpi
      use m_machine_parameter
      use t_physical_property
      use t_read_control_arrays
      use t_bc_data_list
      use set_node_group_types
      use set_surface_group_types
!
      type(conductive_property), intent(in) :: cd_prop
      type(ctl_array_c2r), intent(in) :: node_bc_J_ctl
      type(ctl_array_c2r), intent(in) :: surf_bc_JN_ctl
      type(boundary_condition_list), intent(inout) :: current_nod
      type(boundary_condition_list), intent(inout) :: current_surf
!
      integer (kind = kint) :: i
!
!
      if (      cd_prop%iflag_Bevo_scheme .eq. id_no_evolution          &
     &   .and.  cd_prop%iflag_Aevo_scheme .eq. id_no_evolution) then
        current_nod%num_bc =  0
        current_surf%num_bc = 0
      else
        current_nod%num_bc =  node_bc_J_ctl%num
        current_surf%num_bc = surf_bc_JN_ctl%num
      end if
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'current_nod%num_bc ',current_nod%num_bc
      if (current_nod%num_bc .gt. 0) then
!
        call alloc_bc_type_ctl(current_nod)
!
        current_nod%bc_name(1:current_nod%num_bc)                       &
     &             = node_bc_J_ctl%c2_tbl(1:current_nod%num_bc)
        current_nod%bc_magnitude(1:current_nod%num_bc)                  &
     &             = node_bc_J_ctl%vect(1:current_nod%num_bc)
!
        if (iflag_debug.eq.1) write(*,*) 'current_nod%bc_magnitude ',   &
     &                                    current_nod%bc_magnitude
!
        do i = 1, current_nod%num_bc
         call set_bc_group_types_vector(node_bc_J_ctl%c1_tbl(i),        &
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
        call alloc_bc_type_ctl(current_surf)
!
        current_surf%bc_name(1:current_surf%num_bc)                     &
     &      = surf_bc_JN_ctl%c2_tbl(1:current_surf%num_bc)
        current_surf%bc_magnitude(1:current_surf%num_bc)                &
     &      = surf_bc_JN_ctl%vect(1:current_surf%num_bc)
!
        do i = 1, current_surf%num_bc
          call set_surf_group_types_vector(surf_bc_JN_ctl%c1_tbl(i),    &
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
