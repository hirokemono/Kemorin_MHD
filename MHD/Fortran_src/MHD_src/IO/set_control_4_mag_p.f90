!>@file   set_control_4_mag_p.f90
!!@brief  module set_control_4_mag_p
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for electric scalar potential
!!        from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_mag_p
!!@endverbatim
!
      module set_control_4_mag_p
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
      subroutine s_set_control_4_mag_p
!
      use m_machine_parameter
      use calypso_mpi
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
      integer (kind = kint) :: i
!
!
!   set boundary_conditons for magnetic potential
!
      if (iflag_t_evo_4_magne .eq. id_no_evolution                      &
     &       .and.  iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        e_potential_nod%num_bc =  0
        e_potential_surf%num_bc = 0
      else
        e_potential_nod%num_bc =  num_bc_mag_p_ctl
        e_potential_surf%num_bc = num_bc_grad_magp_ctl
      end if
!
!
      if (e_potential_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_mag_p
!
        e_potential_nod%bc_name =      bc_mag_p_name_ctl
        e_potential_nod%bc_magnitude = bc_mag_p_magnitude_ctl
!
!
        do i = 1, e_potential_nod%num_bc
          call set_bc_group_types_scalar(bc_mag_p_type_ctl(i),          &
     &        e_potential_nod%ibc_type(i))
          call set_bc_group_types_sgs_scalar(bc_mag_p_type_ctl(i),      &
     &        e_potential_nod%ibc_type(i))
        end do
!
      end if
!
!   set boundary_conditons for magnetic potential
!
      if (e_potential_surf%num_bc .gt. 0) then
!
        call allocate_magp_surf_ctl
!
        e_potential_surf%bc_name =      bc_grad_magp_name_ctl
        e_potential_surf%bc_magnitude = bc_grad_magp_magnitude_ctl
        e_potential_surf%ibc_type=      0
!
!
        do i = 1, e_potential_surf%num_bc
          call set_surf_group_types_scalar(bc_grad_magp_type_ctl(i),    &
     &        e_potential_surf%ibc_type(i) )
          call set_surf_wall_group_types(bc_grad_magp_type_ctl(i),      &
     &        e_potential_surf%ibc_type(i) )
        end do
!
      end if
!
      end subroutine s_set_control_4_mag_p
!
! -----------------------------------------------------------------------
!
      end module set_control_4_mag_p
