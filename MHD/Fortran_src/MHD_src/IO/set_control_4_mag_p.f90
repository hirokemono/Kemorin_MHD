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
      use set_surface_group_types
!
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i
!
!
!   set boundary_conditons for magnetic potential
!
      if (iflag_t_evo_4_magne .eq. id_no_evolution                      &
     &       .and.  iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        num_bc_mag_p = 0
        e_potential_surf%num_bc = 0
      else
        num_bc_mag_p = num_bc_mag_p_ctl
        e_potential_surf%num_bc = num_bc_grad_magp_ctl
      end if
!
!
      if (num_bc_mag_p .gt. 0) then
!
        call allocate_nod_bc_list_mag_p
!
        bc_mag_p_name     = bc_mag_p_name_ctl
        bc_mag_p_magnitude = bc_mag_p_magnitude_ctl
!
!
        do i = 1, num_bc_mag_p
          tmpchara = bc_mag_p_type_ctl(i)
          if ( tmpchara .eq. 'fixed' ) then
            ibc_mag_p_type(i) =  iflag_bc_fix_s
          else if ( tmpchara .eq. 'file' ) then
            ibc_mag_p_type(i) = -iflag_bc_fix_s
          else if ( tmpchara .eq. 'sgs' ) then
            ibc_mag_p_type(i) = iflag_bc_sgs_s
          end if
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
