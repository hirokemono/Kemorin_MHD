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
!!      subroutine s_set_control_4_mag_p                                &
!!     &         (evo_B, evo_A, node_bc_MP_ctl, surf_bc_MPN_ctl)
!!        type(time_evolution_params), intent(in) :: evo_B, evo_A
!!        type(ctl_array_c2r), intent(inout) :: node_bc_MP_ctl
!!        type(ctl_array_c2r), intent(inout) :: surf_bc_MPN_ctl
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
      subroutine s_set_control_4_mag_p                                  &
     &         (evo_B, evo_A, node_bc_MP_ctl, surf_bc_MPN_ctl)
!
      use m_machine_parameter
      use calypso_mpi
      use t_time_stepping_parameter
      use t_read_control_arrays
      use m_bc_data_list
      use m_surf_data_list
      use set_node_group_types
      use set_surface_group_types
!
      type(time_evolution_params), intent(in) :: evo_B, evo_A
      type(ctl_array_c2r), intent(inout) :: node_bc_MP_ctl
      type(ctl_array_c2r), intent(inout) :: surf_bc_MPN_ctl
!
      integer (kind = kint) :: i
!
!
!   set boundary_conditons for magnetic potential
!
      if (      evo_B%iflag_scheme .eq. id_no_evolution                 &
     &   .and.  evo_A%iflag_scheme .eq. id_no_evolution) then
        e_potential_nod%num_bc =  0
        e_potential_surf%num_bc = 0
      else
        e_potential_nod%num_bc =  node_bc_MP_ctl%num
        e_potential_surf%num_bc = surf_bc_MPN_ctl%num
      end if
!
!
      if (e_potential_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_mag_p
!
        e_potential_nod%bc_name(1:e_potential_nod%num_bc)               &
     &      = node_bc_MP_ctl%c2_tbl(1:e_potential_nod%num_bc)
        e_potential_nod%bc_magnitude(1:e_potential_nod%num_bc)          &
     &      = node_bc_MP_ctl%vect(1:e_potential_nod%num_bc)
!
!
        do i = 1, e_potential_nod%num_bc
          call set_bc_group_types_scalar(node_bc_MP_ctl%c1_tbl(i),      &
     &        e_potential_nod%ibc_type(i))
          call set_bc_group_types_sgs_scalar(node_bc_MP_ctl%c1_tbl(i),  &
     &        e_potential_nod%ibc_type(i))
        end do
!
        call dealloc_control_array_c2_r(node_bc_MP_ctl)
      end if
!
!   set boundary_conditons for magnetic potential
!
      if (e_potential_surf%num_bc .gt. 0) then
!
        call allocate_magp_surf_ctl
!
        e_potential_surf%bc_name(1:e_potential_surf%num_bc)             &
     &       = surf_bc_MPN_ctl%c2_tbl(1:e_potential_surf%num_bc)
        e_potential_surf%bc_magnitude(1:e_potential_surf%num_bc)        &
     &       = surf_bc_MPN_ctl%vect(1:e_potential_surf%num_bc)
        e_potential_surf%ibc_type(1:e_potential_surf%num_bc) = 0
!
!
        do i = 1, e_potential_surf%num_bc
          call set_surf_group_types_scalar(surf_bc_MPN_ctl%c1_tbl(i),   &
     &        e_potential_surf%ibc_type(i) )
          call set_surf_wall_group_types(surf_bc_MPN_ctl%c1_tbl(i),     &
     &        e_potential_surf%ibc_type(i) )
        end do
!
        call dealloc_control_array_c2_r(surf_bc_MPN_ctl)
      end if
!
      end subroutine s_set_control_4_mag_p
!
! -----------------------------------------------------------------------
!
      end module set_control_4_mag_p
