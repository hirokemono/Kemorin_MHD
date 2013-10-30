!>@file   set_control_4_vect_p.f90
!!@brief  module set_control_4_vect_p
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for magnetic vector potential
!!        from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_vect_p
!!@endverbatim
!
      module set_control_4_vect_p
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
      subroutine s_set_control_4_vect_p
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
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        a_potential_nod%num_bc =  0
        a_potential_surf%num_bc = 0
      else
        a_potential_nod%num_bc =  num_bc_vp_ctl
        a_potential_surf%num_bc = num_bc_vps_ctl
      end if
!
!   set boundary_conditons for magnetic field
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'a_potential_nod%num_bc ',a_potential_nod%num_bc
      if (a_potential_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_vecp
!
        a_potential_nod%bc_name =      bc_vp_name_ctl
        a_potential_nod%bc_magnitude = bc_vp_magnitude_ctl
!
        do i = 1, a_potential_nod%num_bc
         call set_bc_group_types_vector(bc_vp_type_ctl(i),              &
     &       a_potential_nod%ibc_type(i))
         call set_bc_group_types_sgs_vect(bc_vp_type_ctl(i),            &
     &       a_potential_nod%ibc_type(i))
!
          tmpchara = bc_vp_type_ctl(i)
          if ( tmpchara .eq. 'insulate_shell' ) then
            a_potential_nod%ibc_type(i) = iflag_insulator
!          else if ( tmpchara .eq. 'sph' ) then
!            a_potential_nod%ibc_type(i) = 999
          end if
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, a_potential_nod'
          do i = 1, a_potential_nod%num_bc
            write(*,*)  i, a_potential_nod%ibc_type(i),                 &
     &                  a_potential_nod%bc_magnitude(i),                &
     &                  trim(a_potential_nod%bc_name(i))
          end do
        end if
      end if
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &    write(*,*) 'a_potential_surf%num_bc ',a_potential_surf%num_bc
      if (a_potential_surf%num_bc .gt. 0) then
!
        call allocate_vect_p_surf_ctl
!
        a_potential_surf%bc_name =      bc_vps_name_ctl
        a_potential_surf%bc_magnitude = bc_vps_magnitude_ctl
!
        do i = 1, a_potential_surf%num_bc
          call set_surf_group_types_vector(bc_vps_type_ctl(i),          &
     &        a_potential_surf%ibc_type(i))
          call set_pseudo_vacuum_group_types(bc_vps_type_ctl(i),        &
     &        a_potential_surf%ibc_type(i))
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, a_potential_surf'
          do i = 1, a_potential_surf%num_bc
            write(*,*)  i, a_potential_surf%ibc_type(i),                &
     &                     a_potential_surf%bc_magnitude(i),            &
     &                     trim(a_potential_surf%bc_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_vect_p
!
! -----------------------------------------------------------------------
!
      end module set_control_4_vect_p
