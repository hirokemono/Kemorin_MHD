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
      use m_parallel_var_dof
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
      if (iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        num_bc_vp = 0
        num_bc_vps = 0
      else
        num_bc_vp = num_bc_vp_ctl
        num_bc_vps = num_bc_vps_ctl
      end if
!
!   set boundary_conditons for magnetic field
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &             write(*,*) 'num_bc_vp ',num_bc_vp
      if (num_bc_vp/=0) then
!
        call allocate_nod_bc_list_vecp
!
        bc_vp_name = bc_vp_name_ctl
        bc_vp_magnitude = bc_vp_magnitude_ctl
!
        do i = 1, num_bc_vp
          tmpchara = bc_vp_type_ctl(i)
          if ( tmpchara .eq. 'fix_x' ) then
            ibc_vp_type(i) = iflag_bc_fixed + 1
          else if ( tmpchara .eq. 'fix_y' ) then
            ibc_vp_type(i) = iflag_bc_fixed + 2
          else if ( tmpchara .eq. 'fix_z' ) then
            ibc_vp_type(i) = iflag_bc_fixed + 3
          else if ( tmpchara .eq. 'file_x' ) then
            ibc_vp_type(i) = iflag_bc_fixed - 1
          else if ( tmpchara .eq. 'file_y' ) then
            ibc_vp_type(i) = iflag_bc_fixed - 2
          else if ( tmpchara .eq. 'file_z' ) then
            ibc_vp_type(i) = iflag_bc_fixed - 3
          else if ( tmpchara .eq. 'insulate_shell' ) then
            ibc_vp_type(i) = iflag_insulator
          else if ( tmpchara .eq. 'sgs_x' ) then
            ibc_vp_type(i) = iflag_bc_sgs + 1
          else if ( tmpchara .eq. 'sgs_y' ) then
            ibc_vp_type(i) = iflag_bc_sgs + 2
          else if ( tmpchara .eq. 'sgs_z' ) then
            ibc_vp_type(i) = iflag_bc_sgs + 3
!          else if ( tmpchara .eq. 'sph' ) then
!            ibc_vp_type(i) = 999
          end if
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, ibc_vp_type, bc_vp_magnitude, bc_vp_name'
          do i = 1, num_bc_vp
            write(*,*)  i, ibc_vp_type(i), bc_vp_magnitude(i),          &
     &                 trim(bc_vp_name(i))
          end do
        end if
      end if
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &            write(*,*) 'num_bc_vps ',num_bc_vps
      if (num_bc_vps/=0) then
!
        call allocate_vect_p_surf_ctl
!
        bc_vps_name     =  bc_vps_name_ctl
        bc_vps_magnitude = bc_vps_magnitude_ctl
!
        do i = 1, num_bc_vps
          call set_surf_group_types_vector(bc_vps_type_ctl(i),          &
     &        ibc_vps_type(i))
          call set_pseudo_vacuum_group_types(bc_vps_type_ctl(i),        &
     &        ibc_vps_type(i))
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, ibc_vps_type, bc_vps_magnitude, bc_vps_name'
          do i = 1, num_bc_vps
            write(*,*)  i, ibc_vps_type(i), bc_vps_magnitude(i),        &
     &                 trim(bc_vps_name(i))
          end do
        end if
      end if
!
      end subroutine s_set_control_4_vect_p
!
! -----------------------------------------------------------------------
!
      end module set_control_4_vect_p
