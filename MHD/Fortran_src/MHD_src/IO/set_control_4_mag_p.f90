!
!      module set_control_4_mag_p
!
!        programmed by H.Matsui
!        modified by H.Matsui on Aug., 2007
!
!
!     subroutine s_set_control_4_mag_p
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
!   set boundary_conditons for magnetic potential
!
      if ( iflag_t_evo_4_magne .eq. 0                                   &
     &       .and.  iflag_t_evo_4_vect_p .eq. 0 ) then
        num_bc_mag_p = 0
        num_surf_magp = 0
      else
        num_bc_mag_p = num_bc_mag_p_ctl
        num_surf_magp = num_bc_grad_magp_ctl
      end if
!
!
      if (num_bc_mag_p/=0) then
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
      if (num_surf_magp/=0) then
!
        call allocate_magp_surf_ctl
!
        surf_magp_name      =  bc_grad_magp_name_ctl
        surf_magp_magnitude = bc_grad_magp_magnitude_ctl
        isurf_magp_type= 0
!
!
        do i = 1, num_surf_magp
          call set_surf_group_types_scalar(bc_grad_magp_type_ctl(i),    &
     &        isurf_magp_type(i) )
          call set_surf_wall_group_types(bc_grad_magp_type_ctl(i),      &
     &        isurf_magp_type(i) )
        end do
!
      end if
!
      end subroutine s_set_control_4_mag_p
!
! -----------------------------------------------------------------------
!
      end module set_control_4_mag_p
