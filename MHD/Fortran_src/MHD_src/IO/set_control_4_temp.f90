!
!      module set_control_4_temp
!
!        programmed by H.Matsui
!        modified by H.Matsui on Aug., 2007
!
!     subroutine s_set_control_4_temp
!
      module set_control_4_temp
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
      subroutine s_set_control_4_temp
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_surface_group_types
!
      integer(kind = kint) :: i
!
!
      if ( iflag_t_evo_4_temp .eq. 0 ) then
        num_bc_e = 0
        num_bc_h_flux = 0
      else
        num_bc_e =      num_bc_e_ctl
        num_bc_h_flux = num_bc_h_flux_ctl
      end if
!
!   set boundary conditions for temperature
!
!
      if (iflag_debug.eq.1) write(12,*)  'num_bc_e ',num_bc_e
      if (num_bc_e/=0) then
!
        call allocate_nod_bc_list_temp
!
        bc_e_name      =  bc_e_name_ctl
        bc_e_magnitude = bc_e_magnitude_ctl
!
        if (iflag_debug.eq.1)  then
          write(12,*) 'bc_e_name ',bc_e_name
          write(12,*) 'bc_e_magnitude ',bc_e_magnitude
        end if
!
        do i = 1, num_bc_e
          if ( bc_e_type_ctl(i) .eq. 'fixed' ) then
            ibc_e_type(i) =  iflag_bc_fix_s
          else if ( bc_e_type_ctl(i) .eq. 'file' ) then
            ibc_e_type(i) = -iflag_bc_fix_s
          else if ( bc_e_type_ctl(i) .eq. 'fixed_flux' ) then
            ibc_e_type(i) =  iflag_bc_fix_flux
          else if ( bc_e_type_ctl(i) .eq. 'sgs' ) then
            ibc_e_type(i) =  iflag_bc_sgs_s
          else if ( bc_e_type_ctl(i) .eq. 'SGS_commute' ) then
            ibc_e_type(i) =  iflag_bc_sgs_commute_s
          end if
        end do
!
      end if
!
!
!
      if (num_bc_h_flux/=0) then
!
        call allocate_temp_surf_ctl
!
        bc_h_flux_magnitude = bc_h_flux_magnitude_ctl
        bc_h_flux_name     =  bc_h_flux_name_ctl
!
        do i = 1, num_bc_h_flux
          call set_surf_group_types_scalar(bc_h_flux_type_ctl(i),       &
     &            ibc_h_flux_type(i))
        end do
      end if
!
      end subroutine s_set_control_4_temp
!
! -----------------------------------------------------------------------
!
      end module set_control_4_temp
