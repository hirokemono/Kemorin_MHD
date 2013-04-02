!
!      module add_nodal_fields_4_MHD
!
!        programmed by H.Matsui on Sep., 2006
!
!      subroutine add_field_name_4_mhd
!
      module add_nodal_fields_4_MHD
!
      use m_precision
!
      use m_control_parameter
      use m_phys_labels
      use add_nodal_fields_ctl
!
      implicit  none
!
      private :: add_work_area_4_potentials, add_ctl_4_ref_temp
      private :: add_data_4_previous_step, add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_mhd
!
      use m_machine_parameter
!
!    set work fields for potentials
!
      if (iflag_debug.eq.1) write(*,*) 'add_work_area_4_potentials'
      call add_work_area_4_potentials
!
!    set work fields for reference temperature
!
      if (iflag_debug.eq.1) write(*,*) 'add_ctl_4_ref_temp'
      call add_ctl_4_ref_temp
!
!     set work fields for adams-bashforth
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_previous_step'
      call add_data_4_previous_step
!
!     set work fields for evolution check
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_check_step'
      call add_data_4_check_step
!
      end subroutine add_field_name_4_mhd
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_ctl_4_ref_temp
!
!
      if ( iflag_4_ref_temp .gt. 0) then
        call add_phys_name_tmp(fhd_part_temp)
        call add_phys_name_tmp(fhd_ref_temp)
        call add_phys_name_tmp(fhd_grad_ref_temp)
      end if
!
      if (iflag_4_coriolis .gt. 0) call add_phys_name_tmp(fhd_Coriolis)
      if (iflag_4_gravity .eq. 2)                                       &
     &                         call add_phys_name_tmp(fhd_buoyancy)
      if (iflag_4_composit_buo .eq. 2)                                  &
     &                         call add_phys_name_tmp(fhd_comp_buo)
      if (iflag_4_filter_gravity .eq. 2)                                &
     &                         call add_phys_name_tmp(fhd_filter_buo)
!
      end subroutine add_ctl_4_ref_temp
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_potentials
!
!    set work fields for potentials
!
      if (iflag_t_evo_4_velo.gt.0) then
        call add_phys_name_tmp(fhd_press_work)
      end if
      if (iflag_t_evo_4_magne.gt.0 .or. iflag_t_evo_4_vect_p.gt.0) then
        call add_phys_name_tmp(fhd_m_potential_work)
      end if
!
      end subroutine add_work_area_4_potentials
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_previous_step
!
!
      if(iflag_t_evo_4_velo .ge. 1) then
        call add_phys_name_tmp(fhd_pre_mom)
        call add_phys_name_tmp(fhd_pre_press)
!
        call add_phys_name_tmp(fhd_forces)
        call add_phys_name_tmp(fhd_div_forces)
      end if
      if(iflag_t_evo_4_magne.ge.1 .or. iflag_t_evo_4_vect_p.ge.1) then
        call add_phys_name_tmp(fhd_pre_uxb)
      end if
      if(iflag_t_evo_4_temp .ge.  1) then
        call add_phys_name_tmp(fhd_pre_heat)
      end if
      if(iflag_t_evo_4_composit .ge.  1) then
        call add_phys_name_tmp(fhd_pre_composit)
      end if
!
      end subroutine add_data_4_previous_step
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_check_step
!
!
      if(iflag_t_evo_4_velo .ge. 1) then
        call add_phys_name_tmp(fhd_chk_mom)
        call add_phys_name_tmp(fhd_chk_press)
      end if
      if(iflag_t_evo_4_magne.ge.1 .or. iflag_t_evo_4_vect_p.ge.1) then
        call add_phys_name_tmp(fhd_chk_uxb)
        call add_phys_name_tmp(fhd_chk_potential)
      end if
      if(iflag_t_evo_4_temp .ge.  1) then
        call add_phys_name_tmp(fhd_chk_heat)
      end if
      if(iflag_t_evo_4_composit .ge.  1) then
        call add_phys_name_tmp(fhd_chk_composit)
      end if
!
!      if(iflag_t_evo_4_velo .ge. 3) then
!        call add_phys_name_tmp(fhd_chk_mom_2)
!        call add_phys_name_tmp(fhd_chk_press_2)
!      end if
!      if(iflag_t_evo_4_magne.ge.3 .or. iflag_t_evo_4_vect_p.ge.3) then
!        call add_phys_name_tmp(fhd_chk_uxb_2)
!        call add_phys_name_tmp(fhd_chk_potential_2)
!      end if
!      if(iflag_t_evo_4_temp .ge.  3) then
!        call add_phys_name_tmp(fhd_chk_heat_2)
!      end if
!      if(iflag_t_evo_4_composit .ge.  3) then
!        call add_phys_name_tmp(fhd_chk_composit_2)
!      end if
!
      end subroutine add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_MHD
