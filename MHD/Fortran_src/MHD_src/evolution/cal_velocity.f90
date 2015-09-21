!
!      module cal_velocity
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!      subroutine cal_velocity
!
      module cal_velocity
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine velocity_evolution
!
      use m_control_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
!
      use cal_velocity_pre
      use cal_mod_vel_potential
      use cal_sol_pressure_MHD
      use cal_velocity_correct
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_rms_potentials
!
      integer(kind=kint) :: iloop
      real(kind = kreal) :: rel_correct
!
!
      if (iflag_4_lorentz .eq. id_turn_ON) then
        if (iflag_4_rotate .eq. id_turn_OFF) then
          call cal_sol_pressure_w_mag_ene                               &
     &       (node1%numnod, node1%istack_internal_smp,                  &
     &        nod_fld1%ntot_phys, iphys%i_p_phi, iphys%i_magne,         &
     &        iphys%i_press, d_nod)
        else if (iflag_magneto_cv .eq. id_turn_ON                       &
     &     .and. iflag_4_rotate .eq. id_turn_OFF) then
          call cal_sol_pressure_mcv                                     &
     &       (node1%numnod, node1%istack_internal_smp,                  &
     &        nod_fld1%ntot_phys, iphys%i_p_phi, iphys%i_magne,         &
     &        iphys%i_press, d_nod)
        else
          call init_4_sol_k_potential(node1%istack_nod_smp)
        end if
      else
        call init_4_sol_k_potential(node1%istack_nod_smp)
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1)  write(*,*) 's_cal_velocity_pre'
      call s_cal_velocity_pre
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_v_monitor(iloop, rel_correct)
!      call int_rms_div_v_monitor(iloop, rel_correct)
!
      do iloop = 0, maxiter
!
        call cal_mod_potential
!
        call cal_sol_pressure                                           &
     &     (node1%numnod, node1%istack_internal_smp,                    &
     &      nod_fld1%ntot_phys, iphys%i_p_phi, iphys%i_press, d_nod)
!
        call cal_velocity_co
!
!
        call cal_rms_pressure_4_loop(iloop, rel_correct)
!
        call int_norm_div_v_monitor(iloop, rel_correct)
!        call int_rms_div_v_monitor(iloop, rel_correct)
!
        if ( abs(rel_correct) .lt. eps_4_velo ) go to 10
!
      end do
 10   continue
!
      if (iflag_4_rotate .eq. id_turn_ON) then
        call cal_sol_pressure_rotate                                    &
     &     (node1%numnod, node1%istack_internal_smp,                    &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press, d_nod)
      end if
!
      end subroutine velocity_evolution
!
!-----------------------------------------------------------------------
!
      end module cal_velocity
