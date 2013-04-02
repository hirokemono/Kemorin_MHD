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
!
!
      if ( iflag_4_lorentz.eq.1 ) then
        if (iflag_4_rotate.eq.0) then
          call cal_sol_pressure_w_mag_ene
        else if ( iflag_magneto_cv.eq.1 .and. iflag_4_rotate.eq.0) then
          call cal_sol_pressure_mcv
        else
          call init_4_sol_k_potential
        end if
      else
        call init_4_sol_k_potential
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
      call int_norm_div_v_monitor(iloop)
!      call int_rms_div_v_monitor
!
      do iloop = 0, maxiter
!
        call cal_mod_potential
!
        call cal_sol_pressure
!
        call cal_velocity_co
!
!
        call cal_rms_pressure_4_loop(iloop, rsig)
!
        call int_norm_div_v_monitor(iloop)
!        call int_rms_div_v_monitor(iloop)
!
        if ( abs(rsig) .lt. eps_4_velo ) go to 10
!
      end do
 10   continue
!
      if (iflag_4_rotate.eq.1) then
        call cal_sol_pressure_rotate
      end if
!
      end subroutine velocity_evolution
!
!-----------------------------------------------------------------------
!
      end module cal_velocity
