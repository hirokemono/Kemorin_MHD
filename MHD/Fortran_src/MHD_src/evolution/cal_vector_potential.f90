!
!      module cal_vector_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!------- subroutine cal_magne_vector_potential ---------------------
!
      module cal_vector_potential
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
      subroutine cal_magne_vector_potential
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
!
      use cal_vector_potential_pre
      use cal_electric_potential
      use cal_sol_magne_potential
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_vector_p_correct
      use cal_rms_potentials
!
      integer(kind=kint ) :: iloop
!
!
      call init_4_sol_m_potential
!
!     --------------------- 
!
      if (iflag_debug.eq.1)  write(*,*) 'vector_p_pre'
      call cal_vector_p_pre
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_a_monitor(iloop)
!      call int_rms_div_a_monitor(iloop)
!
      call init_4_sol_m_potential
!
      do iloop = 0, maxiter_vecp
!
        call cal_scalar_potential
!
        call cal_sol_m_potential
!
        if (iflag_debug.eq.1) write(*,*) 'vector_potential_correct'
        call cal_vector_p_co
!
!
        call cal_rms_scsalar_potential(iloop, rsig)
!
        call int_norm_div_a_monitor(iloop)
!        call int_rms_div_a_monitor(iloop)
!
        if ( abs(rsig) .lt. eps_4_magne ) go to 20
!
      end do
 20   continue
!
      end subroutine cal_magne_vector_potential
!
!-----------------------------------------------------------------------
!
      end module cal_vector_potential
