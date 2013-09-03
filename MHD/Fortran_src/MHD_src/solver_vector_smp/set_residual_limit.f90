!
!     module set_residual_limit
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on July, 2006
!
!      subroutine set_residual_4_crank
!
      module set_residual_limit
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
      subroutine set_residual_4_crank
!
      use m_machine_parameter
      use m_physical_property
      use m_control_parameter
      use m_t_int_parameter
      use m_iccg_parameter
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        eps_4_velo_crank = eps_crank * coef_d_velo * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_velo', eps_4_velo_crank
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        eps_4_temp_crank = eps_crank * coef_d_temp * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_temp_crank', eps_4_temp_crank
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        eps_4_magne_crank = eps_crank * coef_d_magne * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_magne_crank', eps_4_magne_crank
      end if
!
      if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        eps_4_magne_crank = eps_crank * coef_d_magne * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_magne_crank', eps_4_magne_crank
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        eps_4_d_scalar_crank = eps_crank * coef_d_magne * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'iflag_t_evo_4_composit', iflag_t_evo_4_composit
      end if
!
      end subroutine set_residual_4_crank
!
! ----------------------------------------------------------------------
!
      end module set_residual_limit
