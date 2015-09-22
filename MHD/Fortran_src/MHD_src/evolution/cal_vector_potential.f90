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
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
      use cal_vector_potential_pre
      use cal_electric_potential
      use cal_sol_pressure_MHD
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_vector_p_correct
      use cal_rms_potentials
!
      integer(kind=kint ) :: iloop
      real(kind = kreal) :: rel_correct
!
!
      call init_sol_potential(node1%numnod, node1%istack_nod_smp,       &
     &    coef_mag_p, nod_fld1%ntot_phys, iphys%i_m_phi, iphys%i_mag_p, &
     &    nod_fld1%d_fld)
!
!     --------------------- 
!
      if (iflag_debug .gt. 0)  write(*,*) 'vector_p_pre'
      call cal_vector_p_pre
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_a_monitor(iloop, rel_correct)
!      call int_rms_div_a_monitor(iloop, rel_correct)
!
      call init_sol_potential(node1%numnod, node1%istack_nod_smp,       &
     &    coef_mag_p, nod_fld1%ntot_phys, iphys%i_m_phi, iphys%i_mag_p, &
     &    nod_fld1%d_fld)
!
      do iloop = 0, maxiter_vecp
!
        if (iflag_debug.gt.0) write(*,*) 'cal_scalar_potential'
        call cal_scalar_potential
!
        if (iflag_debug.gt.0) write(*,*) 'cal_sol_m_potential', iloop
        call cal_sol_m_potential                                        &
     &     (node1%numnod, node1%istack_internal_smp,                    &
     &      nod_fld1%ntot_phys, iphys%i_m_phi, iphys%i_mag_p,           &
     &      nod_fld1%d_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'vector_potential_correct'
        call cal_vector_p_co
!
!
        if (iflag_debug.gt.0) write(*,*) 'cal_rms_scalar_potential'
        call cal_rms_scalar_potential(iloop, rel_correct)
!
        if (iflag_debug.gt.0) write(*,*) 'int_norm_div_a_monitor'
        call int_norm_div_a_monitor(iloop, rel_correct)
!        call int_rms_div_a_monitor(iloop, rel_correct)
!
        if ( abs(rel_correct) .lt. eps_4_magne ) exit
      end do
!
      end subroutine cal_magne_vector_potential
!
!-----------------------------------------------------------------------
!
      end module cal_vector_potential
