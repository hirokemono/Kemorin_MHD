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
      real(kind = kreal) :: ave_mp0, rms_mp0
      private :: ave_mp0, rms_mp0
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
      use m_node_phys_data
      use m_physical_property
      use m_SGS_address
!
      use cal_vector_potential_pre
      use cal_mod_vel_potential
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
      call int_norm_div_a_monitor(iloop, node1, ele1,                   &
     &    iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!      call int_rms_div_a_monitor(iloop, node1, ele1,                   &
!     &    iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!
      call init_sol_potential(node1%numnod, node1%istack_nod_smp,       &
     &    coef_mag_p, nod_fld1%ntot_phys, iphys%i_m_phi, iphys%i_mag_p, &
     &    nod_fld1%d_fld)
!
      do iloop = 0, maxiter_vecp
!
        if (iflag_debug.gt.0) write(*,*) 'cal_electric_potential'
        call cal_electric_potential(iak_diff_b)
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
        call cal_rms_scalar_potential(iloop, ele1%istack_ele_smp,       &
     &      iphys%i_mag_p, i_rms%i_mag_p, j_ave%i_mag_p,                &
     &      node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l, fem1_wk,       &
     &      rel_correct, ave_mp0, rms_mp0)
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
        if (iflag_debug.gt.0) write(*,*) 'int_norm_div_a_monitor'
        call int_norm_div_a_monitor(iloop, node1, ele1,                 &
     &      iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!        call int_rms_div_a_monitor(iloop, node1, ele1,                 &
!     &      iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!
        if ( abs(rel_correct) .lt. eps_4_magne ) exit
      end do
!
      end subroutine cal_magne_vector_potential
!
!-----------------------------------------------------------------------
!
      end module cal_vector_potential
