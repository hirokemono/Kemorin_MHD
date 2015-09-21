!
!      module cal_magnetic_field
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!------- subroutine s_cal_magnetic_field ---------------------
!
      module cal_magnetic_field
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
      subroutine s_cal_magnetic_field
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
      use cal_magnetic_pre
      use cal_sol_magne_potential
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_mod_magne_potential
      use cal_magnetic_correct
      use cal_rms_potentials
!
      integer(kind=kint) :: iloop, maxiter_insulater
      real(kind = kreal) :: rel_correct
!
!
      if ( cd_ele_grp_name(1) .eq. 'all'                                &
     &       .or. cd_ele_grp_name(1) .eq. 'ALL' ) then
        maxiter_insulater = 0
      else
        maxiter_insulater = 1
      end if
!
      call init_sol_potential(node1%numnod, node1%istack_nod_smp,       &
     &    coef_mag_p, nod_fld1%ntot_phys, iphys%i_m_phi, iphys%i_mag_p, &
     &    d_nod)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_magnetic_field_pre'
      call cal_magnetic_field_pre
!
!----  set magnetic field in insulate layer
!
      iloop = -1
      call int_norm_div_b_monitor(iloop, rel_correct)
!
!
      do iloop = 0, maxiter
        call cal_mag_potential
!
        call cal_sol_m_potential                                        &
     &     (node1%numnod, node1%istack_internal_smp,                    &
     &      nod_fld1%ntot_phys, iphys%i_m_phi, iphys%i_mag_p, d_nod)
!
!
      if (iflag_debug.eq.1) write(*,*) 'magnetic_correction'
        call cal_magnetic_co
!
        call cal_rms_scalar_potential(iloop, rel_correct)
!
        call int_norm_div_b_monitor(iloop, rel_correct)
!        call int_rms_div_b_monitor(iloop, rel_correct)
!
        if ( abs(rel_correct) .lt. eps_4_magne ) exit
      end do
!
!
      end subroutine s_cal_magnetic_field
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_field
