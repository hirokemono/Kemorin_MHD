!
!      module cal_velocity
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!      subroutine velocity_evolution(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_velocity
!
      use m_precision
!
      use t_layering_ele_list
!
      implicit none
!
      real(kind = kreal) :: ave_pr0, rms_pr0
      private :: ave_pr0, rms_pr0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine velocity_evolution(layer_tbl)
!
      use m_control_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_physical_property
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_SGS_model_coefs
      use m_SGS_address
      use m_geometry_data_MHD
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
      type(layering_tbl), intent(in) :: layer_tbl
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
     &        iphys%i_press, nod_fld1%d_fld)
        else if (iflag_magneto_cv .eq. id_turn_ON                       &
     &     .and. iflag_4_rotate .eq. id_turn_OFF) then
          call cal_sol_pressure_mcv                                     &
     &       (node1%numnod, node1%istack_internal_smp,                  &
     &        nod_fld1%ntot_phys, iphys%i_p_phi, iphys%i_magne,         &
     &        iphys%i_press, nod_fld1%d_fld)
        else
          call init_sol_potential(node1%numnod, node1%istack_nod_smp,   &
     &        coef_press, nod_fld1%ntot_phys, iphys%i_p_phi,            &
     &        iphys%i_press, nod_fld1%d_fld)
        end if
      else
        call init_sol_potential(node1%numnod, node1%istack_nod_smp,     &
     &      coef_press, nod_fld1%ntot_phys, iphys%i_p_phi,              &
     &      iphys%i_press, nod_fld1%d_fld)
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1)  write(*,*) 's_cal_velocity_pre'
      call s_cal_velocity_pre(layer_tbl)
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_v_monitor(iloop, node1, ele1, fluid1,           &
     &    iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!      call int_rms_div_v_monitor(iloop, node1, ele1, fluid1,           &
!     &    iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!
      do iloop = 0, maxiter
!
        call cal_mod_potential(iak_diff_v,                              &
     &      node1, ele1, surf1, fluid1, sf_grp1, iphys,                 &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_l, rhs_tbl1,           &
     &      FEM1_elen, fem1_wk, f1_l, f1_nl, nod_fld1)
!
        call cal_sol_pressure                                           &
     &     (node1%numnod, node1%istack_internal_smp,                    &
     &      nod_fld1%ntot_phys, iphys%i_p_phi, iphys%i_press,           &
     &      nod_fld1%d_fld)
!
        call cal_velocity_co
!
!
        call cal_rms_scalar_potential(iloop, fluid1%istack_ele_fld_smp, &
     &      iphys%i_press, i_rms%i_press, j_ave%i_press,                &
     &      node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l, fem1_wk,       &
     &      rel_correct, ave_pr0, rms_pr0)
!
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of presssur correction: ',  &
     &         iloop, ave_pr0, rms_pr0
!
!
        call int_norm_div_v_monitor(iloop, node1, ele1, fluid1,         &
     &      iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!        call int_rms_div_v_monitor(iloop, node1, ele1, fluid1,         &
!     &      iphys, nod_fld1, jac1_3d_q, fem1_wk, rel_correct)
!
        if ( abs(rel_correct) .lt. eps_4_velo ) go to 10
!
      end do
 10   continue
!
      if (iflag_4_rotate .eq. id_turn_ON) then
        call cal_sol_pressure_rotate                                    &
     &     (node1%numnod, node1%istack_internal_smp,                    &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      nod_fld1%d_fld)
      end if
!
      end subroutine velocity_evolution
!
!-----------------------------------------------------------------------
!
      end module cal_velocity
