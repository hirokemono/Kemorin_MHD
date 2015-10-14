!
!     module cal_induction_terms
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_vecp_induction
!      subroutine cal_vecp_diffusion
!
      module cal_induction_terms
!
      use m_precision
!
      use m_control_parameter
      use m_finite_element_matrix
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use cal_multi_pass
      use nod_phys_send_recv
      use set_vecp_boundary
      use int_surf_magne_pre
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_vecp_induction
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_data
      use m_int_vol_data
!
      use int_vol_vect_p_pre
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
        call int_vol_vect_p_pre_ele_upm                                 &
     &     (fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      else
        call int_vol_vect_p_pre_ele                                     &
     &     (fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end if
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg)
      call set_boundary_vect_p_4_rhs
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%ml_cd,                                  &
     &    nod_fld1%ntot_phys, iphys%i_vp_induct, nod_fld1%d_fld)
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_vp_induct, nod_fld1%d_fld)
!
      end subroutine cal_vecp_induction
!
!-----------------------------------------------------------------------
!
      subroutine cal_vecp_diffusion
!
      use m_geometry_data
      use m_phys_constants
      use m_geometry_data
      use m_group_data
      use m_node_phys_address
      use m_node_phys_data
      use m_jacobian_sf_grp
      use m_int_vol_data
!
      use int_vol_diffusion_ele
      use int_surf_fixed_gradients
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_vp_diffuse_ele_monitor
!
      call int_sf_grad_vecp                                             &
     &   (ele1, surf1, sf_grp1, jac1_sf_grp_2d_q, intg_point_t_evo)
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
!
      call set_boundary_vect_p_4_rhs
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_l%ff, mhd_fem1_wk%ml_cd,                                   &
     &    nod_fld1%ntot_phys, iphys%i_vp_diffuse, nod_fld1%d_fld)
!
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_vp_diffuse, nod_fld1%d_fld)
!
      end subroutine cal_vecp_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_induction_terms
