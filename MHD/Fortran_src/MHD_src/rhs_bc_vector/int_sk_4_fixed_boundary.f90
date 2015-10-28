!
!     module int_sk_4_fixed_boundary
!
!      Written by H. Matsui on Oct., 2005
!
!       subroutine int_vol_sk_po_bc
!       subroutine int_vol_sk_mp_bc
!       subroutine int_vol_sk_mag_p_ins_bc
!
!       subroutine int_sk_4_fixed_temp
!       subroutine int_sk_4_fixed_part_temp
!       subroutine int_sk_4_fixed_velo
!       subroutine int_sk_4_fixed_vector_p
!       subroutine int_sk_4_fixed_magne
!       subroutine int_sk_4_fixed_composition
!
      module int_sk_4_fixed_boundary
!
      use m_precision
!
      use m_phys_constants
      use m_control_parameter
      use m_machine_parameter
      use m_t_int_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_jacobians
      use m_sorted_node
      use m_finite_element_matrix
      use m_node_phys_address
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sk_po_bc
!
      use m_finite_element_matrix
      use m_bc_data_press
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf                             &
     &     (node1, ele1, nod_fld1, jac1_3d_l, rhs_tbl1, FEM1_elen,      &
     &      intg_point_poisson, ibc_p_end, num_index_ibc_press,         &
     &      ele_bc_p_id, ibc_p_stack_smp, ibc_p_shape, ifilter_final,   &
     &      iphys%i_p_phi, ak_diff(1,iak_diff_v), fem1_wk, f1_l )
      else
        call int_vol_fixed_poisson_surf                                 &
     &     (node1, ele1, nod_fld1, jac1_3d_l, rhs_tbl1,                 &
     &      intg_point_poisson, ibc_p_end, num_index_ibc_press,         &
     &      ele_bc_p_id, ibc_p_stack_smp, ibc_p_shape,                  &
     &      iphys%i_p_phi, fem1_wk, f1_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node1, rhs_tbl1, n_scalar, f1_l%ff_smp, f1_l%ff)
!
      end subroutine int_vol_sk_po_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mp_bc
!
      use m_finite_element_matrix
      use m_bc_data_magne_p
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf                             &
     &     (node1, ele1, nod_fld1, jac1_3d_l, rhs_tbl1, FEM1_elen,      &
     &      intg_point_poisson, ibc_mag_p_end, num_index_ibc_mag_p,     &
     &      ele_bc_mag_p_id,  ibc_mag_p_stack_smp, ibc_mag_p_shape,     &
     &      ifilter_final, iphys%i_m_phi, ak_diff(1,iak_diff_b),        &
     &      fem1_wk, f1_l)
      else
        call int_vol_fixed_poisson_surf                                 &
     &     (node1, ele1, nod_fld1, jac1_3d_l, rhs_tbl1,                 &
     &      intg_point_poisson, ibc_mag_p_end, num_index_ibc_mag_p,     &
     &      ele_bc_mag_p_id,  ibc_mag_p_stack_smp, ibc_mag_p_shape,     &
     &      iphys%i_m_phi, fem1_wk, f1_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node1, rhs_tbl1, n_scalar, f1_l%ff_smp, f1_l%ff)
!      call check_ff(my_rank, n_scalar, node1%numnod, f1_l)
!
      end subroutine int_vol_sk_mp_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mag_p_ins_bc
!
      use m_bc_data_mag_p_ins
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf                             &
     &     (node1, ele1, nod_fld1, jac1_3d_l, rhs_tbl1, FEM1_elen,      &
     &      intg_point_poisson, ibc_mag_pi_end, num_index_ibc_mag_pi,   &
     &      ele_bc_mag_pi_id, ibc_mag_pi_stack_smp, ibc_mag_pi_shape,   &
     &      ifilter_final, iphys%i_m_phi, ak_diff(1,iak_diff_b),        &
     &      fem1_wk, f1_l)
      else
        call int_vol_fixed_poisson_surf                                 &
     &     (node1, ele1, nod_fld1, jac1_3d_l, rhs_tbl1,                 &
     &      intg_point_poisson, ibc_mag_pi_end, num_index_ibc_mag_pi,   &
     &      ele_bc_mag_pi_id, ibc_mag_pi_stack_smp, ibc_mag_pi_shape,   &
     &      iphys%i_m_phi, fem1_wk, f1_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node1, rhs_tbl1, n_scalar, f1_l%ff_smp, f1_l%ff)
!
      end subroutine int_vol_sk_mag_p_ins_bc
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_temp
!
      use m_bc_data_ene
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
!
      if (iflag_commute_temp .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_scalar_surf                              &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      intg_point_t_evo, ibc_temp_end, num_index_ibc_temp,         &
     &      ele_bc_temp_id, ibc_temp_stack_smp, ibc_temp_shape,         &
     &      ifilter_final, iphys%i_temp, ak_diff(1,iak_diff_t),         &
     &      ak_d_temp, coef_imp_t, fem1_wk, f1_l)
      else
        call int_vol_fixed_scalar_surf                                  &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                 &
     &      intg_point_t_evo, ibc_temp_end, num_index_ibc_temp,         &
     &      ele_bc_temp_id, ibc_temp_stack_smp, ibc_temp_shape,         &
     &      iphys%i_temp, ak_d_temp, coef_imp_t, fem1_wk, f1_l)
      end if
!
      end subroutine int_sk_4_fixed_temp
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_part_temp
!
      use m_bc_data_ene
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
!
      if (iflag_commute_temp .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_scalar_surf                              &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      intg_point_t_evo, ibc_temp_end, num_index_ibc_temp,         &
     &      ele_bc_temp_id, ibc_temp_stack_smp, ibc_temp_shape,         &
     &      ifilter_final, iphys%i_par_temp, ak_diff(1,iak_diff_t),     &
     &      ak_d_temp, coef_imp_t, fem1_wk, f1_l)
      else
        call int_vol_fixed_scalar_surf                                  &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                 &
     &      intg_point_t_evo, ibc_temp_end, num_index_ibc_temp,         &
     &      ele_bc_temp_id, ibc_temp_stack_smp, ibc_temp_shape,         &
     &      iphys%i_par_temp, ak_d_temp, coef_imp_t, fem1_wk, f1_l)
      end if
!
      end subroutine int_sk_4_fixed_part_temp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_velo
!
      use m_bc_data_velo
      use m_bc_data_rotate
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_vector_surf                              &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      intg_point_t_evo, nmax_idx_ibc_v, nod_bc1_v%ibc_end,                &
     &      num_idx_ibc_v, ele_bc_v_id,                                 &
     &      nod_bc1_v%ibc_stack_smp, nod_bc1_v%ibc_shape, ifilter_final,                &
     &      iphys%i_velo, ak_diff(1,iak_diff_v), ak_d_velo, coef_imp_v, &
     &      fem1_wk, f1_l)
!
        call int_vol_fixed_rotate_sgs_surf                              &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      intg_point_t_evo, ibc_vrot_end, num_index_ibc_vrot,         &
     &      ele_bc_vrot_id, ibc_vrot_stack_smp, ibc_vrot_shape,         &
     &      ifilter_final, iphys%i_velo, ak_diff(1,iak_diff_v),         &
     &      ak_d_velo, coef_imp_v, fem1_wk, f1_l)
      else
        call int_vol_fixed_vector_surf                                  &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                 &
     &      intg_point_t_evo, nmax_idx_ibc_v, nod_bc1_v%ibc_end, num_idx_ibc_v, &
     &      ele_bc_v_id, nod_bc1_v%ibc_stack_smp, nod_bc1_v%ibc_shape,                  &
     &      iphys%i_velo, ak_d_velo, coef_imp_v, fem1_wk, f1_l)
!
        call int_vol_fixed_rotate_surf                                  &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                 &
     &      intg_point_t_evo, ibc_vrot_end, num_index_ibc_vrot,         &
     &      ele_bc_vrot_id, ibc_vrot_stack_smp, ibc_vrot_shape,         &
     &      iphys%i_velo, ak_d_velo, coef_imp_v, fem1_wk, f1_l)
      end if
!
      end subroutine int_sk_4_fixed_velo
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_vector_p
!
      use m_bc_data_vect_p
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_vector_surf                              &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      intg_point_t_evo, nmax_idx_ibc_vp, ibc_vp_end,              &
     &      num_idx_ibc_vp, ele_bc_vp_id,                               &
     &      ibc_vp_stack_smp, ibc_vp_shape, ifilter_final,              &
     &      iphys%i_vecp, ak_diff(1,iak_diff_b), ak_d_magne,            &
     &      coef_imp_b, fem1_wk, f1_l)
      else
        call int_vol_fixed_vector_surf                                  &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                 &
     &      intg_point_t_evo, nmax_idx_ibc_vp, ibc_vp_end,              &
     &      num_idx_ibc_vp, ele_bc_vp_id, ibc_vp_stack_smp,             &
     &      ibc_vp_shape, iphys%i_vecp, ak_d_magne, coef_imp_b,         &
     &      fem1_wk, f1_l)
       end if
!
       end subroutine int_sk_4_fixed_vector_p
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_magne
!
      use m_bc_data_magne
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_vector_surf                              &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      intg_point_t_evo, nmax_idx_ibc_b, ibc_b_end,                &
     &      num_idx_ibc_b, ele_bc_b_id,                                 &
     &      ibc_b_stack_smp, ibc_b_shape, ifilter_final, iphys%i_magne, &
     &      ak_diff(1,iak_diff_b), ak_d_magne, coef_imp_b,              &
     &      fem1_wk, f1_l)
      else
        call int_vol_fixed_vector_surf                                  &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                 &
     &      intg_point_t_evo, nmax_idx_ibc_b, ibc_b_end, num_idx_ibc_b, &
     &      ele_bc_b_id, ibc_b_stack_smp, ibc_b_shape,                  &
     &      iphys%i_magne, ak_d_magne, coef_imp_b, fem1_wk, f1_l)
       end if
!
       end subroutine int_sk_4_fixed_magne
!
! ----------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_composition
!
      use m_bc_data_composition
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
!
      if (iflag_commute_composit .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_scalar_surf                              &
       &   (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
       &    intg_point_t_evo, ibc_composition_end,                      &
       &    num_index_ibc_compsition, ele_bc_composit_id,               &
       &    ibc_composit_stack_smp,  ibc_composit_shape, ifilter_final, &
       &    iphys%i_light, ak_diff(1,iak_diff_c), ak_d_composit,        &
       &    coef_imp_c, fem1_wk, f1_l)
      else
        call int_vol_fixed_scalar_surf                                  &
     &     (node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                 &
     &      intg_point_t_evo, ibc_composition_end,                      &
     &      num_index_ibc_compsition, ele_bc_composit_id,               &
     &      ibc_composit_stack_smp, ibc_composit_shape,                 &
     &      iphys%i_light, ak_d_composit, coef_imp_c, fem1_wk, f1_l)
      end if
!
       end subroutine int_sk_4_fixed_composition
!
! ----------------------------------------------------------------------
!
      end module int_sk_4_fixed_boundary
