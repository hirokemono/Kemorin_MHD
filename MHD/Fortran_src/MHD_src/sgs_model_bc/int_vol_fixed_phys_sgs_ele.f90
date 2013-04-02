!int_vol_fixed_phys_sgs_ele.f90
!     module int_vol_fixed_phys_sgs_ele
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_vol_sf_press_sgs_ele
!      subroutine int_vol_sf_mag_p_sgs_ele
!      subroutine int_vol_sf_mag_p_ins_sgs_ele
!
!      subroutine int_vol_sf_temp_sgs_ele
!      subroutine int_vol_sf_part_temp_sgs_ele
!      subroutine int_vol_sf_composition_sgs_ele
!
!      subroutine int_vol_sf_velo_sgs_ele
!      subroutine int_vol_sf_vec_p_sgs_ele
!      subroutine int_vol_sf_magne_sgs_ele
!
      module int_vol_fixed_phys_sgs_ele
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_phys_constants
      use m_t_int_parameter
      use m_node_phys_address
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
!
      use int_vol_fixed_fld_sgs_ele
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sf_press_sgs_ele
!
      use m_bc_data_press
!
!
      call int_vol_fixed_sgs_poisson_surf(intg_point_poisson,           &
     &        ibc_p_end, num_index_ibc_press, ele_bc_p_id,              &
     &        ibc_p_stack_smp, ibc_p_shape, n_filter_final,             &
     &        iphys%i_p_phi, ak_diff(1,iak_diff_v) )
!
      end subroutine int_vol_sf_press_sgs_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sf_mag_p_sgs_ele
!
      use m_bc_data_magne_p
!
!
      call int_vol_fixed_sgs_poisson_surf(intg_point_poisson,           &
     &        ibc_mag_p_end, num_index_ibc_mag_p, ele_bc_mag_p_id,      &
     &        ibc_mag_p_stack_smp, ibc_mag_p_shape, n_filter_final,     &
     &        iphys%i_m_phi, ak_diff(1,iak_diff_b) )
!
      end subroutine int_vol_sf_mag_p_sgs_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sf_mag_p_ins_sgs_ele
!
      use m_bc_data_mag_p_ins
!
!
      call int_vol_fixed_sgs_poisson_surf(intg_point_poisson,           &
     &        ibc_mag_pi_end, num_index_ibc_mag_pi, ele_bc_mag_pi_id,   &
     &        ibc_mag_pi_stack_smp, ibc_mag_pi_shape, n_filter_final,   &
     &        iphys%i_m_phi, ak_diff(1,iak_diff_b) )
!
      end subroutine int_vol_sf_mag_p_ins_sgs_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sf_temp_sgs_ele
!
      use m_bc_data_ene
!
!
      call int_vol_fixed_sgs_scalar_surf(intg_point_t_evo,              &
     &    ibc_temp_end, num_index_ibc_temp, ele_bc_temp_id,             &
     &    ibc_temp_stack_smp, ibc_temp_shape, n_filter_final,           &
     &    iphys%i_temp, ak_diff(1,iak_diff_t), ak_d_temp, coef_imp_t)
!
      end subroutine int_vol_sf_temp_sgs_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sf_part_temp_sgs_ele
!
      use m_bc_data_ene
!
!
      call int_vol_fixed_sgs_scalar_surf(intg_point_t_evo,              &
     &    ibc_temp_end, num_index_ibc_temp, ele_bc_temp_id,             &
     &    ibc_temp_stack_smp, ibc_temp_shape, n_filter_final,           &
     &    iphys%i_par_temp, ak_diff(1,iak_diff_t), ak_d_temp,           &
     &    coef_imp_t)
!
      end subroutine int_vol_sf_part_temp_sgs_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_sf_composition_sgs_ele
!
      use m_bc_data_composition
!
!
      call int_vol_fixed_sgs_scalar_surf(intg_point_t_evo,              &
     &    ibc_composition_end, num_index_ibc_compsition,                &
     &    ele_bc_composit_id, ibc_composit_stack_smp,                   &
     &    ibc_composit_shape, n_filter_final,                           &
     &    iphys%i_light, ak_diff(1,iak_diff_c), ak_d_composit,          &
     &    coef_imp_c)
!
      end subroutine int_vol_sf_composition_sgs_ele
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_sf_velo_sgs_ele
!
      use m_bc_data_velo
      use m_bc_data_rotate
!
!
      call int_vol_fixed_sgs_vector_surf(intg_point_t_evo,              &
     &    nmax_idx_ibc_v, ibc_v_end,  num_idx_ibc_v, ele_bc_v_id,       &
     &    ibc_v_stack_smp, ibc_v_shape, n_filter_final,                 &
     &    iphys%i_velo, ak_diff(1,iak_diff_v), ak_d_velo, coef_imp_v)
!
      call int_vol_fixed_rotate_sgs_surf(intg_point_t_evo,              &
     &    ibc_vrot_end, num_index_ibc_vrot, ele_bc_vrot_id,             &
     &    ibc_vrot_stack_smp, ibc_vrot_shape, n_filter_final,           &
     &    iphys%i_velo, ak_diff(1,iak_diff_v), ak_d_velo, coef_imp_v)
!
      end subroutine int_vol_sf_velo_sgs_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sf_vec_p_sgs_ele
!
      use m_bc_data_vect_p
!
!
      call int_vol_fixed_sgs_vector_surf(intg_point_t_evo,              &
     &    nmax_idx_ibc_vp, ibc_vp_end, num_idx_ibc_vp, ele_bc_vp_id,    &
     &    ibc_vp_stack_smp, ibc_vp_shape, n_filter_final,               &
     &    iphys%i_vecp, ak_diff(1,iak_diff_b), ak_d_magne, coef_imp_b)
!
      end subroutine int_vol_sf_vec_p_sgs_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sf_magne_sgs_ele
!
      use m_bc_data_magne
!
!
      call int_vol_fixed_sgs_vector_surf(intg_point_t_evo,              &
     &    nmax_idx_ibc_b, ibc_b_end, num_idx_ibc_b, ele_bc_b_id,        &
     &    ibc_b_stack_smp, ibc_b_shape, n_filter_final, iphys%i_magne,  &
     &    ak_diff(1,iak_diff_b), ak_d_magne, coef_imp_b)
!
      end subroutine int_vol_sf_magne_sgs_ele
!
! ----------------------------------------------------------------------
!
      end module int_vol_fixed_phys_sgs_ele
