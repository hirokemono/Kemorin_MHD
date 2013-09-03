!int_vol_poisson_sgs_mat.f90
!     module int_vol_poisson_sgs_mat
!
! numerical integration for finite elememt equations(Poisson's equation)
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2003 (ver 1.1)
!        modifired by H. Matsui on June, 2005
!        modifired by H. Matsui on Nov., 2007
!
!      subroutine int_vol_velo_sgs_poisson_mat(n_int)
!      subroutine int_vol_magne_sgs_poisson_mat(n_int)
!
!      subroutine int_vol_velo_sgs_crank_mat(n_int)
!      subroutine int_vol_magne_sgs_crank_mat(n_int)
!      subroutine int_vol_vecp_sgs_crank_mat(n_int)
!
!      subroutine int_vol_temp_sgs_crank_mat(n_int)
!      subroutine int_vol_composit_sgs_crank_mat(n_int)
!
      module int_vol_poisson_sgs_mat
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_velo_sgs_poisson_mat(n_int)
!
      use m_sorted_node_MHD
      use m_SGS_model_coefs
      use m_SGS_address
      use m_press_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_sgs_mat11(n_int, idx_4_fll_mat,              &
     &    ifilter_final, ak_diff(1,iak_diff_v),                         &
     &    num_press_comp, aiccg_press)
!
      end subroutine int_vol_velo_sgs_poisson_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_magne_sgs_poisson_mat(n_int)
!
      use m_sorted_node_MHD
      use m_SGS_model_coefs
      use m_SGS_address
      use m_mag_potential_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_sgs_mat11(n_int, idx_4_l_mat,                &
     &    ifilter_final, ak_diff(1,iak_diff_b),                         &
     &    num_mp_comp, aiccg_mag_p)
!
      end subroutine int_vol_magne_sgs_poisson_mat
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_velo_sgs_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_velo_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_sgs_mat33(n_int, idx_4_fl_mat, coef_imp_v,     &
     &    ifilter_final, ak_diff(1,iak_diff_v), ak_d_velo,              &
     &    num_velo_comp, aiccg_velo)
!
      end subroutine int_vol_velo_sgs_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_magne_sgs_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_magne_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
      call int_vol_crank_sgs_mat33(n_int, idx_4_cd_mat_full,            &
     &    coef_imp_b, ifilter_final, ak_diff(1,iak_diff_b),             &
     &    ak_d_magne, num_mag_comp, aiccg_magne)
!
      end subroutine int_vol_magne_sgs_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_vecp_sgs_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_magne_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_sgs_mat33(n_int, idx_4_mat, coef_imp_b,        &
     &    ifilter_final, ak_diff(1,iak_diff_b), ak_d_magne,             &
     &    num_mag_comp, aiccg_magne)
!
      end subroutine int_vol_vecp_sgs_crank_mat
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_temp_sgs_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_temp_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_sgs_mat11(n_int, idx_4_fl_mat, coef_imp_t,     &
     &    ifilter_final, ak_diff(1,iak_diff_t), ak_d_temp,              &
     &    num_temp_comp, aiccg_temp)
!
      end subroutine int_vol_temp_sgs_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_composit_sgs_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_light_element_matrix
!
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_sgs_mat11(n_int, idx_4_fl_mat, coef_imp_c,     &
     &    ifilter_final, ak_diff(1,iak_diff_c), ak_d_composit,          &
     &    num_composit_comp, aiccg_composit)
!
      end subroutine int_vol_composit_sgs_crank_mat
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_sgs_mat
