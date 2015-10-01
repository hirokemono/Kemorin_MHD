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
      use m_velo_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_sgs_mat11(n_int, mat_tbl_fl_l%idx_4_mat,     &
     &    ifilter_final, ak_diff(1,iak_diff_v),                         &
     &    Pmat_DJDS%num_non0, Pmat_DJDS%aiccg)
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
      use m_magne_matrix
      use int_vol_poisson_sgs_matrix
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_sgs_mat11(n_int, mat_tbl_l1%idx_4_mat,       &
     &    ifilter_final, ak_diff(1,iak_diff_b),                         &
     &    Fmat_DJDS%num_non0, Fmat_DJDS%aiccg)
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
      call int_vol_crank_sgs_mat33(n_int, mat_tbl_fl_q%idx_4_mat,       &
     &    coef_imp_v, ifilter_final, ak_diff(1,iak_diff_v),             &
     &    ak_d_velo, Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
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
      call int_vol_crank_sgs_mat33(n_int, mat_tbl_full_cd_q%idx_4_mat,  &
     &    coef_imp_b, ifilter_final, ak_diff(1,iak_diff_b),             &
     &    ak_d_magne, Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
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
      call int_vol_crank_sgs_mat33                                      &
     &   (n_int, mat_tbl_q1%idx_4_mat, coef_imp_b,                      &
     &    ifilter_final, ak_diff(1,iak_diff_b), ak_d_magne,             &
     &    Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
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
      call int_vol_crank_sgs_mat11(n_int, mat_tbl_fl_q%idx_4_mat,       &
     &    coef_imp_t, ifilter_final, ak_diff(1,iak_diff_t),             &
     &    ak_d_temp, Tmat_DJDS%num_non0, Tmat_DJDS%aiccg)
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
      call int_vol_crank_sgs_mat11(n_int, mat_tbl_fl_q%idx_4_mat,       &
     &    coef_imp_c, ifilter_final, ak_diff(1,iak_diff_c),             &
     &    ak_d_composit, Cmat_DJDS%num_non0, Cmat_DJDS%aiccg)
!
      end subroutine int_vol_composit_sgs_crank_mat
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_sgs_mat
