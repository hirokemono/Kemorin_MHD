!int_vol_poisson_phys_mat.f90
!     module int_vol_poisson_phys_mat
!
! numerical integration for finite elememt equations(Poisson's equation)
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2003 (ver 1.1)
!        modifired by H. Matsui on June, 2005
!        modifired by H. Matsui on Nov., 2007
!
!      subroutine int_vol_velo_poisson_mat(n_int)
!      subroutine int_vol_magne_poisson_mat(n_int)
!
!      subroutine int_vol_velo_crank_mat(n_int)
!      subroutine int_vol_magne_crank_mat(n_int)
!      subroutine int_vol_vecp_crank_mat(n_int)
!
!      subroutine int_vol_temp_crank_mat(n_int)
!      subroutine int_vol_composit_crank_mat(n_int)
!
      module int_vol_poisson_phys_mat
!
      use m_precision
!
      use m_geometry_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_velo_poisson_mat(n_int)
!
      use m_sorted_node_MHD
      use m_velo_matrix
      use int_vol_poisson_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_mat11                                        &
     &   (ele1, jac1_3d_l, rhs_tbl1, mat_tbl_fl_l,                      &
     &    n_int, fem1_wk, Pmat_DJDS)
!
      end subroutine int_vol_velo_poisson_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_magne_poisson_mat(n_int)
!
      use m_sorted_node_MHD
      use m_magne_matrix
      use int_vol_poisson_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_mat11                                        &
     &   (ele1, jac1_3d_l, rhs_tbl1, mat_tbl_l1,                        &
     &    n_int, fem1_wk, Fmat_DJDS)
!
      end subroutine int_vol_magne_poisson_mat
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_velo_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_velo_matrix
      use int_vol_poisson_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_diffuse_mat33                                        &
     &   (ele1, jac1_3d_q, rhs_tbl1, mat_tbl_fl_q,                      &
     &    n_int, coef_imp_v, ak_d_velo, fem1_wk, Vmat_DJDS)
!
      end subroutine int_vol_velo_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_magne_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_magne_matrix
      use int_vol_poisson_mat
!
      integer(kind = kint), intent(in) :: n_int
!
      call int_vol_diffuse_mat33                                        &
     &   (ele1, jac1_3d_q, rhs_tbl1, mat_tbl_full_cd_q,                 &
     &    n_int, coef_imp_b, ak_d_magne, fem1_wk, Bmat_DJDS)
!
      end subroutine int_vol_magne_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_vecp_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_element_id_4_node
      use m_ele_material_property
      use m_magne_matrix
      use int_vol_poisson_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_diffuse_mat33                                        &
     &   (ele1, jac1_3d_q, rhs_tbl1, mat_tbl_q1, n_int,                 &
     &    coef_imp_b, ak_d_magne, fem1_wk, Bmat_DJDS)
!
      end subroutine int_vol_vecp_crank_mat
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_temp_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_temp_matrix
      use int_vol_poisson_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_diffuse_mat11                                        &
     &   (ele1, jac1_3d_q, rhs_tbl1, mat_tbl_fl_q,                      &
     &    n_int, coef_imp_t, ak_d_temp, fem1_wk, Tmat_DJDS)
!
      end subroutine int_vol_temp_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_composit_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_light_element_matrix
!
      use int_vol_poisson_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_diffuse_mat11                                        &
     &   (ele1, jac1_3d_q, rhs_tbl1, mat_tbl_fl_q,                      &
     &    n_int, coef_imp_c, ak_d_composit, fem1_wk, Cmat_DJDS)
!
      end subroutine int_vol_composit_crank_mat
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_phys_mat
