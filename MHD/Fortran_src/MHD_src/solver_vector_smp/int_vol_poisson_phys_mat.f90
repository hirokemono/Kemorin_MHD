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
      use int_vol_poisson_mat_1st
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_mat11(n_int, idx_4_fll_mat,                  &
     &    Pmat_DJDS%num_non0, Pmat_DJDS%aiccg)
!
      end subroutine int_vol_velo_poisson_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_magne_poisson_mat(n_int)
!
      use m_sorted_node_MHD
      use m_magne_matrix
      use int_vol_poisson_mat_1st
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_poisson_mat11(n_int, idx_4_l_mat,                    &
     &    Fmat_DJDS%num_non0, Fmat_DJDS%aiccg)
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
      use int_vol_poisson_mat_1st
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_mat33(n_int, idx_4_fl_mat, coef_imp_v,         &
     &    ak_d_velo, Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
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
      use int_vol_poisson_mat_1st
!
      integer(kind = kint), intent(in) :: n_int
!
      call int_vol_crank_mat33(n_int, idx_4_cd_mat_full, coef_imp_b,    &
     &    ak_d_magne, Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
!
      end subroutine int_vol_magne_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_vecp_crank_mat(n_int)
!
      use m_t_int_parameter
      use m_sorted_node
      use m_ele_material_property
      use m_magne_matrix
      use int_vol_poisson_mat_1st
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_mat33(n_int, idx_4_mat, coef_imp_b,            &
     &      ak_d_magne, Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
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
      use int_vol_poisson_mat_1st
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_mat11(n_int, idx_4_fl_mat, coef_imp_t,         &
     &    ak_d_temp, Tmat_DJDS%num_non0, Tmat_DJDS%aiccg)
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
      use int_vol_poisson_mat_1st
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_vol_crank_mat11(n_int, idx_4_fl_mat, coef_imp_c,         &
     &      ak_d_composit, Cmat_DJDS%num_non0, Cmat_DJDS%aiccg)
!
      end subroutine int_vol_composit_crank_mat
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_phys_mat
