!cal_filter_moments_again.f90
!      module cal_filter_moments_again
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine s_cal_filter_moments_again                           &
!!     &         (gfil_p, node, ele, g_FEM, jac_3d, FEM_elen, ref_m,    &
!!     &          inod, ele_4_nod, neib_nod, mom_nod, fil_coef, fil_mat)
!!       type(ctl_params_4_gen_filter), intent(in) :: gfil_p
!!       type(node_data), intent(in) :: node
!!       type(element_data), intent(in) :: ele
!!       type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!       type(jacobians_3d), intent(in) :: jac_3d
!!       type(gradient_model_data_type), intent(in) :: FEM_elen
!!       type(reference_moments), intent(in) :: ref_m
!!       type(element_around_node), intent(inout) :: ele_4_nod
!!       type(next_nod_id_4_nod), intent(inout) :: neib_nod
!!       type(nod_mom_diffs_type), intent(inout) :: mom_nod
!!       type(each_filter_coef), intent(inout) :: fil_coef
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!
      module cal_filter_moments_again
!
      use m_precision
!
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_filter_moments_again                             &
     &         (gfil_p, node, ele, g_FEM, jac_3d, FEM_elen, ref_m,      &
     &          inod, ele_4_nod, neib_nod, mom_nod, fil_coef, fil_mat)
!
      use m_crs_matrix_4_filter
!
      use t_filter_elength
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_next_node_ele_4_node
      use t_filter_moments
      use t_reference_moments
      use t_filter_coefs
      use t_matrix_4_filter
      use t_ctl_params_4_gen_filter
!
      use expand_filter_area_4_1node
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use set_simple_filters
!
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
!
      integer(kind = kint), intent(in) :: inod
!
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(each_filter_coef), intent(inout) :: fil_coef
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: num_fixed_point = 0
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each                                     &
     &   (inod, node%numnod, ele_4_nod, neib_nod, fil_coef)
      call resize_matrix_size_gen_filter(ele%nnod_4_ele,                &
     &    fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
!
      do i = 1, gfil_p%maximum_neighbour
        call s_expand_filter_area_4_1node                               &
     &     (inod, gfil_p, node, ele, ele_4_nod, FEM_elen, fil_coef)

        call resize_matrix_size_gen_filter(ele%nnod_4_ele,              &
     &      fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
      end do
      fil_mat%mat_size = fil_coef%nnod_4_1nod_w
!
!    set nxn matrix
!
      call int_node_filter_matrix(node, ele, g_FEM, jac_3d, ref_m,      &
     &    fil_coef, inod, gfil_p%num_int_points, fil_mat)
!
      call copy_2_filter_matrix(num_fixed_point, fil_mat%max_mat_size,  &
     &   fil_mat%num_work, fil_mat%mat_work, fil_mat%a_mat)
!
!      set filter function without normalization
!
      fil_coef%nnod_near_nod_w(inod) = fil_coef%nnod_4_1nod_w
      call cal_filter_moms_each_nod_type                                &
     &    (inod, ref_m, fil_coef, fil_mat, mom_nod)
!
      end subroutine s_cal_filter_moments_again
!
! -----------------------------------------------------------------------
!
      end module cal_filter_moments_again
