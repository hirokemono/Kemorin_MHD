!
!      module cal_3d_filter_4_each_node
!
!        programmed by H.Matsui on Mar., 2008
!
!!      subroutine const_filter_mat_each_nod                            &
!!     &         (ref_filter_width, node, FEM_elen, ref_m,              &
!!     &          fil_coef, inod, num_fixed_point, fil_mat, ierr)
!!        type(node_data), intent(in) :: node
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(reference_moments), intent(in) :: ref_m
!!        type(each_filter_coef), intent(in) :: fil_coef
!!      subroutine cal_filter_and_coefficients(num_int_points,          &
!!     &         (ele, g_FEM, jac_3d, fil_mat, fil_coef)
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(matrix_4_filter), intent(in) :: fil_mat
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!      subroutine cal_rms_filter_coefs(fil_coef, rms_weight, rms_filter)
!
      module cal_3d_filter_4_each_node
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_filter_coefs
      use t_matrix_4_filter
!
      implicit none
!
      private :: set_constant_filter_coefs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_filter_mat_each_nod                              &
     &         (ref_filter_width, node, FEM_elen, ref_m,                &
     &          fil_coef, inod, num_fixed_point, fil_mat, ierr)
!
      use t_geometry_data
      use t_filter_elength
      use t_reference_moments
!
      use copy_moments_2_matrix
      use fem_const_filter_matrix
      use modify_matrix_and_rhs
!
      real(kind = kreal), intent(in) :: ref_filter_width
      type(node_data), intent(in) :: node
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(each_filter_coef), intent(in) :: fil_coef
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint), intent(in) :: num_fixed_point
!
      type(matrix_4_filter), intent(inout) :: fil_mat
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
!
      if ( num_fixed_point .gt. 0 ) then
        call set_constant_filter_coefs                                 &
     &     (ref_filter_width, node, FEM_elen, fil_coef,                &
     &      inod, num_fixed_point, fil_mat)
      end if
!
      call set_filter_moments_on_node(inod, ref_m, num_fixed_point,     &
     &    fil_mat%max_mat_size, fil_mat%mat_size, fil_mat%vec_mat)
      call copy_2_filter_matrix(num_fixed_point, fil_mat%max_mat_size,  &
     &   fil_mat%num_work, fil_mat%mat_work, fil_mat%a_mat)
!      call substitute_fixed_moments(num_fixed_point,                   &
!    &     fil_mat%max_mat_size, fil_mat%vec_mat, fil_mat%a_mat)
!
      call swap_matrix_by_diag_size(fil_mat%mat_size,                   &
     &    fil_mat%max_mat_size, fil_mat%vec_mat, fil_mat%a_mat)
      call check_diagonal(fil_mat%mat_size, fil_mat%max_mat_size,       &
     &    fil_mat%a_mat, ierr)
      if (ierr .eq. 1) return
!
      call scaling_by_diagonal(fil_mat%mat_size, fil_mat%max_mat_size,  &
     &    fil_mat%vec_mat, fil_mat%a_mat)
!
      end subroutine const_filter_mat_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_and_coefficients(num_int_points,            &
     &          ele, g_FEM, jac_3d, fil_mat, fil_coef)
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use copy_moments_2_matrix
      use int_filter_functions
!
      integer(kind = kint) :: num_int_points
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(matrix_4_filter), intent(in) :: fil_mat
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      fil_coef%weight_1nod = 0.0d0
      call copy_filter_coefs(fil_mat, fil_coef)
!
      call int_node_filter_weights                                      &
     &   (ele, g_FEM, jac_3d, fil_coef, num_int_points)
!
      end subroutine cal_filter_and_coefficients
!
!-----------------------------------------------------------------------
!
      subroutine cal_rms_filter_coefs(fil_coef, rms_weight, ierr2)
!
      type(each_filter_coef), intent(inout) :: fil_coef
      real(kind = kreal), intent(inout) :: rms_weight
      integer(kind = kint), intent(inout) :: ierr2
      integer(kind = kint) :: i
!
!
      rms_weight = 0.0d0
      do i = 1, fil_coef%nnod_4_1nod_w
        rms_weight = rms_weight + fil_coef%weight_1nod(i)**2
      end do
!
      if(fil_coef%weight_1nod(1) .lt. 0.0d0) then
        ierr2 = 1000
      else
        ierr2 = 0
      end if
!
      if(fil_coef%filter_1nod(1) .le. 0.0d0) then
        ierr2 = -1
      else
        ierr2 = 0
      end if
!
      end subroutine cal_rms_filter_coefs
!
!-----------------------------------------------------------------------
!
      subroutine set_constant_filter_coefs(ref_filter_width,            &
     &          node, FEM_elen, fil_coef, inod, num_fixed, fil_mat)
!
      use t_geometry_data
      use t_filter_elength
!
      use cal_gaussian_at_node
!
      real(kind = kreal), intent(in) :: ref_filter_width
      type(node_data), intent(in) :: node
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(each_filter_coef), intent(in) :: fil_coef
      integer(kind = kint), intent(in) :: inod, num_fixed
!
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      real(kind = kreal) :: g
      integer(kind = kint) :: i, j, jnod
!
!
      fil_mat%vec_mat = 0.0d0
      fil_mat%a_mat = 0.0d0
      do i = 1, num_fixed
        j = i
!        j = mat_size-i+1
        jnod = fil_coef%inod_4_1nod_w(i)
        call s_cal_gaussian_at_node(ref_filter_width,                   &
     &      node%xx(jnod,1), node%xx(jnod,2), node%xx(jnod,3),          &
     &      node%xx(inod,1), node%xx(inod,2), node%xx(inod,3),          &
     &      FEM_elen%elen_nod%moms%f_x2(inod),                          &
     &      FEM_elen%elen_nod%moms%f_y2(inod),                          &
     &      FEM_elen%elen_nod%moms%f_z2(inod), g)
!
        fil_mat%vec_mat(i) = g
        fil_mat%a_mat(i,j) = one
      end do
!
      end subroutine set_constant_filter_coefs
!
! ----------------------------------------------------------------------
!
      end module cal_3d_filter_4_each_node
