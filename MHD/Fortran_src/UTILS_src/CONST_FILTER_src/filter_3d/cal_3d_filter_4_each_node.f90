!
!      module cal_3d_filter_4_each_node
!
!        programmed by H.Matsui on Mar., 2008
!
!      subroutine const_filter_mat_each_nod                             &
!     &         (node, FEM_elen, inod, num_fixed_point, ierr)
!      subroutine cal_filter_and_coefficients(ele, g_FEM, jac_3d)
!      subroutine cal_rms_filter_coefs(rms_weight, rms_filter)
!
      module cal_3d_filter_4_each_node
!
      use m_precision
      use m_constants
!
      use calypso_mpi
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
     &         (node, FEM_elen, inod, num_fixed_point, ierr)
!
      use t_geometry_data
      use t_filter_elength
!
      use m_matrix_4_filter
      use copy_moments_2_matrix
      use fem_const_filter_matrix
      use modify_matrix_and_rhs
!
      type(node_data), intent(in) :: node
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint), intent(in) :: num_fixed_point
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
!
      if ( num_fixed_point .gt. 0 ) then
        call set_constant_filter_coefs(node, FEM_elen,                  &
     &      inod, num_fixed_point)
      end if
!
      call set_filter_moments_on_node(inod, num_fixed_point )
      call copy_2_filter_matrix( num_fixed_point )
!      call substitute_fixed_moments( num_fixed_point )
!
      call swap_matrix_by_diag_size(mat_size, max_mat_size,             &
     &         vec_mat, a_mat)
      call check_diagonal(mat_size, max_mat_size, a_mat, ierr)
      if (ierr .eq. 1) return
!
      call scaling_by_diagonal(mat_size, max_mat_size, vec_mat, a_mat)
!
      end subroutine const_filter_mat_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_and_coefficients(ele, g_FEM, jac_3d)
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use copy_moments_2_matrix
      use int_filter_functions
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      weight_1nod = 0.0d0
      call copy_filter_coefs(nnod_near_1nod_weight)
!
      call int_node_filter_weights(ele, g_FEM, jac_3d, num_int_points,  &
     &    nele_near_1nod_weight, iele_near_1nod_weight(1) )
!
      end subroutine cal_filter_and_coefficients
!
!-----------------------------------------------------------------------
!
      subroutine cal_rms_filter_coefs(rms_weight, ierr2)
!
      use m_filter_coefs
!
      real(kind = kreal), intent(inout) :: rms_weight
      integer(kind = kint), intent(inout) :: ierr2
      integer(kind = kint) :: i
!
!
      rms_weight = 0.0d0
      do i = 1, nnod_near_1nod_weight
        rms_weight = rms_weight + weight_1nod(i)**2
      end do
!
      if (weight_1nod(1) .lt. 0.0d0) then
        ierr2 = 1000
      else
        ierr2 = 0
      end if
!
      if (filter_1nod(1) .le. 0.0d0) then
        ierr2 = -1
      else
        ierr2 = 0
      end if
!
      end subroutine cal_rms_filter_coefs
!
!-----------------------------------------------------------------------
!
      subroutine set_constant_filter_coefs                              &
     &         (node, FEM_elen, inod, num_fixed)
!
      use t_geometry_data
      use t_filter_elength
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
      use cal_gaussian_at_node
!
      type(node_data), intent(in) :: node
      type(gradient_model_data_type), intent(in) :: FEM_elen
      integer(kind = kint), intent(in) :: inod, num_fixed
!
      real(kind = kreal) :: g
      integer(kind = kint) :: i, j, jnod
!
!
      vec_mat = 0.0d0
      a_mat = 0.0d0
      do i = 1, num_fixed
        j = i
!        j = mat_size-i+1
        jnod = inod_near_1nod_weight(i)
        call s_cal_gaussian_at_node(ref_filter_width(1),                &
     &      node%xx(jnod,1), node%xx(jnod,2), node%xx(jnod,3),          &
     &      node%xx(inod,1), node%xx(inod,2), node%xx(inod,3),          &
     &      FEM_elen%elen_nod%moms%f_x2(inod),                          &
     &      FEM_elen%elen_nod%moms%f_y2(inod),                          &
     &      FEM_elen%elen_nod%moms%f_z2(inod), g)
!
        vec_mat(i) = g
        a_mat(i,j) = one
      end do
!
      end subroutine set_constant_filter_coefs
!
! ----------------------------------------------------------------------
!
      end module cal_3d_filter_4_each_node
