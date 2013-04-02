!fem_skv_diffusion_type.f90
!      module fem_skv_diffusion_type
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_scalar_diffuse_type(iele_fsmp_stack,          &
!     &          n_int, k2, ak_d, ele, jac_3d, fem_wk)
!      subroutine fem_skv_vector_diffuse_type(iele_fsmp_stack,          &
!     &          n_int, k2, ak_d, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_poisson_type(iele_fsmp_stack,                 &
!     &          n_int, k2, ele, jac_3d, fem_wk)
!      subroutine fem_skv_poisson_linear_type(iele_fsmp_stack,          &
!     &          n_int, k2, ele, jac_3d, fem_wk)
!        integer(kind=kint), intent(in) :: n_int, k2
!        integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        real (kind=kreal), intent(in) :: ak_d(ele%numele)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
      module fem_skv_diffusion_type
!
      use m_precision
!
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
      use m_machine_parameter
      use m_geometry_constants
!
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_diffuse_type(iele_fsmp_stack,           &
     &          n_int, k2, ak_d, ele, jac_3d, fem_wk)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_diffuse                                       &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,          &
     &     iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                 &
     &     jac_3d%xjac, jac_3d%dnx, jac_3d%dnx, ak_d, fem_wk%scalar_1,  &
     &     fem_wk%sk6)
!
      end subroutine fem_skv_scalar_diffuse_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_diffuse_type(iele_fsmp_stack,           &
     &          n_int, k2, ak_d, ele, jac_3d, fem_wk)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_diffuse                                       &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,          &
     &     iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                 &
     &     jac_3d%xjac, jac_3d%dnx, jac_3d%dnx, ak_d, fem_wk%vector_1,  &
     &     fem_wk%sk6)
!
      end subroutine fem_skv_vector_diffuse_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_type(iele_fsmp_stack,                  &
     &          n_int, k2, ele, jac_3d, fem_wk)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_poisson                                              &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,          &
     &     iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                 &
     &     jac_3d%xjac, jac_3d%dnx, jac_3d%dnx, fem_wk%sk6)
!
      end subroutine fem_skv_poisson_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_linear_type(iele_fsmp_stack,           &
     &          n_int, k2, ele, jac_3d_l, fem_wk)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_poisson                                              &
     &    (ele%numele, num_t_linear, num_t_linear, np_smp,              &
     &     iele_fsmp_stack, n_int, k2, jac_3d_l%ntot_int,               &
     &     jac_3d_l%xjac, jac_3d_l%dnx, jac_3d_l%dnx, fem_wk%sk6)
!
      end subroutine fem_skv_poisson_linear_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffusion_type
