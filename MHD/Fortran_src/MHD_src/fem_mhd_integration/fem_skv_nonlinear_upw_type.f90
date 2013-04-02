!fem_skv_nonlinear_upw_type.f90
!      module fem_skv_nonlinear_upw_type
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_scalar_inertia_upw_type(iele_fsmp_stack,      &
!     &          n_int, k2, vxe, vxe_up, ele, jac_3d, fem_wk)
!      subroutine fem_skv_vector_inertia_upw_type(iele_fsmp_stack,      &
!     &          n_int, k2, vxe, vxe_up, ele, jac_3d, fem_wk)
!      subroutine fem_skv_rot_inertia_upw_type(iele_fsmp_stack,         &
!     &          n_int, k2, wxe, vxe_up, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_coriolis_upw_type(iele_fsmp_stack,            &
!     &          n_int, k2, angular, vxe_up, ele, jac_3d, fem_wk)
!
      module fem_skv_nonlinear_upw_type
!
      use m_precision
!
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
      use m_machine_parameter
      use m_t_int_parameter
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_inertia_upw_type(iele_fsmp_stack,       &
     &          n_int, k2, vxe, vxe_up, ele, jac_3d, fem_wk)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_inertia_upw                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    fem_wk%scalar_1, vxe, vxe_up, fem_wk%sk6)
!
      end subroutine fem_skv_scalar_inertia_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_inertia_upw_type(iele_fsmp_stack,       &
     &          n_int, k2, vxe, vxe_up, ele, jac_3d, fem_wk)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_inertia_upw                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    fem_wk%vector_1, vxe, vxe_up, fem_wk%sk6)
!
      end subroutine fem_skv_vector_inertia_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_inertia_upw_type(iele_fsmp_stack,          &
     &          n_int, k2, wxe, vxe_up, ele, jac_3d, fem_wk)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: wxe(ele%numele,3)
      real (kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_inertia_rot_upw                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%an,                &
     &    fem_wk%vector_1, wxe, vxe_up, fem_wk%sk6)
!
      end subroutine fem_skv_rot_inertia_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_coriolis_upw_type(iele_fsmp_stack,             &
     &          n_int, k2, angular, vxe_up, ele, jac_3d, fem_wk)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: angular(3)
      real (kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_coriolis_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%an,                &
     &    fem_wk%vector_1, angular, vxe_up, fem_wk%sk6)
!
      end subroutine fem_skv_coriolis_upw_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_nonlinear_upw_type
