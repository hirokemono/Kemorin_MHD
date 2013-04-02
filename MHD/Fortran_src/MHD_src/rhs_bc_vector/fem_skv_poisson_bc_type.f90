!fem_skv_poisson_bc_type.f90
!     module fem_skv_poisson_bc_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine fem_skv_poisson_bc_t(num_index_ibc, ele_bc_id,        &
!     &          ibc_stack_smp, k2, n_int, ele, jac_3d_l, fem_wk)
!
!      subroutine fem_skv_scalar_diffuse_bc_t(num_index_ibc, ele_bc_id, &
!     &          ibc_stack_smp, k2, n_int, ak_d, ele, jac_3d, fem_wk)
!      subroutine fem_skv_vector_diffuse_bc_t(num_index_ibc, ele_bc_id, &
!     &         ibc_stack_smp, k2, nd, n_int, ak_d, ele, jac_3d, fem_wk)
!
      module fem_skv_poisson_bc_type
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_jacobians
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_bc_t(num_index_ibc, ele_bc_id,         &
     &          ibc_stack_smp, k2, n_int, ele, jac_3d_l, fem_wk)
!
      use fem_skv_poisson_bc
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer (kind=kint), intent(in) :: n_int, k2
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_poisson_fixed                                        &
     &   (ele%numele, num_t_linear, num_t_linear, np_smp,               &
     &    num_index_ibc, ele_bc_id, ibc_stack_smp, k2, n_int,           &
     &    jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%dnx, jac_3d_l%dnx, &
     &    fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_poisson_bc_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_diffuse_bc_t(num_index_ibc, ele_bc_id,  &
     &          ibc_stack_smp, k2, n_int, ak_d, ele, jac_3d, fem_wk)
!
      use fem_skv_poisson_bc
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer (kind=kint), intent(in) :: n_int, k2
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_diffuse_fixed                                 &
     &       (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,       &
     &        num_index_ibc, ele_bc_id, ibc_stack_smp, k2, ione, n_int, &
     &        jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,     &
     &        ak_d, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_scalar_diffuse_bc_t
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_diffuse_bc_t(num_index_ibc, ele_bc_id,  &
     &         ibc_stack_smp, k2, nd, n_int, ak_d, ele, jac_3d, fem_wk)
!
      use fem_skv_poisson_bc
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer (kind=kint), intent(in) :: n_int, k2, nd
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_diffuse_fixed                                 &
     &       (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,       &
     &        num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int,   &
     &        jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,     &
     &        ak_d, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_vector_diffuse_bc_t
!
!-----------------------------------------------------------------------
!
      end module fem_skv_poisson_bc_type
