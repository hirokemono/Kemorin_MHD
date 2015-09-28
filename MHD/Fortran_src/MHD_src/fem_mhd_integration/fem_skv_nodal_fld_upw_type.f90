!fem_skv_nodal_fld_upw_type.f90
!      module fem_skv_nodal_fld_upw_type
!
!     programmed by H.Matsui on May 2012
!
!      subroutine fem_skv_scalar_field_upwind(iele_fsmp_stack,          &
!     &          n_int, k2, vxe, ele, jac_3d, scalar_1, sk_v) 
!      subroutine fem_skv_vector_field_upwind(iele_fsmp_stack,          &
!     &          n_int, k2, vxe, ele, jac_3d, vector_1, sk_v) 
!      subroutine fem_skv_tensor_field_upwind(iele_fsmp_stack,          &
!     &          n_int, k2, vxe, ele, jac_3d, tensor_1, sk_v) 
!
      module fem_skv_nodal_fld_upw_type
!
      use m_precision
!
      use t_geometry_data
      use t_jacobians
      use m_machine_parameter
      use m_phys_constants
      use m_fem_gauss_int_coefs
      use m_t_int_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_field_upwind(iele_fsmp_stack,           &
     &          n_int, k2, vxe, ele, jac_3d, scalar_1, sk_v) 
!
      use fem_skv_nodal_field_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &           :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_field_upw                                     &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      jac_3d%ntot_int, iele_fsmp_stack, n_int, k2,                &
     &      jac_3d%xjac, jac_3d%an, jac_3d%an, jac_3d%dnx,              &
     &      dt, vxe, scalar_1, sk_v) 
!
      end subroutine fem_skv_scalar_field_upwind
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_vector_field_upwind(iele_fsmp_stack,           &
     &          n_int, k2, vxe, ele, jac_3d, vector_1, sk_v) 
!
      use fem_skv_nodal_field_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: vector_1(ele%numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &           :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_field_upw                                     &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      jac_3d%ntot_int, iele_fsmp_stack, n_int, k2,                &
     &      jac_3d%xjac, jac_3d%an, jac_3d%an, jac_3d%dnx,              &
     &      dt, vxe, vector_1, sk_v) 
!
      end subroutine fem_skv_vector_field_upwind
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_field_upwind(iele_fsmp_stack,           &
     &          n_int, k2, vxe, ele, jac_3d, tensor_1, sk_v) 
!
      use fem_skv_nodal_field_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in)                                     &
     &                   :: tensor_1(ele%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &           :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_tensor_field_upw                                     &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      jac_3d%ntot_int, iele_fsmp_stack, n_int, k2,                &
     &      jac_3d%xjac, jac_3d%an, jac_3d%an, jac_3d%dnx,              &
     &      dt, vxe, tensor_1, sk_v) 
!
      end subroutine fem_skv_tensor_field_upwind
!
! ----------------------------------------------------------------------
!
      end module fem_skv_nodal_fld_upw_type
