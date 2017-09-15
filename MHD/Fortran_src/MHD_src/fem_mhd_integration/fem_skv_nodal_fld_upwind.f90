!fem_skv_nodal_fld_upwind.f90
!      module fem_skv_nodal_fld_upwind
!
!     programmed by H.Matsui on May 2012
!
!!      subroutine fem_skv_scalar_field_upwind                          &
!!     &         (iele_fsmp_stack, n_int, k2, dt, vxe,                  &
!!     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!!      subroutine fem_skv_vector_field_upwind                          &
!!     &         (iele_fsmp_stack,  n_int, k2, dt, vxe,                 &
!!     &         ele, g_FEM, jac_3d, vector_1, sk_v)
!!      subroutine fem_skv_tensor_field_upwind                          &
!!     &         (iele_fsmp_stack, n_int, k2, dt, vxe,                  &
!!     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!
      module fem_skv_nodal_fld_upwind
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_field_upwind                            &
     &         (iele_fsmp_stack, n_int, k2, dt, vxe,                    &
     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!
      use fem_skv_nodal_field_upw
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: scalar_1(ele%numele)
!
      real(kind=kreal), intent(inout)                                   &
     &           :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_field_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, k2, jac_3d%xjac,         &
     &    jac_3d%an, jac_3d%an, jac_3d%dnx, dt, vxe, scalar_1, sk_v) 
!
      end subroutine fem_skv_scalar_field_upwind
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_vector_field_upwind                            &
     &         (iele_fsmp_stack,  n_int, k2, dt, vxe,                   &
     &         ele, g_FEM, jac_3d, vector_1, sk_v)
!
      use fem_skv_nodal_field_upw
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: vector_1(ele%numele,n_vector)
!
      real(kind=kreal), intent(inout)                                   &
     &           :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_field_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, k2, jac_3d%xjac,         &
     &    jac_3d%an, jac_3d%an, jac_3d%dnx, dt, vxe, vector_1, sk_v) 
!
      end subroutine fem_skv_vector_field_upwind
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_field_upwind                            &
     &         (iele_fsmp_stack, n_int, k2, dt, vxe,                    &
     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!
      use fem_skv_nodal_field_upw
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in)                                      &
     &                   :: tensor_1(ele%numele,n_sym_tensor)
!
      real(kind=kreal), intent(inout)                                   &
     &           :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_tensor_field_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, k2, jac_3d%xjac,         &
     &    jac_3d%an, jac_3d%an, jac_3d%dnx, dt, vxe, tensor_1, sk_v) 
!
      end subroutine fem_skv_tensor_field_upwind
!
! ----------------------------------------------------------------------
!
      end module fem_skv_nodal_fld_upwind
