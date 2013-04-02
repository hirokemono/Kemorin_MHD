!cal_differences_on_ele_type.f90
!     module cal_differences_on_ele_type
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine difference_on_element_type(mesh, jac_3d,              &
!     &          iele_fsmp_stack, n_int, nd, d_ele, d_nod)
!      subroutine gradient_on_element_type(mesh, jac_3d,                &
!     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!      subroutine divergence_on_element_type(mesh, jac_3d,              &
!     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!      subroutine rotation_on_element_type(mesh, jac_3d,                &
!     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!      subroutine div_sym_tensor_on_ele_type(mesh, jac_3d,              &
!     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!      subroutine div_asym_tensor_on_ele_type(mesh, jac_3d,             &
!     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!
!      subroutine difference_grp_on_element_type(mesh, jac_3d,          &
!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int, nd,        &
!     &           d_ele, d_nod)
!      subroutine gradient_grp_on_element_type(mesh, jac_3d,            &
!     &          iele_fsmp_stack,  nele_grp, iele_grp, n_int,           &
!     &          d_ele, d_nod)
!      subroutine divergence_grp_on_element_type(mesh, jac_3d,          &
!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,            &
!     &          d_ele, d_nod)
!      subroutine rotation_grp_on_element_type(mesh, jac_3d,            &
!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,            &
!     &          d_ele, d_nod)
!      subroutine div_sym_tensor_grp_on_ele_type(mesh, jac_3d,          &
!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,            &
!     &          d_ele, d_nod)
!      subroutine div_asym_tensor_grp_on_ele_type(mesh, jac_3d,         &
!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,            &
!     &          d_ele, d_nod)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d
!
!
      module cal_differences_on_ele_type
!
      use m_precision
!
      use t_mesh_data
      use t_jacobian_3d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine difference_on_element_type(mesh, jac_3d,               &
     &          iele_fsmp_stack, n_int, nd, d_ele, d_nod)
!
      use cal_difference_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int, nd
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele)
!
!
      call fem_difference_on_element(iele_fsmp_stack,                   &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac, nd, d_ele, d_nod)
!
      end subroutine difference_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine gradient_on_element_type(mesh, jac_3d,                 &
     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!
      use cal_gradient_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
!
      call fem_gradient_on_element(iele_fsmp_stack,                     &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac, d_ele, d_nod)
!
      end subroutine gradient_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine divergence_on_element_type(mesh, jac_3d,               &
     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!
      use cal_divergence_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele)
!
      call fem_divergence_on_element(iele_fsmp_stack,                   &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac, d_ele, d_nod)
!
      end subroutine divergence_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine rotation_on_element_type(mesh, jac_3d,                 &
     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!
      use cal_rotation_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
!
      call fem_rotation_on_element(iele_fsmp_stack,                     &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele,                              &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine rotation_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine div_sym_tensor_on_ele_type(mesh, jac_3d,               &
     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!
      use div_s_tensor_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) ::  n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
!
      call fem_div_sym_tensor_on_ele(iele_fsmp_stack,                   &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac, d_ele, d_nod)
!
      end subroutine div_sym_tensor_on_ele_type
!
! ----------------------------------------------------------------------
!
      subroutine div_asym_tensor_on_ele_type(mesh, jac_3d,              &
     &          iele_fsmp_stack, n_int, d_ele, d_nod)
!
      use div_as_tensor_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
      call fem_div_asym_tensor_on_ele(iele_fsmp_stack,                  &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac,  d_ele, d_nod)
!
      end subroutine div_asym_tensor_on_ele_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine difference_grp_on_element_type(mesh, jac_3d,           &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int, nd,         &
     &           d_ele, d_nod)
!
      use cal_difference_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int, nd
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele)
!
!
      call fem_difference_grp_on_element(iele_fsmp_stack,               &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, nd,          &
     &    d_ele, d_nod)
!
      end subroutine difference_grp_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine gradient_grp_on_element_type(mesh, jac_3d,             &
     &          iele_fsmp_stack,  nele_grp, iele_grp, n_int,            &
     &          d_ele, d_nod)
!
      use cal_gradient_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
!
      call fem_gradient_grp_on_element(iele_fsmp_stack,                 &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine gradient_grp_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine divergence_grp_on_element_type(mesh, jac_3d,           &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_ele, d_nod)
!
      use cal_divergence_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele)
!
      call fem_divergence_grp_on_element(iele_fsmp_stack,               &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine divergence_grp_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine rotation_grp_on_element_type(mesh, jac_3d,             &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_ele, d_nod)
!
      use cal_rotation_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
!
      call fem_rotation_grp_on_element(iele_fsmp_stack,                 &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine rotation_grp_on_element_type
!
! ----------------------------------------------------------------------
!
      subroutine div_sym_tensor_grp_on_ele_type(mesh, jac_3d,           &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_ele, d_nod)
!
      use div_s_tensor_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
!
      call fem_div_sym_tensor_grp_on_ele(iele_fsmp_stack,               &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine div_sym_tensor_grp_on_ele_type
!
! ----------------------------------------------------------------------
!
      subroutine div_asym_tensor_grp_on_ele_type(mesh, jac_3d,          &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_ele, d_nod)
!
      use div_as_tensor_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,3)
!
      call fem_div_asym_tensor_grp_on_ele(iele_fsmp_stack,              &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine div_asym_tensor_grp_on_ele_type
!
! ----------------------------------------------------------------------
!
      end module cal_differences_on_ele_type
