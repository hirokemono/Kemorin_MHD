!
!     module cal_fields_on_ele_type
!
!     Written by H. Matsui on Oct., 2005
!
!      subroutine scalar_on_ele_type(mesh, jac_3d, iele_fsmp_stack,     &
!     &          n_int, d_ele, d_nod)
!      subroutine vector_on_ele_type(mesh, jac_3d, iele_fsmp_stack,     &
!     &          n_int, d_ele, d_nod)
!      subroutine sym_tensor_on_ele_type(mesh, jac_3d, iele_fsmp_stack, &
!     &          n_int, d_ele, d_nod)
!
!      subroutine scalar_grp_on_ele_type(mesh, jac_3d, iele_fsmp_stack, &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine vector_grp_on_ele_type(mesh, jac_3d, iele_fsmp_stack, &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine sym_tensor_grp_on_ele_type(mesh, jac_3d,              &
!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,            &
!     &          d_ele, d_nod)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d
!
      module cal_fields_on_ele_type
!
      use m_precision
!
      use t_mesh_data
      use t_jacobian_3d
!
      implicit none
!
! ----------------------------------------------------------------------
!
     contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_on_ele_type(mesh, jac_3d, iele_fsmp_stack,      &
     &          n_int, d_ele, d_nod)
!
      use fem_fields_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele)
!
!
      call fem_scalar_on_element(iele_fsmp_stack,                       &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac, d_ele, d_nod)
!
      end subroutine scalar_on_ele_type
!
! ----------------------------------------------------------------------
!
      subroutine vector_on_ele_type(mesh, jac_3d, iele_fsmp_stack,      &
     &          n_int, d_ele, d_nod)
!
      use fem_fields_on_element
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
      call fem_vector_on_element(iele_fsmp_stack,                       &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac, d_ele, d_nod)
!
      end subroutine vector_on_ele_type
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_on_ele_type(mesh, jac_3d, iele_fsmp_stack,  &
     &          n_int, d_ele, d_nod)
!
      use fem_fields_on_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,6)
!
!
      call fem_sym_tensor_on_element(iele_fsmp_stack,                   &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, jac_3d%ntot_int, n_int,      &
     &    jac_3d%dnx, jac_3d%xjac, d_ele, d_nod)
!
      end subroutine sym_tensor_on_ele_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_grp_on_ele_type(mesh, jac_3d, iele_fsmp_stack,  &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use fem_fields_on_element
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
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele)
!
      call fem_scalar_grp_on_element(iele_fsmp_stack,                   &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine scalar_grp_on_ele_type
!
! ----------------------------------------------------------------------
!
      subroutine vector_grp_on_ele_type(mesh, jac_3d, iele_fsmp_stack,  &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use fem_fields_on_element
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
      call fem_vector_grp_on_element(iele_fsmp_stack,                   &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine vector_grp_on_ele_type
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_grp_on_ele_type(mesh, jac_3d,               &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_ele, d_nod)
!
      use fem_fields_on_element
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
      real(kind = kreal), intent(inout) :: d_ele(mesh%ele%numele,6)
!
!
      call fem_sym_tensor_grp_on_element(iele_fsmp_stack,               &
     &    mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, mesh%ele%a_vol_ele, nele_grp, iele_grp,          &
     &    jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac,              &
     &    d_ele, d_nod)
!
      end subroutine sym_tensor_grp_on_ele_type
!
! ----------------------------------------------------------------------
!
      end module cal_fields_on_ele_type
