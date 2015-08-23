!
!     module const_jacobians_3d_type
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine cal_jacobian_type_trilinear(mesh, jac_3d)
!      subroutine cal_jacobian_type_triquad(mesh, jac_3d)
!      subroutine cal_jacobian_type_trilag(mesh, jac_3d)
!      subroutine cal_jacobian_type_triquad_on_l(mesh, jac_3d)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(inout) :: jac_3d
!
!      subroutine copy_shape_func_from_array(ntot_int_3d, nnod_4_ele,   &
!     &          an_org, an_tgt)
!
      module const_jacobians_3d_type
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_trilinear(mesh, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_linear
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, jac1_3d_l%an, jac_3d%an)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_8                                      &
     &       (mesh%node%numnod, mesh%ele%numele,                        &
     &        np_smp, mesh%ele%istack_ele_smp,                          &
     &        mesh%ele%ie(1:mesh%ele%numele,1:num_t_linear),            &
     &        mesh%node%xx, jac_3d%xjac(1:mesh%ele%numele,ix),          &
     &        jac_3d%axjac(1:mesh%ele%numele,ix),                       &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_linear,ix,1),        &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_linear,ix,2),        &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_linear,ix,3),        &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                &
     &        dnxi_1(1:num_t_linear,ix), dnei_1(1:num_t_linear,ix),     &
     &        dnzi_1(1:num_t_linear,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_type_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_triquad(mesh, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, aw, jac_3d%an)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_20                                     &
     &       (mesh%node%numnod, mesh%ele%numele, np_smp,                &
     &        mesh%ele%istack_ele_smp, mesh%ele%ie, mesh%node%xx,       &
     &        jac_3d%xjac(1:mesh%ele%numele,ix),                        &
     &        jac_3d%axjac(1:mesh%ele%numele,ix),                       &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_quad,ix,1),          &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_quad,ix,2),          &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_quad,ix,3),          &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                &
     &        dnxi_20(1:num_t_quad,ix), dnei_20(1:num_t_quad,ix),       &
     &        dnzi_20(1:num_t_quad,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_type_triquad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_trilag(mesh, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_lag
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, aw, jac_3d%an)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_27                                     &
     &       (mesh%node%numnod, mesh%ele%numele, np_smp,                &
     &        mesh%ele%istack_ele_smp, mesh%ele%ie, mesh%node%xx,       &
     &        jac_3d%xjac(1:mesh%ele%numele,ix),                        &
     &        jac_3d%axjac(1:mesh%ele%numele,ix),                       &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_lag,ix,1),           &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_lag,ix,2),           &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_lag,ix,3),           &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                &
     &        dnxi_27(1:num_t_lag,ix), dnei_27(1:num_t_lag,ix),         &
     &        dnzi_27(1:num_t_lag,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_type_trilag
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_triquad_on_l(mesh, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_linear_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, aw, jac_3d%an)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call cal_jacobian_3d_8_20                                     &
     &       (mesh%node%numnod, mesh%ele%numele, np_smp,                &
     &        mesh%ele%istack_ele_smp, mesh%ele%ie, mesh%node%xx,       &
     &        jac_3d%xjac(1:mesh%ele%numele,ix),                        &
     &        jac_3d%axjac(1:mesh%ele%numele,ix),                       &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_quad,ix,1),          &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_quad,ix,2),          &
     &        jac_3d%dnx(1:mesh%ele%numele,1:num_t_quad,ix,3),          &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                &
     &        jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                &
     &        dnxi_20(1:num_t_quad,ix), dnei_20(1:num_t_quad,ix),       &
     &        dnzi_20(1:num_t_quad,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_type_triquad_on_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_from_array(ntot_int_3d, nnod_4_ele,    &
     &          an_org, an_tgt)
!
      integer(kind = kint), intent(in) :: ntot_int_3d, nnod_4_ele
      real(kind=kreal), intent(in) ::    an_org(nnod_4_ele,ntot_int_3d)
      real(kind=kreal), intent(inout) :: an_tgt(nnod_4_ele,ntot_int_3d)
      integer(kind = kint) :: ix, k1
!
!
      do ix = 1, ntot_int_3d
        do k1 = 1, nnod_4_ele
          an_tgt(k1,ix) = an_org(k1,ix)
        end do
      end do
!
      end subroutine copy_shape_func_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_3d_type
