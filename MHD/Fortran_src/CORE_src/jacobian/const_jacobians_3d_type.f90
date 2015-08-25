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
      use cal_1ele_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, jac1_3d_l%an, jac_3d%an)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_8                                            &
     &       (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,   &
     &        np_smp, mesh%ele%istack_ele_smp, mesh%ele%ie,             &
     &        mesh%node%xx, jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac, &
     &        jac_3d%dnx, jac_3d%dxidx_3d, dnxi_1, dnei_1, dnzi_1)
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
      use cal_1ele_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, jac1_3d_q%an, jac_3d%an)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_20(mesh%node%numnod, mesh%ele%numele,        &
     &        mesh%ele%nnod_4_ele, np_smp, mesh%ele%istack_ele_smp,     &
     &        mesh%ele%ie, mesh%node%xx, jac_3d%ntot_int,               &
     &        jac_3d%xjac, jac_3d%axjac, jac_3d%dnx, jac_3d%dxidx_3d,   &
     &        dnxi_20, dnei_20, dnzi_20)
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
      use cal_1ele_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, jac1_3d_q%an, jac_3d%an)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_27(mesh%node%numnod, mesh%ele%numele,        &
     &        mesh%ele%nnod_4_ele, np_smp, mesh%ele%istack_ele_smp,     &
     &        mesh%ele%ie, mesh%node%xx, jac_3d%ntot_int,               &
     &        jac_3d%xjac, jac_3d%axjac, jac_3d%dnx, jac_3d%dxidx_3d,   &
     &        dnxi_27, dnei_27, dnzi_27)
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
      use cal_1ele_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call copy_shape_func_from_array(mesh%ele%nnod_4_ele,              &
     &    jac_3d%ntot_int, jac1_3d_q%an, jac_3d%an)
!
      call cal_jacobian_3d_8_20(mesh%node%numnod, mesh%ele%numele,      &
     &        mesh%ele%nnod_4_ele, np_smp, mesh%ele%istack_ele_smp,     &
     &        mesh%ele%ie, mesh%node%xx,                                &
     &        jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac, jac_3d%dnx,   &
     &        jac_3d%dxidx_3d, dnxi_20, dnei_20, dnzi_20 )
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
