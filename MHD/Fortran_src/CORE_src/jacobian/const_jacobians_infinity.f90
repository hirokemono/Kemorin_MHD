!const_jacobians_infinity.f90
!      module const_jacobians_infinity
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!!      subroutine cal_jacobian_infty_linear                            &
!!     &         (node, ele, sf_grp, infty_grp, jac_3d)
!!      subroutine cal_jacobian_infty_quad                              &
!!     &         (node, ele, sf_grp, infty_grp, jac_3d)
!!      subroutine cal_jacobian_infty_lag                               &
!!     &         (node, ele, sf_grp, infty_grp, jac_3d)
!!
!!      subroutine cal_jacobian_infty_l_quad                            &
!!     &         (node, ele, sf_grp, infty_grp, jac_3d)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(jacobians_3d), intent(inout) :: jac_3d
!
      module const_jacobians_infinity
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_jacobians
      use cal_1ele_jacobians_infinte
!
      implicit none
!
      private :: copy_shape_func_inf_from_array
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_linear                              &
     &          (node, ele, sf_grp, infty_grp, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_func_infty_linear                                &
     &   (jac_3d%ntot_int, infty_grp%sf_apt(1), jac_3d%an_infty,        &
     &    dnxi_infty, dnei_infty, dnzi_infty,                           &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_8(node%numnod, ele%numele,               &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_1, dnei_1, dnzi_1,          &
     &    dnxi_infty, dnei_infty, dnzi_infty)
!
      end subroutine cal_jacobian_infty_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_quad                                &
     &         (node, ele, sf_grp, infty_grp, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_func_infty_quad(jac_3d%ntot_int,                 &
     &    infty_grp%sf_apt(1), jac_3d%an_infty,                         &
     &    dnxi_infty20, dnei_infty20, dnzi_infty20,                     &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_20(node%numnod, ele%numele,              &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_20, dnei_20, dnzi_20,       &
     &    dnxi_infty20, dnei_infty20, dnzi_infty20)
!
      end subroutine cal_jacobian_infty_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_lag                                 &
     &         (node, ele, sf_grp, infty_grp, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_func_infty_lag                                   &
     &   (jac_3d%ntot_int, infty_grp%sf_apt(1), jac_3d%an_infty,        &
     &    dnxi_infty27, dnei_infty27, dnzi_infty27,                     &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_27(node%numnod, ele%numele,              &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_27, dnei_27, dnzi_27,       &
     &    dnxi_infty27, dnei_infty27, dnzi_infty27)
!
      end subroutine cal_jacobian_infty_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_l_quad                              &
     &         (node, ele, sf_grp, infty_grp, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_func_infty_quad(jac_3d%ntot_int,                 &
     &    infty_grp%sf_apt(1), jac_3d%an_infty,                         &
     &    dnxi_infty20, dnei_infty20, dnzi_infty20,                     &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_8_20(node%numnod, ele%numele,            &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_20, dnei_20, dnzi_20,       &
     &    dnxi_infty20, dnei_infty20, dnzi_infty20)
!
      end subroutine cal_jacobian_infty_l_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_inf_from_array(ntot_int_3d,            &
     &          nnod_4_ele, an_infty_org, an_infty_dest)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, nnod_4_ele
      real(kind=kreal), intent(in)                                      &
     &      ::    an_infty_org(nnod_4_ele,nsurf_4_ele,ntot_int_3d)
      real(kind=kreal), intent(inout)                                   &
     &     :: an_infty_dest(nnod_4_ele,nsurf_4_ele,ntot_int_3d)
!
      integer (kind=kint) :: ix, isf, k1
!
      do ix = 1, ntot_int_3d
        do isf = 1, nsurf_4_ele
          do k1 = 1, nnod_4_ele
            an_infty_dest(k1,isf,ix) = an_infty_org(k1,isf,ix)
          end do
        end do
      end do
!
      end subroutine copy_shape_func_inf_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_infinity
