!const_jacobians_infinity.f90
!      module const_jacobians_infinity
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!!      subroutine sel_jacobian_infinity                                &
!!     &         (node, ele, surf_grp, infty_grp, jac_3d)
!!      subroutine const_linear_jacobian_infinity                       &
!!     &         (node, ele, surf_grp, infty_grp, jac_3d)
!!
!!      subroutine cal_jacobian_infty_l_quad                            &
!!     &         (node, ele, sf_grp, infty_grp, jac_3d)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(jacobians_3d), intent(inout) :: jac_3d
!!
!!      subroutine copy_shape_func_inf_from_array(ntot_int_3d,          &
!!     &          nnod_4_ele, an_infty_org, an_infty_dest)
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
      use t_shape_functions
      use t_jacobian_3d
      use cal_1ele_jacobians_infinte
!
      implicit none
!
      type(infty_shape_function), save, private :: spf_infty
!
      private :: cal_jacobian_infty_linear, cal_jacobian_infty_quad
      private :: cal_jacobian_infty_lag
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_jacobian_infinity                                  &
     &         (node, ele, surf_grp, infty_grp, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      if(infty_grp%ngrp_sf .le. 0) return
!
      call alloc_shape_func_infty                                       &
     &   (ele%nnod_4_ele, nsurf_4_ele, maxtot_int_3d, spf_infty)
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call cal_jacobian_infty_linear                                  &
     &     (node, ele, surf_grp, infty_grp, spf_infty, jac_3d)
      else if (ele%nnod_4_ele .eq. num_t_quad) then
        call cal_jacobian_infty_quad                                    &
     &     (node, ele, surf_grp, infty_grp, spf_infty, jac_3d)
      else if (ele%nnod_4_ele .eq. num_t_lag) then
        call cal_jacobian_infty_lag                                     &
     &    (node, ele, surf_grp, infty_grp, spf_infty, jac_3d)
      end if
!
      call dealloc_shape_func_infty(spf_infty)
!
      end subroutine sel_jacobian_infinity
!
!-----------------------------------------------------------------------
!
      subroutine const_linear_jacobian_infinity                         &
     &         (node, ele, surf_grp, infty_grp, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      if(infty_grp%ngrp_sf .le. 0) return
!
      call alloc_shape_func_infty                                       &
     &   (num_t_linear, nsurf_4_ele, maxtot_int_3d, spf_infty)
      call cal_jacobian_infty_linear                                    &
     &   (node, ele, surf_grp, infty_grp, spf_infty, jac_3d)
      call dealloc_shape_func_infty(spf_infty)
!
      end subroutine const_linear_jacobian_infinity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_linear                              &
     &          (node, ele, sf_grp, infty_grp, spf_inf8, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(infty_shape_function), intent(inout) :: spf_inf8
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      if(infty_grp%ngrp_sf .le. 0) return
!
      call s_cal_shape_func_infty_linear                                &
     &   (jac_3d%ntot_int, infty_grp%sf_apt(1), jac_3d%an_infty,        &
     &    spf_inf8%dnxi_inf, spf_inf8%dnei_inf, spf_inf8%dnzi_inf,      &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_8(node%numnod, ele%numele,               &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_1, dnei_1, dnzi_1,          &
     &    spf_inf8%dnxi_inf, spf_inf8%dnei_inf, spf_inf8%dnzi_inf)
!
      end subroutine cal_jacobian_infty_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_quad                                &
     &         (node, ele, sf_grp, infty_grp, spf_inf20, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(infty_shape_function), intent(inout) :: spf_inf20
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_func_infty_quad(jac_3d%ntot_int,                 &
     &    infty_grp%sf_apt(1), jac_3d%an_infty,                         &
     &    spf_inf20%dnxi_inf, spf_inf20%dnei_inf, spf_inf20%dnzi_inf,   &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_20(node%numnod, ele%numele,              &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_20, dnei_20, dnzi_20,       &
     &    spf_inf20%dnxi_inf, spf_inf20%dnei_inf, spf_inf20%dnzi_inf)
!
      end subroutine cal_jacobian_infty_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_lag                                 &
     &         (node, ele, sf_grp, infty_grp, spf_inf27, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_grp
      type(infty_shape_function), intent(inout) :: spf_inf27
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_func_infty_lag                                   &
     &   (jac_3d%ntot_int, infty_grp%sf_apt(1), jac_3d%an_infty,        &
     &    spf_inf27%dnxi_inf, spf_inf27%dnei_inf, spf_inf27%dnzi_inf,   &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_27(node%numnod, ele%numele,              &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_27, dnei_27, dnzi_27,       &
     &    spf_inf27%dnxi_inf, spf_inf27%dnei_inf, spf_inf27%dnzi_inf)
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
      call alloc_shape_func_infty                                       &
     &   (ele%nnod_4_ele, nsurf_4_ele, maxtot_int_3d, spf_infty)
!
      call s_cal_shape_func_infty_quad(jac_3d%ntot_int,                 &
     &    infty_grp%sf_apt(1), jac_3d%an_infty,                         &
     &    spf_infty%dnxi_inf, spf_infty%dnei_inf, spf_infty%dnzi_inf,   &
     &    xi3, ei3, zi3)
!
      call cal_jacobian_3d_inf_8_20(node%numnod, ele%numele,            &
     &    ele%nnod_4_ele, np_smp, ele%ie, node%xx,                      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    infty_grp%ngrp_sf, infty_grp%igrp_sf,                         &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_20, dnei_20, dnzi_20,       &
     &    spf_infty%dnxi_inf, spf_infty%dnei_inf, spf_infty%dnzi_inf)
!
      call dealloc_shape_func_infty(spf_infty)
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
