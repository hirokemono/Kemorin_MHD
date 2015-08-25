!>@file  const_jacobians_2d_type.f90
!!       module const_jacobians_2d_type
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Construct Jacobians on surfaces
!!
!!@verbatim
!!      subroutine cal_jacobian_surface_linear(node, surf, jac_2d)
!!      subroutine cal_jacobian_surface_quad(node, surf, jac_2d)
!!      subroutine cal_jacobian_surface_lag(node, surf, jac_2d)
!!      subroutine cal_jacobian_surface_quad_on_l(node, surf, jac_2d)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in)  :: surf
!!        type(jacobians_2d), intent(inout) :: jac_2d
!!
!!      subroutine cal_jacobian_sf_grp_linear(node, ele, surf_grp,      &
!!     &          jac_sf_grp)
!!      subroutine cal_jacobian_sf_grp_quad(node, ele, surf_grp,        &
!!     &          jac_sf_grp)
!!      subroutine cal_jacobian_sf_grp_lag(node, ele, surf_grp,         &
!!     &          jac_sf_grp)
!!      subroutine cal_jacobian_sf_grp_l_quad(node, ele, surf_grp,      &
!!     &          jac_sf_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(jacobians_2d), intent(inout) :: jac_sf_grp
!!@endverbatim
!
      module const_jacobians_2d_type
!
      use m_precision
      use m_machine_parameter
!
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobians
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_linear(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_linear(jac_2d%ntot_int,              &
     &    jac_2d%an_sf, dnxi_sf1, dnei_sf1, xi2, ei2)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_2d_4                                            &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf1, dnei_sf1)

      end subroutine cal_jacobian_surface_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_quad(jac_2d%ntot_int,                &
     &    jac_2d%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_2d_8                                            &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf20, dnei_sf20)
!
      end subroutine cal_jacobian_surface_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_lag(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_lag(jac_2d%ntot_int,                 &
     &    jac_2d%an_sf, dnxi_sf27, dnei_sf27, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_2d_9                                            &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf27, dnei_sf27)
!
      end subroutine cal_jacobian_surface_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad_on_l(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_quad(jac_2d%ntot_int,                &
     &    jac_2d%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature elaments
      call cal_jacobian_2d_4_8                                          &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf20, dnei_sf20)
!
      end subroutine cal_jacobian_surface_quad_on_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_sf_grp_linear(node, ele, surf_grp,        &
     &          jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_linear(jac_sf_grp%ntot_int,          &
     &    jac_sf_grp%an_sf, dnxi_sf1, dnei_sf1, xi2, ei2)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_sf_grp_4                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf1, dnei_sf1)
!
      end subroutine cal_jacobian_sf_grp_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_sf_grp_quad(node, ele, surf_grp,          &
     &          jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp%ntot_int,            &
     &    jac_sf_grp%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_8                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf20, dnei_sf20 )
!
      end subroutine cal_jacobian_sf_grp_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_sf_grp_lag(node, ele, surf_grp,           &
     &          jac_sf_grp)
!
      use m_geometry_constants
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_lag(jac_sf_grp%ntot_int,             &
     &    jac_sf_grp%an_sf, dnxi_sf27, dnei_sf27, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_9                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf27, dnei_sf27)
!
!
      end subroutine cal_jacobian_sf_grp_lag
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_sf_grp_l_quad(node, ele, surf_grp,        &
     &          jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp%ntot_int,            &
     &    jac_sf_grp%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_4_8                                      &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf20, dnei_sf20)
!
      end subroutine cal_jacobian_sf_grp_l_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_from_array(ntot_int_2d, nnod_4_surf,   &
     &          an_org, an_tgt)
!
      integer(kind = kint), intent(in) :: ntot_int_2d, nnod_4_surf
      real(kind=kreal), intent(in) :: an_org(nnod_4_surf,ntot_int_2d)
      real(kind=kreal), intent(inout)                                   &
     &         :: an_tgt(nnod_4_surf,ntot_int_2d)
      integer(kind = kint) :: ix, k1
!
!
      do ix = 1, ntot_int_2d
        do k1 = 1, nnod_4_surf
          an_tgt(k1,ix) = an_org(k1,ix)
        end do
      end do
!
      end subroutine copy_shape_func_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_2d_type
