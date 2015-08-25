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
!!      subroutine cal_jacobian_type_2d_linear(mesh, surf_mesh, jac_2d)
!!      subroutine cal_jacobian_type_2d_quad(mesh, surf_mesh, jac_2d)
!!      subroutine cal_jacobian_type_2d_quad(mesh, surf_mesh, jac_2d)
!!      subroutine cal_jacobian_type_2d_l_quad(mesh, surf_mesh, jac_2d)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(surface_geometry), intent(in)  :: surf_mesh
!!        type(jacobians_2d), intent(inout) :: jac_2d
!!
!!      subroutine cal_jacobian_type_sf_grp_linear(mesh, surf_mesh,     &
!!     &          group, jac_sf_grp)
!!      subroutine cal_jacobian_type_sf_grp_quad(mesh, surf_mesh,       &
!!     &          group, jac_sf_grp)
!!      subroutine cal_jacobian_type_sf_grp_lag(mesh, surf_mesh,        &
!!     &          group, jac_sf_grp)
!!      subroutine cal_jacobian_type_sf_grp_l_quad(mesh, surf_mesh,     &
!!     &          group, jac_sf_grp)
!!        type(mesh_geometry),    intent(in) :: mesh
!!        type(surface_geometry), intent(in) :: surf_mesh
!!        type(mesh_groups),      intent(in) :: group
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
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_2d_linear(mesh, surf_mesh, jac_2d)
!
      use m_jacobians_4_surface
      use t_mesh_data
      use t_jacobians
      use cal_1surf_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call copy_shape_func_from_array(jac_2d%ntot_int,                  &
     &    surf_mesh%surf%nnod_4_surf, jac1_2d_l%an_sf, jac_2d%an_sf)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_2d_4(mesh%node%numnod, surf_mesh%surf%numsurf,  &
     &     surf_mesh%surf%nnod_4_surf, surf_mesh%surf%ie_surf,          &
     &     mesh%node%xx, np_smp, surf_mesh%surf%istack_surf_smp,        &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf1, dnei_sf1)

      end subroutine cal_jacobian_type_2d_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_2d_quad(mesh, surf_mesh, jac_2d)
!
      use m_jacobians_4_surface
      use t_mesh_data
      use t_jacobians
      use cal_1surf_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_2d%ntot_int,                  &
     &    surf_mesh%surf%nnod_4_surf, jac1_2d_q%an_sf, jac_2d%an_sf)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_2d_8(mesh%node%numnod, surf_mesh%surf%numsurf,  &
     &     surf_mesh%surf%nnod_4_surf, surf_mesh%surf%ie_surf,          &
     &     mesh%node%xx, np_smp, surf_mesh%surf%istack_surf_smp,        &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf1, dnei_sf1)
!
      end subroutine cal_jacobian_type_2d_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_2d_lag(mesh, surf_mesh, jac_2d)
!
      use m_jacobians_4_surface
      use t_mesh_data
      use t_jacobians
      use cal_1surf_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call copy_shape_func_from_array(jac_2d%ntot_int,                  &
     &   surf_mesh%surf%nnod_4_surf, jac1_2d_q%an_sf, jac_2d%an_sf)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_2d_9(mesh%node%numnod, surf_mesh%surf%numsurf,  &
     &     surf_mesh%surf%nnod_4_surf, surf_mesh%surf%ie_surf,          &
     &     mesh%node%xx, np_smp, surf_mesh%surf%istack_surf_smp,        &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf1, dnei_sf1)
!
      end subroutine cal_jacobian_type_2d_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_2d_l_quad(mesh, surf_mesh, jac_2d)
!
      use m_geometry_constants
      use m_jacobians_4_surface
      use t_mesh_data
      use t_jacobians
      use cal_1surf_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_2d%ntot_int,                  &
     &    surf_mesh%surf%nnod_4_surf, jac1_2d_q%an_sf, jac_2d%an_sf)
!
!   jacobian for quadrature elaments
      call cal_jacobian_2d_4_8(mesh%node%numnod, surf_mesh%surf%numsurf,  &
     &     surf_mesh%surf%nnod_4_surf, surf_mesh%surf%ie_surf,          &
     &     mesh%node%xx, np_smp, surf_mesh%surf%istack_surf_smp,        &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf1, dnei_sf1)
!
      end subroutine cal_jacobian_type_2d_l_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_sf_grp_linear(mesh, surf_mesh,       &
     &          group, jac_sf_grp)
!
      use m_geometry_constants
      use m_jacobian_sf_grp
      use t_mesh_data
      use t_jacobians
      use cal_jacobian_sf_grp_linear
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(mesh_groups),      intent(in) :: group
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_sf_grp%ntot_int,              &
     &    surf_mesh%surf%nnod_4_surf, jac1_sf_grp_2d_l%an_sf,           &
     &    jac_sf_grp%an_sf)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_4(mesh%node%numnod,                &
     &       mesh%ele%numele, mesh%ele%ie, mesh%node%xx,                &
     &       group%surf_grp%num_grp, group%surf_grp%num_item,           &
     &       group%surf_grp%item_sf_grp, np_smp,                        &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_sf_grp%xj_sf(1:group%surf_grp%num_item,ix),            &
     &       jac_sf_grp%axj_sf(1:group%surf_grp%num_item,ix),           &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,1),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,2),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,3),         &
     &       dnxi_sf1(1:surf_mesh%surf%nnod_4_surf,ix),                 &
     &       dnei_sf1(1:surf_mesh%surf%nnod_4_surf,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_type_sf_grp_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_sf_grp_quad(mesh, surf_mesh,         &
     &          group, jac_sf_grp)
!
      use m_geometry_constants
      use m_jacobian_sf_grp
      use t_mesh_data
      use t_jacobians
      use cal_jacobian_sf_grp_quad
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(mesh_groups),      intent(in) :: group
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_sf_grp%ntot_int,              &
     &    surf_mesh%surf%nnod_4_surf, jac1_sf_grp_2d_q%an_sf,           &
     &    jac_sf_grp%an_sf)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_8(mesh%node%numnod,                &
     &       mesh%ele%numele, mesh%ele%ie, mesh%node%xx,                &
     &       group%surf_grp%num_grp, group%surf_grp%num_item,           &
     &       group%surf_grp%item_sf_grp, np_smp,                        &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_sf_grp%xj_sf(1:group%surf_grp%num_item,ix),            &
     &       jac_sf_grp%axj_sf(1:group%surf_grp%num_item,ix),           &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,1),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,2),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,3),         &
     &       dnxi_sf1(1:surf_mesh%surf%nnod_4_surf,ix),                 &
     &       dnei_sf1(1:surf_mesh%surf%nnod_4_surf,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_type_sf_grp_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_sf_grp_lag(mesh, surf_mesh,          &
     &          group, jac_sf_grp)
!
      use m_geometry_constants
      use m_jacobian_sf_grp
      use t_mesh_data
      use t_jacobians
      use cal_jacobian_sf_grp_lag
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(mesh_groups),      intent(in) :: group
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_sf_grp%ntot_int,              &
     &    surf_mesh%surf%nnod_4_surf, jac1_sf_grp_2d_q%an_sf,           &
     &    jac_sf_grp%an_sf)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_9(mesh%node%numnod,                &
     &       mesh%ele%numele, mesh%ele%ie, mesh%node%xx,                &
     &       group%surf_grp%num_grp, group%surf_grp%num_item,           &
     &       group%surf_grp%item_sf_grp, np_smp,                        &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_sf_grp%xj_sf(1:group%surf_grp%num_item,ix),            &
     &       jac_sf_grp%axj_sf(1:group%surf_grp%num_item,ix),           &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,1),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,2),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,3),         &
     &       dnxi_sf1(1:surf_mesh%surf%nnod_4_surf,ix),                 &
     &       dnei_sf1(1:surf_mesh%surf%nnod_4_surf,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_type_sf_grp_lag
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_sf_grp_l_quad(mesh, surf_mesh,       &
     &          group, jac_sf_grp)
!
      use m_geometry_constants
      use m_jacobian_sf_grp
      use t_mesh_data
      use t_jacobians
      use cal_jacobian_sf_grp_l_quad
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(mesh_groups),      intent(in) :: group
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_sf_grp%ntot_int,              &
     &    surf_mesh%surf%nnod_4_surf, jac1_sf_grp_2d_q%an_sf,           &
     &    jac_sf_grp%an_sf)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_4_8(mesh%node%numnod,              &
     &       mesh%ele%numele,  mesh%ele%ie, mesh%node%xx,               &
     &       group%surf_grp%num_grp, group%surf_grp%num_item,           &
     &       group%surf_grp%item_sf_grp, np_smp,                        &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_sf_grp%xj_sf(1:group%surf_grp%num_item,ix),            &
     &       jac_sf_grp%axj_sf(1:group%surf_grp%num_item,ix),           &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,1),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,2),         &
     &       jac_sf_grp%xsf_sf(1:group%surf_grp%num_item,ix,3),         &
     &       dnxi_sf1(1:surf_mesh%surf%nnod_4_surf,ix),                 &
     &       dnei_sf1(1:surf_mesh%surf%nnod_4_surf,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_type_sf_grp_l_quad
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
