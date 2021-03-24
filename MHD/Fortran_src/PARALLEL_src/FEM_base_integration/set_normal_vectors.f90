!>@file   set_normal_vectors.f90
!!@brief  module set_normal_vectors
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2006
!!
!>@brief Construct normal vector on surface data
!!
!!@verbatim
!!      subroutine const_normal_vector                                  &
!!     &         (id_rank, nprocs, node, surf, spf_2d, jacs)
!!      subroutine int_normal_4_all_surface(g_FEM, surf, jac_2d)
!!        type(jacobians_2d), intent(in) :: jac_2d
!!        type(surface_data), intent(inout) :: surf
!!        type(jacobians_type), intent(inout) :: jacs
!!      subroutine int_normal_4_surface_groups                          &
!!     &         (sf_grp, g_FEM, jac_sf_grp, sf_grp_v)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(surface_group_normals), intent(inout) :: sf_grp_v
!!@endverbatim
!!
      module set_normal_vectors
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_normals
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_jacobian_2d
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_normal_vector                                    &
     &         (id_rank, nprocs, node, surf, spf_2d, jacs)
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(surface_data), intent(inout) :: surf
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call alloc_surf_shape_func(surf%nnod_4_surf, jacs%g_FEM, spf_2d)
      call const_jacobians_surface                                      &
     &   (id_rank, nprocs, node, surf, spf_2d, jacs)
      call int_normal_4_all_surface(jacs%g_FEM, surf, jacs%jac_2d)
!
      call dealloc_jacobians_surface(surf, jacs)
!
      end subroutine const_normal_vector
!
!-----------------------------------------------------------------------
!
      subroutine const_normal_4_all_surf_grps(id_rank, nprocs,          &
     &          node, ele, surf, sf_grp, spf_2d, jacs, sf_grp_v)
!
      use int_area_normal_4_surface
      use sum_normal_4_surf_group
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
      type(surface_group_normals), intent(inout) :: sf_grp_v
!
!
      if (iflag_debug.eq.1) write(*,*)  'const_jacobian_sf_grp'
      call alloc_surf_shape_func(surf%nnod_4_surf, jacs%g_FEM, spf_2d)
      call const_jacobians_surf_group(id_rank, nprocs,                  &
     &    node, ele, surf, sf_grp, spf_2d, jacs)
!
      call alloc_vectors_surf_group(sf_grp%num_grp, sf_grp%num_item,    &
     &                              sf_grp_v)
      call int_normal_4_surface_groups                                  &
     &   (sf_grp, jacs%g_FEM, jacs%jac_sf_grp, sf_grp_v)
      call dealloc_jacobians_surf_grp(surf, jacs)
!
      call s_sum_normal_4_surf_group(ele, sf_grp, sf_grp_v)
!
      end subroutine const_normal_4_all_surf_grps
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_normal_4_all_surface(g_FEM, surf, jac_2d)
!
      use int_area_normal_4_surface
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d
      type(surface_data), intent(inout) :: surf
!
!
      call int_normal_all_surf(surf%numsurf, surf%istack_surf_smp,      &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_2d, g_FEM%int_start2,   &
     &    g_FEM%owe2d, jac_2d%ntot_int, g_FEM%max_int_point,            &
     &    jac_2d%xj_sf, jac_2d%xsf_sf, surf%area_surf,                  &
     &    surf%a_area_surf, surf%vnorm_surf)
!
      end subroutine int_normal_4_all_surface
!
!-----------------------------------------------------------------------
!
      subroutine int_normal_4_surface_groups                            &
     &         (sf_grp, g_FEM, jac_sf_grp, sf_grp_v)
!
      use int_area_normal_4_surface
!
      type(surface_group_data), intent(in) :: sf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      type(surface_group_normals), intent(inout) :: sf_grp_v
!
!
      call int_normal_surf_groups(sf_grp%num_grp, sf_grp%num_item,      &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_2d, g_FEM%int_start2,   &
     &    g_FEM%owe2d, jac_sf_grp%ntot_int, g_FEM%max_int_point,        &
     &    jac_sf_grp%xj_sf, jac_sf_grp%xsf_sf,                          &
     &    sf_grp_v%area_sf_grp, sf_grp_v%a_area_sf_grp,                 &
     &    sf_grp_v%vnorm_sf_grp)
!
      end subroutine int_normal_4_surface_groups
!
!-----------------------------------------------------------------------
!
      end module set_normal_vectors
