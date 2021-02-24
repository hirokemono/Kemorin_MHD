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
!!@endverbatim
!!
      module set_normal_vectors
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_surface_data
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
      subroutine int_normal_4_all_surface(g_FEM, surf, jac_2d)
!
      use int_area_normal_4_surface
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d
      type(surface_data), intent(inout) :: surf
!
      integer(kind = kint) :: num_int
!
!
      call alloc_normal_vector(surf)
      num_int = g_FEM%max_int_point
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
      end module set_normal_vectors
