!>@file  int_volume_of_domain.f90
!!       module int_volume_of_domain
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!!@date  programmed by H.Matsui and H.Okuda
!!@n                                    in July 2000 (ver 1.1)
!!@n        Modified by H. Matsui in Aug., 2006
!!@n        Modified by H. Matsui in June, 2007
!!@n        Modified by H. Matsui in Sep., 2016
!!@n
!> @brief Construct jacobians and volume integrations
!!
!!@verbatim
!!      subroutine const_jacobian_volume_normals(my_rank, nprocs,       &
!!     &          mesh, surf, group, spfs, jacobians)
!!      subroutine const_jacobian_and_volume(my_rank, nprocs,           &
!!     &          node, sf_grp, infinity_list, ele, spf_3d, jacobians)
!!      subroutine const_jacobian_and_vol_layer                         &
!!     &         (my_rank, nprocs, node, sf_grp, infinity_list, ele,    &
!!     &          spfs, jacobians, layer_tbl)
!!        type(node_data), intent(in) :: node
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!!        type(element_data), intent(inout) :: ele
!!        type(shape_finctions_at_points), intent(inout) :: spfs
!!        type(volume_shape_function), intent(inout) :: spf_3d
!!        type(jacobians_type), intent(inout) :: jacobians
!!        type(layering_tbl), intent(inout) :: layer_tbl
!!      subroutine s_int_volume_of_domain(ele, jac_3d)
!!@endverbatim
!
      module int_volume_of_domain
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_surface_boundary
      use t_shape_functions
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_volume_normals(my_rank, nprocs,         &
     &          mesh, surf, group, spfs, jacobians)
!
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use set_connects_4_surf_group
      use const_jacobians_3d
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(mesh_geometry), intent(inout) :: mesh
      type(surface_data), intent(inout) :: surf
      type(mesh_groups), intent(inout) :: group
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call initialize_FEM_integration                                   &
     &  (spfs%spf_3d, spfs%spf_2d, spfs%spf_1d)
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_volume'
      call const_jacobian_and_volume(my_rank, nprocs,                   &
     &    mesh%node, group%surf_grp, group%infty_grp,                   &
     &    mesh%ele, spfs%spf_3d, jacobians)
      call dealloc_vol_shape_func(spfs%spf_3d)
!
!     --------------------- Surface jacobian for fieldline
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector                                          &
     &   (my_rank, nprocs, mesh%node, surf, spfs%spf_2d, jacobians)
      call dealloc_surf_shape_func(spfs%spf_2d)
!
      if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group(surf, group%surf_grp,              &
     &    group%tbls_surf_grp, group%surf_grp_geom)
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group(mesh%ele,                          &
     &    group%surf_grp, group%surf_grp_geom)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_surf_norm_node'
      call cal_surf_normal_at_nod(mesh%node, mesh%ele, surf,            &
     &    group%surf_grp, group%surf_grp_geom, group%surf_nod_grp)
!
!      call check_jacobians_trilinear                                   &
!     &   (my_rank, mesh%ele, jacobians%jac_3d_l)
!
      end subroutine const_jacobian_volume_normals
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_jacobian_and_volume(my_rank, nprocs,             &
     &          node, sf_grp, infinity_list, ele, spf_3d, jacobians)
!
      use t_shape_functions
      use sum_volume_of_domain
      use const_jacobians_3d
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
!
      type(element_data), intent(inout) :: ele
      type(volume_shape_function), intent(inout) :: spf_3d
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call alloc_vol_shape_func(ele%nnod_4_ele, maxtot_int_3d, spf_3d)
      call const_jacobians_element(my_rank, nprocs,                     &
     &    node, ele, sf_grp, infinity_list, spf_3d, jacobians)
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain(ele, jacobians%jac_3d)
      call deallocate_volume_4_smp
!
      call dealloc_dxi_dx_element(ele, jacobians)
!
      end subroutine const_jacobian_and_volume
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_and_vol_layer                           &
     &         (my_rank, nprocs, node, sf_grp, infinity_list, ele,      &
     &          spfs, jacobians, layer_tbl)
!
      use t_shape_functions
      use t_layering_ele_list
      use const_jacobians_3d
      use sum_volume_of_domain
      use cal_layered_volumes
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
!
      type(element_data), intent(inout) :: ele
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacobians
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      call initialize_FEM_integration                                   &
     &   (spfs%spf_3d, spfs%spf_2d, spfs%spf_1d)
      call alloc_vol_shape_func                                         &
     &   (ele%nnod_4_ele, maxtot_int_3d, spfs%spf_3d)
      call const_jacobians_element(my_rank, nprocs,                     &
     &    node, ele, sf_grp, infinity_list, spfs%spf_3d, jacobians)
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain(ele, jacobians%jac_3d)
      call s_cal_layered_volumes(ele, layer_tbl)
      call deallocate_volume_4_smp
!
      call dealloc_dxi_dx_element(ele, jacobians)
!
      end subroutine const_jacobian_and_vol_layer
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_int_volume_of_domain(ele, jac_3d)
!
      use calypso_mpi
      use m_fem_gauss_int_coefs
      use fem_element_volume
      use sum_volume_of_domain
!
      type(jacobians_3d), intent(in) :: jac_3d
      type(element_data), intent(inout) :: ele
!
!
!      write(*,*) 'fem_element_volume_pg', max_int_point
       call fem_element_volume_pg(ele, g_FEM1, jac_3d, max_int_point)
!
!     ---  lead total volume
!
!      write(*,*) 'sum_4_volume'
      call sum_4_volume(ele%numele, ele%interior_ele,                   &
     &    ele%istack_ele_smp, ele%volume_ele, vol_local)
!
!      write(*,*) 'MPI_allREDUCE'
       call MPI_allREDUCE (vol_local, ele%volume, ione,                 &
     &  CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
       if (ele%volume .eq. 0.0d0) then
         ele%a_vol = 1.0d30
       else
         ele%a_vol = 1.0d0 / ele%volume
       end if
!
       end subroutine s_int_volume_of_domain
!
!-----------------------------------------------------------------------
!
      end module int_volume_of_domain
