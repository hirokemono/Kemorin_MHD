!>@file  int_volume_of_single_domain.f90
!!       module int_volume_of_single_domain
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
!!      subroutine const_jacobian_and_single_vol                        &
!!     &         (mesh, group, spfs, jacs)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(shape_finctions_at_points), intent(inout) :: spfs
!!        type(jacobians_type), intent(inout) :: jacs
!!      subroutine int_single_volume_of_domain(ele, g_FEM, jac_3d)
!!        type(node_data), intent(in) :: node
!!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!!        type(element_data), intent(inout) :: ele
!!      subroutine cal_node_volue(ele, g_FEM, jac_3d)
!!@endverbatim
!
      module int_volume_of_single_domain
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_surface_boundary
      use t_shape_functions
      use t_fem_gauss_int_coefs
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
      subroutine const_jacobian_and_single_vol                          &
     &         (mesh, group, spfs, jacs)
!
      use t_shape_functions
      use sum_volume_of_domain
      use const_jacobians_3d
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacs
!
!
      call initialize_FEM_integration                                   &
     &   (jacs%g_FEM, spfs%spf_3d, spfs%spf_2d, spfs%spf_1d)
!
      call alloc_vol_shape_func                                         &
     &   (mesh%ele%nnod_4_ele, jacs%g_FEM, spfs%spf_3d)
      call const_jacobians_element(0, 1, mesh%node, mesh%ele,           &
     &    group%surf_grp, group%infty_grp, spfs%spf_3d, jacs)
!
      call allocate_volume_4_smp
      call int_single_volume_of_domain                                  &
     &   (mesh%ele, jacs%g_FEM, jacs%jac_3d)
      call deallocate_volume_4_smp
!
      call dealloc_dxi_dx_element(mesh%ele, jacs)
!
      end subroutine const_jacobian_and_single_vol
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_single_volume_of_domain(ele, g_FEM, jac_3d)
!
      use fem_element_volume
      use sum_volume_of_domain
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(element_data), intent(inout) :: ele
!
!
!      write(*,*) 'fem_element_volume_pg', g_FEM%max_int_point
       call fem_element_volume_pg                                       &
     &    (ele, g_FEM, jac_3d, g_FEM%max_int_point)
!
!     ---  lead total volume
!
!      write(*,*) 'sum_4_volume'
      call sum_4_volume(ele%numele, ele%interior_ele,                   &
     &    ele%istack_ele_smp, ele%volume_ele, ele%volume)
!
      if (ele%volume .eq. 0.0d0) then
        ele%a_vol = 1.0d30
      else
        ele%a_vol = 1.0d0 / ele%volume
      end if
!
      write(*,*)  'size of single domain: ', ele%volume
!
      end subroutine int_single_volume_of_domain
!
!-----------------------------------------------------------------------
!
      subroutine cal_node_volue(node, ele, node_volume)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(inout) :: node_volume(node%numnod)
!
      integer(kind = kint), allocatable :: ele_cnt(:)
      integer(kind = kint) :: inode, i, j
!
      allocate(ele_cnt(node%numnod))
      ele_cnt(:) = 0
      node_volume(1:node%numnod) = 0.0
      do i = 1, ele%numele
        do j = 1, ele%nnod_4_ele
          inode = ele%ie(i,j)
          node_volume(inode) = node_volume(inode) + ele%volume_ele(i)
          ele_cnt(inode) = ele_cnt(inode) + 1
        end do
      end do
!
      do i = 1, node%numnod
        if(ele_cnt(i) .gt. 0) then
          node_volume(i) = node_volume(i) / ele_cnt(i)
        end if
      end do
!
      end subroutine cal_node_volue
!
!-----------------------------------------------------------------------
!
      end module int_volume_of_single_domain
