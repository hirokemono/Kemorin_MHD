!t_jacobians.f90
!     module t_jacobians
!
!      Written by H. Matsui on Dec., 2008
!
!>   Structure of Jacobian and difference of shape functions
!
!!      subroutine dealloc_3d_linear_jac_type(jacobians)
!!      subroutine unlink_3d_linear_jac_type(jacobians)
!!        type(jacobians_type), intent(inout) :: jacobians
!!
!!      subroutine const_jacobians_element(my_rank, nprocs,             &
!!     &          node, ele, surf_grp, infinity_list, jacobians)
!!      subroutine const_jacobians_surf_group (my_rank, nprocs,         &
!!     &          node, ele, surf, surf_grp, spf_2d, jacobians)
!!      subroutine const_jacobians_surface                              &
!!     &         (my_rank, nprocs, node, surf, spf_2d, jacobians)
!!      subroutine const_jacobians_edge                                 &
!!     &         (my_rank, nprocs, node, edge, spf_1d, jacobians)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in)  :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!!        type(surface_shape_function), intent(inout) :: spf_2d
!!        type(jacobians_type), intent(inout) :: jacobians
!!
!!      subroutine dealloc_dxi_dx_element(ele, jacobians)
!!      subroutine dealloc_jacobians_element(ele, jacobians)
!!      subroutine dealloc_jacobians_surface(surf, jacobians)
!!      subroutine dealloc_jacobians_edge(edge, jacobians)
!
      module t_jacobians
!
      use m_precision
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_shape_functions
      use t_jacobian_3d
      use t_jacobian_2d
      use t_jacobian_1d
!
      implicit  none
!
!
!>     Stracture for Jacobians for FEM grid
      type jacobians_type
!>     Stracture for Jacobians for element
        type(jacobians_3d), pointer :: jac_3d
!>     Stracture for Jacobians for surface
        type(jacobians_2d), pointer :: jac_2d
!>     Stracture for Jacobians for edge
        type(jacobians_1d), pointer :: jac_1d
!>     Stracture for Jacobians for surafce group
        type(jacobians_2d), pointer :: jac_sf_grp
!
!>     Stracture for Jacobians for linear element
        type(jacobians_3d), pointer ::  jac_3d_l
!>     Stracture for Jacobians for linear surface
        type(jacobians_2d), pointer ::  jac_2d_l
!>     Stracture for Jacobians for linear edge
        type(jacobians_1d), pointer  :: jac_1d_l
!>     Stracture for Jacobians for linear surafce group
        type(jacobians_2d), pointer :: jac_sf_grp_l
!
        type(jacobians_3d), pointer ::  jac_3d_lq
!>     Stracture for quadrature Jacobians for surface
        type(jacobians_2d), pointer ::  jac_2d_lq
!>     Stracture for quadrature Jacobians for edge
        type(jacobians_1d), pointer  :: jac_1d_lq
!>     Stracture for quadrature Jacobians for surafce group
        type(jacobians_2d), pointer :: jac_sf_grp_lq
      end type jacobians_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_3d_linear_jac_type(jacobians)
!
      type(jacobians_type), intent(inout) :: jacobians
!
      deallocate(jacobians%jac_3d_l)
      deallocate(jacobians%jac_2d_l)
      deallocate(jacobians%jac_1d_l)
      deallocate(jacobians%jac_sf_grp_l)
!
      end subroutine dealloc_3d_linear_jac_type
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_3d_linear_jac_type(jacobians)
!
      type(jacobians_type), intent(inout) :: jacobians
!
      nullify(jacobians%jac_3d_l)
      nullify(jacobians%jac_2d_l)
      nullify(jacobians%jac_1d_l)
      nullify(jacobians%jac_sf_grp_l)
!
      end subroutine unlink_3d_linear_jac_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_element(my_rank, nprocs,               &
     &          node, ele, surf_grp, infinity_list, spf_3d, jacobians)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
      type(volume_shape_function), intent(inout) :: spf_3d
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_3d)
      call alloc_jacobians_type(ele%numele, ele%nnod_4_ele,             &
     &    maxtot_int_3d, jacobians%jac_3d)
      call alloc_dxi_dx_type(ele%numele, jacobians%jac_3d)
!
      if(my_rank .lt. nprocs) then
        call sel_jacobian_type(node, ele, spf_3d, jacobians%jac_3d)
        call sel_jacobian_infinity(node, ele, surf_grp,                 &
     &      infinity_list, spf_3d, jacobians%jac_3d)
      end if
      call dealloc_inv_jac_type(jacobians%jac_3d)
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        jacobians%jac_3d_l => jacobians%jac_3d
      else
        allocate(jacobians%jac_3d_l)
        call alloc_jacobians_type(ele%numele, num_t_linear,             &
     &      maxtot_int_3d, jacobians%jac_3d_l)
        call alloc_dxi_dx_type(ele%numele, jacobians%jac_3d_l)
!
        if(my_rank .lt. nprocs) then
          call cal_jacobian_trilinear                                   &
     &       (node, ele, spf_3d, jacobians%jac_3d_l)
          call const_linear_jacobian_infinity(node, ele,                &
     &        surf_grp, infinity_list, spf_3d, jacobians%jac_3d_l)
        end if
!
        call dealloc_inv_jac_type(jacobians%jac_3d_l)
      end if
!
      end subroutine const_jacobians_element
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface group
!
      subroutine const_jacobians_surf_group (my_rank, nprocs,           &
     &          node, ele, surf, surf_grp, spf_2d, jacobians)
!
      use const_jacobians_sf_grp
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in)  :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacobians
!
!
!
      allocate(jacobians%jac_sf_grp)
      call alloc_2d_jac_type(surf_grp%num_item,                         &
     &    surf%nnod_4_surf, maxtot_int_2d, jacobians%jac_sf_grp)
!
      if(my_rank .lt. nprocs) then
        call sel_jacobian_surface_grp                                   &
     &     (node, ele, surf, surf_grp, spf_2d, jacobians%jac_sf_grp)
      end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        jacobians%jac_sf_grp_l => jacobians%jac_sf_grp
      else
        allocate(jacobians%jac_sf_grp_l)
        call alloc_2d_jac_type(surf_grp%num_item, num_linear_sf,        &
     &      maxtot_int_2d, jacobians%jac_sf_grp_l)
!
        if(my_rank .lt. nprocs) then
          call const_jacobian_sf_grp_linear(node, ele, surf_grp,        &
     &        spf_2d, jacobians%jac_sf_grp_l)
        end if
      end if
!
      end subroutine const_jacobians_surf_group
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface element
!
      subroutine const_jacobians_surface                                &
     &         (my_rank, nprocs, node, surf, spf_2d, jacobians)
!
      use const_jacobians_2d
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_2d)
      call alloc_2d_jac_type(surf%numsurf,                              &
     &    surf%nnod_4_surf, maxtot_int_2d, jacobians%jac_2d)
!
      if(my_rank .lt. nprocs) then
        call sel_jacobian_surface(node, surf, spf_2d, jacobians%jac_2d)
      end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        jacobians%jac_2d_l => jacobians%jac_2d
      else
        allocate(jacobians%jac_2d_l)
        call alloc_2d_jac_type(surf%numsurf, num_linear_sf,             &
     &      maxtot_int_2d, jacobians%jac_2d_l)
        if(my_rank .lt. nprocs) then
          call cal_jacobian_surface_linear                              &
     &       (node, surf, spf_2d, jacobians%jac_2d_l)
        end if
      end if
!
      end subroutine const_jacobians_surface
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for edge element
!
      subroutine const_jacobians_edge                                   &
     &         (my_rank, nprocs, node, edge, spf_1d, jacobians)
!
      use const_jacobians_1d
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_1d)
      call alloc_1d_jac_type(edge%numedge, edge%nnod_4_edge,            &
     &    maxtot_int_1d, jacobians%jac_1d)
!
      if(my_rank .lt. nprocs) then
        call sel_jacobian_edge(node, edge, spf_1d, jacobians%jac_1d)
      end if
!
      if(edge%nnod_4_edge .eq. num_linear_edge) then
        jacobians%jac_1d_l => jacobians%jac_1d
      else
        allocate(jacobians%jac_1d_l)
        call alloc_1d_jac_type(edge%numedge, num_linear_edge,           &
     &      maxtot_int_1d, jacobians%jac_1d_l)
        if(my_rank .lt. nprocs) then
          call cal_jacobian_edge_linear                                 &
     &       (node, edge, spf_1d, jacobians%jac_1d_l)
        end if
      end if
!
      end subroutine const_jacobians_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_dxi_dx_element(ele, jacobians)
!
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call dealloc_dxi_dx_type(jacobians%jac_3d)
!
      if(ele%nnod_4_ele .ne. num_t_linear) then
        call dealloc_dxi_dx_type(jacobians%jac_3d_l)
      end if
!
      end subroutine dealloc_dxi_dx_element
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_element(ele, jacobians)
!
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call dealloc_jacobians_type(jacobians%jac_3d)
      allocate(jacobians%jac_3d)
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        jacobians%jac_3d_l => jacobians%jac_3d
      else
        call dealloc_jacobians_type(jacobians%jac_3d_l)
        deallocate(jacobians%jac_3d_l)
      end if
!
      end subroutine dealloc_jacobians_element
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_surface(surf, jacobians)
!
      use const_jacobians_2d
!
      type(surface_data), intent(in)  :: surf
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call dealloc_2d_jac_type(jacobians%jac_2d)
      deallocate(jacobians%jac_2d)
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        nullify(jacobians%jac_2d_l)
      else
        call dealloc_2d_jac_type(jacobians%jac_2d_l)
        deallocate(jacobians%jac_2d_l)
      end if
!
      end subroutine dealloc_jacobians_surface
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_edge(edge, jacobians)
!
      type(edge_data), intent(in)  :: edge
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call dealloc_1d_jac_type(jacobians%jac_1d)
      deallocate(jacobians%jac_1d)
!
      if(edge%nnod_4_edge .eq. num_linear_edge) then
        nullify(jacobians%jac_1d_l)
      else
        call dealloc_1d_jac_type(jacobians%jac_1d_l)
        deallocate(jacobians%jac_1d_l)
      end if
!
      end subroutine dealloc_jacobians_edge
!
!-----------------------------------------------------------------------
!
      end module t_jacobians
