!>@file   set_interpolate_file_name.f90
!!@brief  module set_interpolate_file_name
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Structure of Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine dealloc_3d_linear_jac_type(jacs)
!!      subroutine unlink_3d_linear_jac_type(jacs)
!!        type(jacobians_type), intent(inout) :: jacs
!!
!!      subroutine const_jacobians_element(id_rank, nprocs,             &
!!     &          node, ele, surf_grp, infinity_list, jacs)
!!      subroutine const_jacobians_surf_group (id_rank, nprocs,         &
!!     &          node, ele, surf, surf_grp, spf_2d, jacs)
!!      subroutine const_jacobians_surface                              &
!!     &         (id_rank, nprocs, node, surf, spf_2d, jacs)
!!      subroutine const_jacobians_edge                                 &
!!     &         (id_rank, nprocs, node, edge, spf_1d, jacs)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in)  :: surf
!!        type(edge_data), intent(in)  :: edge
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!!        type(surface_shape_function), intent(inout) :: spf_2d
!!        type(jacobians_type), intent(inout) :: jacs
!!
!!      subroutine dealloc_dxi_dx_element(ele, jacs)
!!      subroutine dealloc_jacobians_element(ele, jacs)
!!      subroutine dealloc_jacobians_surface(surf, jacs)
!!      subroutine dealloc_jacobians_edge(edge, jacs)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in)  :: surf
!!        type(edge_data), intent(in)  :: edge
!!        type(jacobians_type), intent(inout) :: jacs
!!@endverbatim
!
      module t_jacobians
!
      use m_precision
      use m_geometry_constants
!
      use t_fem_gauss_int_coefs
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
!>     Gauss points and weights
        type(FEM_gauss_int_coefs), pointer :: g_FEM
!
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
      subroutine dealloc_3d_linear_jac_type(jacs)
!
      type(jacobians_type), intent(inout) :: jacs
!
      if(associated(jacs%jac_3d_l) .eqv. .FALSE.) return
      deallocate(jacs%jac_3d_l)
      deallocate(jacs%jac_2d_l)
      deallocate(jacs%jac_1d_l)
      deallocate(jacs%jac_sf_grp_l)
!
      end subroutine dealloc_3d_linear_jac_type
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_3d_linear_jac_type(jacs)
!
      type(jacobians_type), intent(inout) :: jacs
!
      if(associated(jacs%jac_3d_l) .eqv. .FALSE.) return
      nullify(jacs%jac_3d_l)
      nullify(jacs%jac_2d_l)
      nullify(jacs%jac_1d_l)
      nullify(jacs%jac_sf_grp_l)
!
      end subroutine unlink_3d_linear_jac_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_element(id_rank, nprocs,               &
     &          node, ele, surf_grp, infinity_list, spf_3d, jacs)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
      type(volume_shape_function), intent(inout) :: spf_3d
      type(jacobians_type), intent(inout) :: jacs
!
!
      allocate(jacs%jac_3d)
      call alloc_jacobians(ele%numele, ele%nnod_4_ele,                  &
     &    jacs%g_FEM%maxtot_int_3d, jacs%jac_3d)
      call alloc_inv_jacobian(ele%numele, jacs%jac_3d)
      call alloc_dxi_dx(ele%numele, jacs%jac_3d)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_type                                          &
     &     (node, ele, jacs%g_FEM, spf_3d, jacs%jac_3d)
        call sel_jacobian_infinity(node, ele, surf_grp,                 &
     &      infinity_list, jacs%g_FEM, spf_3d, jacs%jac_3d)
      end if
      call dealloc_inv_jacobian(jacs%jac_3d)
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        jacs%jac_3d_l => jacs%jac_3d
      else
        allocate(jacs%jac_3d_l)
        call alloc_jacobians(ele%numele, num_t_linear,                  &
     &      jacs%g_FEM%maxtot_int_3d, jacs%jac_3d_l)
        call alloc_inv_jacobian(ele%numele, jacs%jac_3d)
        call alloc_dxi_dx(ele%numele, jacs%jac_3d_l)
!
        if(id_rank .lt. nprocs) then
          call cal_jacobian_trilinear                                   &
     &       (node, ele, jacs%g_FEM, spf_3d, jacs%jac_3d_l)
          call const_linear_jacobian_infinity(node, ele, surf_grp,      &
     &        infinity_list, jacs%g_FEM, spf_3d, jacs%jac_3d_l)
        end if
!
        call dealloc_inv_jacobian(jacs%jac_3d_l)
      end if
!
      end subroutine const_jacobians_element
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface group
!
      subroutine const_jacobians_surf_group (id_rank, nprocs,           &
     &          node, ele, surf, surf_grp, spf_2d, jacs)
!
      use const_jacobians_sf_grp
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in)  :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
!
!
!
      allocate(jacs%jac_sf_grp)
      call alloc_2d_jac_type(surf_grp%num_item,                         &
     &    surf%nnod_4_surf, jacs%g_FEM%maxtot_int_2d, jacs%jac_sf_grp)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_surface_grp (node, ele, surf, surf_grp,       &
     &      jacs%g_FEM, spf_2d, jacs%jac_sf_grp)
      end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        jacs%jac_sf_grp_l => jacs%jac_sf_grp
      else
        allocate(jacs%jac_sf_grp_l)
        call alloc_2d_jac_type(surf_grp%num_item, num_linear_sf,        &
     &      jacs%g_FEM%maxtot_int_2d, jacs%jac_sf_grp_l)
!
        if(id_rank .lt. nprocs) then
          call const_jacobian_sf_grp_linear(node, ele, surf_grp,        &
     &        jacs%g_FEM, spf_2d, jacs%jac_sf_grp_l)
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
     &         (id_rank, nprocs, node, surf, spf_2d, jacs)
!
      use const_jacobians_2d
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(surface_shape_function), intent(inout) :: spf_2d
      type(jacobians_type), intent(inout) :: jacs
!
!
      allocate(jacs%jac_2d)
      call alloc_2d_jac_type(surf%numsurf,                              &
     &    surf%nnod_4_surf, jacs%g_FEM%maxtot_int_2d, jacs%jac_2d)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_surface                                       &
     &     (node, surf, jacs%g_FEM, spf_2d, jacs%jac_2d)
      end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        jacs%jac_2d_l => jacs%jac_2d
      else
        allocate(jacs%jac_2d_l)
        call alloc_2d_jac_type(surf%numsurf, num_linear_sf,             &
     &      jacs%g_FEM%maxtot_int_2d, jacs%jac_2d_l)
        if(id_rank .lt. nprocs) then
          call cal_jacobian_surface_linear                              &
     &       (node, surf, jacs%g_FEM, spf_2d, jacs%jac_2d_l)
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
     &         (id_rank, nprocs, node, edge, spf_1d, jacs)
!
      use const_jacobians_1d
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_type), intent(inout) :: jacs
!
!
      allocate(jacs%jac_1d)
      call alloc_1d_jac_type(edge%numedge, edge%nnod_4_edge,            &
     &    jacs%g_FEM%maxtot_int_1d, jacs%jac_1d)
!
      if(id_rank .lt. nprocs) then
        call sel_jacobian_edge                                          &
     &     (node, edge, jacs%g_FEM, spf_1d, jacs%jac_1d)
      end if
!
      if(edge%nnod_4_edge .eq. num_linear_edge) then
        jacs%jac_1d_l => jacs%jac_1d
      else
        allocate(jacs%jac_1d_l)
        call alloc_1d_jac_type(edge%numedge, num_linear_edge,           &
     &      jacs%g_FEM%maxtot_int_1d, jacs%jac_1d_l)
        if(id_rank .lt. nprocs) then
          call cal_jacobian_edge_linear                                 &
     &       (node, edge, jacs%g_FEM, spf_1d, jacs%jac_1d_l)
        end if
      end if
!
      end subroutine const_jacobians_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_dxi_dx_element(ele, jacs)
!
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(inout) :: jacs
!
!
      call dealloc_dxi_dx(jacs%jac_3d)
!
      if(ele%nnod_4_ele .ne. num_t_linear) then
        call dealloc_dxi_dx(jacs%jac_3d_l)
      end if
!
      end subroutine dealloc_dxi_dx_element
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_element(ele, jacs)
!
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(inout) :: jacs
!
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        nullify(jacs%jac_3d_l)
      else
        call dealloc_jacobians(jacs%jac_3d_l)
        deallocate(jacs%jac_3d_l)
      end if
!
      call dealloc_jacobians(jacs%jac_3d)
      deallocate(jacs%jac_3d)
!
      end subroutine dealloc_jacobians_element
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_surface(surf, jacs)
!
      use const_jacobians_2d
!
      type(surface_data), intent(in)  :: surf
      type(jacobians_type), intent(inout) :: jacs
!
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        nullify(jacs%jac_2d_l)
      else
        call dealloc_2d_jac_type(jacs%jac_2d_l)
        deallocate(jacs%jac_2d_l)
      end if
!
      call dealloc_2d_jac_type(jacs%jac_2d)
      deallocate(jacs%jac_2d)
!
      end subroutine dealloc_jacobians_surface
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_edge(edge, jacs)
!
      type(edge_data), intent(in)  :: edge
      type(jacobians_type), intent(inout) :: jacs
!
!
      if(edge%nnod_4_edge .eq. num_linear_edge) then
        nullify(jacs%jac_1d_l)
      else
        call dealloc_1d_jac_type(jacs%jac_1d_l)
        deallocate(jacs%jac_1d_l)
      end if
!
      call dealloc_1d_jac_type(jacs%jac_1d)
      deallocate(jacs%jac_1d)
!
      end subroutine dealloc_jacobians_edge
!
!-----------------------------------------------------------------------
!
      end module t_jacobians
