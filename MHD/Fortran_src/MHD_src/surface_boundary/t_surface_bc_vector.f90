!>@file   t_surface_bc_vector.f90
!!@brief  module t_surface_bc_vector
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Sep. 2005
!
!>    @brief flux boundary condition lists for vector
!!
!!@verbatim
!!      subroutine set_surf_grad_vector(name_norm, name_grad,         &
!!     &         IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,&
!!     &         vector_surf, Bsf_bcs)
!!      subroutine dealloc_surf_vector(Bsf_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(surface_group_geometry), intent(in) :: sf_grp_v
!!        type(boundary_condition_list), intent(in) :: vector_surf
!!        type(vector_surf_bc_type), intent(inout) :: Bsf_bcs
!!
!!      subroutine alloc_surf_vector_num(flux_sf)
!!      subroutine alloc_surf_vector_apt(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!!      subroutine alloc_surf_vector_dat(vector_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!!
!!      subroutine dealloc_surf_vector_apt(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!!      subroutine dealloc_surf_vector_dat(vector_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!!@endverbatim
!
      module t_surface_bc_vector
!
      use m_precision
!
      use m_header_4_surface_bc
      use t_surface_bc_data
!
      implicit  none
!
      type vector_surf_bc_type
        type(scaler_surf_bc_data_type), allocatable :: sgs(:)
        type(scaler_surf_flux_bc_type) :: normal
        type(scaler_surf_flux_bc_type), allocatable :: grad(:)
        type(scaler_surf_bc_data_type), allocatable :: torque_lead(:)
      end type vector_surf_bc_type
!
      private :: alloc_surf_data_velo_num, alloc_surf_data_velo
      private :: alloc_surf_data_vect_num, alloc_surf_vector
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_vector(name_norm, name_grad,             &
     &          IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,   &
     &          vector_surf, Bsf_bcs)
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_boundary_field_IO
      use t_bc_data_list
!
      use set_surf_vector_id
      use set_sf_grad_vector_id
!
      character(len=kchara), intent(in) :: name_norm
      character(len=kchara), intent(in) :: name_grad(3)
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(boundary_condition_list), intent(in) :: vector_surf
      type(vector_surf_bc_type), intent(inout) :: Bsf_bcs
!
!
      call alloc_surf_data_vect_num(Bsf_bcs)
!
      call s_count_num_surf_vector(IO_bc, sf_grp, sf_grp_nod,           &
     &   vector_surf%num_bc, vector_surf%bc_name, vector_surf%ibc_type, &
     &   name_norm, Bsf_bcs%sgs, Bsf_bcs%normal)
      call count_num_sf_grad_vector                                     &
     &    (IO_bc, sf_grp, vector_surf%num_bc, vector_surf%bc_name,      &
     &     vector_surf%ibc_type, name_grad,                             &
     &     Bsf_bcs%grad, Bsf_bcs%torque_lead)
!
      call alloc_surf_vector(Bsf_bcs)
!
      call s_set_surf_vector_id                                         &
     &   (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,         &
     &    vector_surf%num_bc, vector_surf%bc_name,                      &
     &    vector_surf%ibc_type, vector_surf%bc_magnitude, name_norm,    &
     &    Bsf_bcs%sgs, Bsf_bcs%normal)
!
      call s_set_sf_grad_vector_id(IO_bc, sf_grp,                       &
     &    vector_surf%num_bc, vector_surf%bc_name,                      &
     &    vector_surf%ibc_type, vector_surf%bc_magnitude,               &
     &    name_grad, Bsf_bcs%grad, Bsf_bcs%torque_lead)
!
      end subroutine set_surf_grad_vector
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_vector(Bsf_bcs)
!
      type(vector_surf_bc_type),  intent(inout) :: Bsf_bcs
!
!
      call dealloc_surf_vector_apt(Bsf_bcs%grad)
      call dealloc_surf_vector_dat(Bsf_bcs%sgs)
      call dealloc_surf_scaler_type(Bsf_bcs%normal)
      call dealloc_surf_vector_dat(Bsf_bcs%torque_lead)
!
      deallocate(Bsf_bcs%sgs, Bsf_bcs%grad, Bsf_bcs%torque_lead)
!
      end subroutine dealloc_surf_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_data_vect_num(Bsf_bcs)
!
      type(vector_surf_bc_type),  intent(inout) :: Bsf_bcs
!
!
      allocate(Bsf_bcs%sgs(3))
      allocate(Bsf_bcs%grad(3))
      allocate(Bsf_bcs%torque_lead(3))
!
      end subroutine alloc_surf_data_vect_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector(Bsf_bcs)
!
      type(vector_surf_bc_type),  intent(inout) :: Bsf_bcs
!
!
      call alloc_surf_vector_num(Bsf_bcs%grad)
      call alloc_surf_vector_dat(Bsf_bcs%sgs)
      call alloc_surf_scaler_num(Bsf_bcs%normal)
      call alloc_surf_vector_dat(Bsf_bcs%torque_lead)
!
      call alloc_surf_vector_apt(Bsf_bcs%grad)
      call alloc_surf_scaler_apt(Bsf_bcs%normal)
!
      end subroutine alloc_surf_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_vector_num(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      call alloc_surf_scaler_num(flux_sf(1))
      call alloc_surf_scaler_num(flux_sf(2))
      call alloc_surf_scaler_num(flux_sf(3))
!
      end subroutine alloc_surf_vector_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector_apt(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      call alloc_surf_scaler_apt(flux_sf(1))
      call alloc_surf_scaler_apt(flux_sf(2))
      call alloc_surf_scaler_apt(flux_sf(3))
!
      end subroutine alloc_surf_vector_apt
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector_dat(vector_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!
      call alloc_surf_scaler_dat_type(vector_sf(1))
      call alloc_surf_scaler_dat_type(vector_sf(2))
      call alloc_surf_scaler_dat_type(vector_sf(3))
!
      end subroutine alloc_surf_vector_dat
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_vector_apt(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      call dealloc_surf_scaler_type(flux_sf(3))
      call dealloc_surf_scaler_type(flux_sf(2))
      call dealloc_surf_scaler_type(flux_sf(1))
!
      end subroutine dealloc_surf_vector_apt
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_vector_dat(vector_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!
      call dealloc_surf_scaler_dat_type(vector_sf(3))
      call dealloc_surf_scaler_dat_type(vector_sf(2))
      call dealloc_surf_scaler_dat_type(vector_sf(1))
!
      end subroutine dealloc_surf_vector_dat
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_vector
