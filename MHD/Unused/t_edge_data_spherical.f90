!>@file   t_edge_data_spherical.f90
!!@brief  module t_edge_data_spherical
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief edge position data in spherical coordinate
!!
!!@verbatim
!!      subroutine alloc_edge_spherical_position(edge, edge_sph)
!!      subroutine alloc_edge_vect_sph(edge, edge_sph)
!!      subroutine alloc_edge_vect_cyl(edge, edge_sph)
!!        type(edge_data), intent(in) :: edge
!!        type(edge_position_sph), intent(inout) :: edge_sph
!!
!!      subroutine dealloc_edge_spherical_position(edge_sph)
!!      subroutine dealloc_edge_vect_sph(edge_sph)
!!      subroutine dealloc_edge_vect_cyl(edge_sph)
!!        type(edge_position_sph), intent(inout) :: edge_sph
!!
!!      subroutine set_center_of_edge_sph(edge, edge_sph)
!!      subroutine cal_edge_vector_spherical(edge, edge_sph)
!!      subroutine cal_edge_vector_cylindrical(edge, edge_sph)
!!        type(edge_data), intent(in) :: edge
!!        type(edge_position_sph), intent(inout) :: edge_sph
!!
!!      subroutine copy_edge_geometry_to_IO_sph                         &
!!     &         (edge, edge_sph, nod_IO, sfed_IO)
!!      subroutine copy_edge_geometry_to_IO_cyl                         &
!!     &         (edge, edge_sph, nod_IO, sfed_IO)
!!        type(edge_data), intent(in) :: edge
!!        type(edge_position_sph), intent(in) :: edge_sph
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module t_edge_data_spherical
!
      use m_precision
!
      use t_edge_data
      use t_read_mesh_data
!
      implicit none
!
!>     Structure of edge popsition data
      type edge_position_sph
!>   distance from the center of edge
        real(kind = kreal), allocatable  :: r_edge(:)
!>   colatitude of center of edge
        real(kind = kreal), allocatable  :: theta_edge(:)
!>   longitude of center of edge
        real(kind = kreal), allocatable  :: phi_edge(:)
!>   1/r_edge
        real(kind = kreal), allocatable  :: ar_edge(:)
!
!>   cylindorical radius of center of edge
        real(kind = kreal), allocatable  :: s_edge(:)
!>   1 / s_edge
        real(kind = kreal), allocatable  :: as_edge(:)
!
!>   edge vector (spherical coordinate)
        real(kind = kreal), allocatable :: edge_vect_sph(:,:)
!>   edge vector (cylindrical coordinate)
        real(kind = kreal), allocatable :: edge_vect_cyl(:,:)
      end type edge_position_sph
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine alloc_edge_spherical_position(edge, edge_sph)
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(inout) :: edge_sph
!
      allocate( edge_sph%r_edge(edge%numedge) )
      allocate( edge_sph%theta_edge(edge%numedge) )
      allocate( edge_sph%phi_edge(edge%numedge) )
      allocate( edge_sph%ar_edge(edge%numedge) )
!
      allocate( edge_sph%s_edge(edge%numedge) )
      allocate( edge_sph%as_edge(edge%numedge) )
!
      if (edge%numedge .gt. 0) then
        edge_sph%r_edge =      0.0d0
        edge_sph%theta_edge =  0.0d0
        edge_sph%phi_edge =    0.0d0
        edge_sph%ar_edge =     0.0d0
!
        edge_sph%s_edge =      0.0d0
        edge_sph%as_edge =     0.0d0
      end if
!
      end subroutine alloc_edge_spherical_position
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_vect_sph(edge, edge_sph)
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(inout) :: edge_sph
!
      allocate( edge_sph%edge_vect_sph(edge%numedge,3) )
      if(edge%numedge .gt. 0) edge_sph%edge_vect_sph =     0.0d0
!
      end subroutine alloc_edge_vect_sph
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_vect_cyl(edge, edge_sph)
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(inout) :: edge_sph
!
      allocate( edge_sph%edge_vect_cyl(edge%numedge,3) )
      if(edge%numedge .gt. 0) edge_sph%edge_vect_cyl = 0.0d0
!
      end subroutine alloc_edge_vect_cyl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_spherical_position(edge_sph)
!
      type(edge_position_sph), intent(inout) :: edge_sph
!
      deallocate(edge_sph%r_edge)
      deallocate(edge_sph%theta_edge, edge_sph%phi_edge)
      deallocate(edge_sph%ar_edge, edge_sph%s_edge, edge_sph%as_edge)
!
      end subroutine dealloc_edge_spherical_position
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_vect_sph(edge_sph)
!
      type(edge_position_sph), intent(inout) :: edge_sph
!
      deallocate( edge_sph%edge_vect_sph )
!
      end subroutine dealloc_edge_vect_sph
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_vect_cyl(edge_sph)
!
      type(edge_position_sph), intent(inout) :: edge_sph
!
      deallocate(edge_sph%edge_vect_cyl)
!
      end subroutine dealloc_edge_vect_cyl
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_center_of_edge_sph(edge, edge_sph)
!
      use coordinate_converter
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(inout) :: edge_sph
!
!
      call position_2_sph(edge%numedge, edge%x_edge,                    &
     &    edge_sph%r_edge, edge_sph%theta_edge, edge_sph%phi_edge,      &
     &    edge_sph%ar_edge, edge_sph%s_edge, edge_sph%as_edge)
!
      end subroutine set_center_of_edge_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_edge_vector_spherical(edge, edge_sph)
!
      use cvt_xyz_vector_2_sph_smp
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(inout) :: edge_sph
!
!
!$omp parallel
      call cvt_vector_2_sph_smp                                         &
     &   (np_smp, edge%numedge, edge%istack_edge_smp,                   &
     &    edge%edge_vect, edge%edge_vect_sph,                           &
     &    edge%x_edge(1:edge%numedge,1), edge%x_edge(1:edge%numedge,2), &
     &    edge%x_edge(1:edge%numedge,3), edge%r_edge, edge%s_edge,      &
     &    edge%ar_edge, edge%as_edge)
!$omp end parallel
!
      end subroutine cal_edge_vector_spherical
!
! -----------------------------------------------------------------------
!
      subroutine cal_edge_vector_cylindrical(edge, edge_sph)
!
      use cvt_xyz_vector_2_cyl_smp
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(inout) :: edge_sph
!
!
!$omp parallel
      call cvt_vector_2_cyl_smp                                         &
     &   (np_smp, edge%numedge, edge%istack_edge_smp,                   &
     &    edge%edge_vect, edge_sph%edge_vect_cyl,                       &
     &    edge%x_edge(1:edge%numedge,1), edge%x_edge(1:edge%numedge,2), &
     &    edge%s_edge, edge%as_edge)
!$omp end parallel
!
      end subroutine cal_edge_vector_cylindrical
!
! -----------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO_sph                           &
     &         (edge, edge_sph, nod_IO, sfed_IO)
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(in) :: edge_sph
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: iedge
!
!
      nod_IO%numnod =        edge%numedge
      nod_IO%internal_node = edge%internal_edge
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!omp parallel do
      do iedge = 1, edge%numedge
        nod_IO%inod_global(iedge) = edge%iedge_global(iedge)
        nod_IO%xx(iedge,1) =        edge%r_edge(iedge)
        nod_IO%xx(iedge,2) =        edge_sph%theta_edge(iedge)
        nod_IO%xx(iedge,3) =        edge_sph%phi_edge(iedge)
        sfed_IO%ele_scalar(iedge) = edge%edge_length(iedge)
        sfed_IO%ele_vector(iedge,1) = edge_sph%edge_vect_sph(iedge,1)
        sfed_IO%ele_vector(iedge,2) = edge_sph%edge_vect_sph(iedge,2)
        sfed_IO%ele_vector(iedge,3) = edge_sph%edge_vect_sph(iedge,3)
      end do
!omp end parallel do
!
      end subroutine copy_edge_geometry_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO_cyl                           &
     &         (edge, edge_sph, nod_IO, sfed_IO)
!
      type(edge_data), intent(in) :: edge
      type(edge_position_sph), intent(in) :: edge_sph
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: iedge
!
!
      nod_IO%numnod =        edge%numedge
      nod_IO%internal_node = edge%internal_edge
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!omp parallel do
      do iedge = 1, edge%numedge
        nod_IO%inod_global(iedge) = edge%iedge_global(iedge)
        nod_IO%xx(iedge,1) =        edge%s_edge(iedge)
        nod_IO%xx(iedge,2) =        edge_sph%phi_edge(iedge)
        nod_IO%xx(iedge,3) =        edge%x_edge(iedge,3)
        sfed_IO%ele_scalar(iedge) = edge%edge_length(iedge)
        sfed_IO%ele_vector(iedge,1) = edge_sph%edge_vect_cyl(iedge,1)
        sfed_IO%ele_vector(iedge,2) = edge_sph%edge_vect_cyl(iedge,2)
        sfed_IO%ele_vector(iedge,3) = edge_sph%edge_vect_cyl(iedge,3)
      end do
!omp end parallel do
!
      end subroutine copy_edge_geometry_to_IO_cyl
!
!------------------------------------------------------------------
!
      end module t_edge_data_spherical
