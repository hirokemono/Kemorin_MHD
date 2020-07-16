!>@file   t_surf_data_spherical.f90
!!@brief  module t_surf_data_spherical
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief surface position data in spherical coordinate
!!
!!@verbatim
!!      subroutine alloc_surf_spherical_position(edge, edge_sph)
!!      subroutine allocate_normal_vect_sph_type(surf, surf_sph)
!!      subroutine allocate_normal_vect_cyl_type(surf, surf_sph)
!!        type(surface_data), intent(in) :: surf
!!        type(surface_position_sph), intent(inout) :: surf_sph
!!
!!      subroutine dealloc_surf_spherical_position(surf_sph)
!!      subroutine deallocate_normal_vect_sph_type(surf_sph)
!!      subroutine deallocate_normal_vect_cyl_type(surf_sph)
!!        type(surface_position_sph), intent(inout) :: surf_sph
!!
!!      subroutine set_center_of_surf_sph(surf, surf_sph)
!!        type(surface_data), intent(in) :: surf
!!        type(surface_position_sph), intent(inout) :: surf_sph
!!
!!      subroutine cal_normal_vector_spherical(surf, surf_sph)
!!      subroutine cal_normal_vector_cylindrical(surf, surf_sph)
!!        type(surface_data), intent(in) :: surf
!!        type(surface_position_sph), intent(inout) :: surf_sph
!!      subroutine copy_surf_geometry_to_IO_sph                         &
!!     &         (surf, surf_sph, nod_IO, sfed_IO)
!!      subroutine copy_surf_geometry_to_IO_cyl                      &
!!     &         (surf, surf_sph, nod_IO, sfed_IO)
!!        type(surface_data), intent(inout) :: surf
!!        type(surface_position_sph), intent(in) :: surf_sph
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module t_surf_data_spherical
!
      use m_precision
!
      use t_surface_data
      use t_surf_edge_IO
      use t_read_mesh_data
!
      implicit none
!
!>     Structure of edge popsition data
      type surface_position_sph
!>       distance from the center of surface
        real(kind=kreal)  , allocatable  :: r_surf(:)
!>       1/r_surf
        real(kind=kreal)  , allocatable  :: ar_surf(:)
!>       colatitude of center of surface
        real(kind=kreal)  , allocatable  :: theta_surf(:)
!>       longitude of center of surface
        real(kind=kreal)  , allocatable  :: phi_surf(:)
!>       cylindorical radius of center of surface
        real(kind=kreal)  , allocatable  :: s_surf(:)
!>       1 / s_surf
        real(kind=kreal)  , allocatable  :: as_surf(:)
!
!>       normal vector for sach surface (spherical coordinate)
        real (kind=kreal), allocatable :: vnorm_surf_sph(:,:)
!>       normal vector for sach surface (cylindrical coordinate)
        real (kind=kreal), allocatable :: vnorm_surf_cyl(:,:)
      end type surface_position_sph
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine alloc_surf_spherical_position(edge, edge_sph)
!
      type(surface_data), intent(integer) :: surf
      type(surface_position_sph), intent(inout) :: surf_sph
!
!
      allocate(surf_sph%x_surf(surf%numsurf,3))
!
      allocate(surf_sph%r_surf(surf%numsurf))
      allocate(surf_sph%ar_surf(surf%numsurf))
      allocate(surf_sph%phi_surf(surf%numsurf))
      allocate(surf_sph%theta_surf(surf%numsurf))
!
      allocate(surf_sph%s_surf(surf%numsurf))
      allocate(surf_sph%as_surf(surf%numsurf))
!
      if(surf%numsurf .gt. 0) then
        surf_sph%x_surf =      0.0d0
!
        surf_sph%r_surf =      0.0d0
        surf_sph%ar_surf =     0.0d0
        surf_sph%phi_surf =    0.0d0
        surf_sph%theta_surf =  0.0d0
!
        surf_sph%s_surf =      0.0d0
        surf_sph%as_surf =     0.0d0
      end if
!
      end subroutine alloc_surf_spherical_position
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_normal_vect_sph_type(surf, surf_sph)
!
      type(surface_data), intent(integer) :: surf
      type(surface_position_sph), intent(inout) :: surf_sph
!
      allocate( surf_sph%vnorm_surf_sph(surf%numsurf,3) )
      if ( surf%numsurf .gt. 0 ) surf_sph%vnorm_surf_sph =  0.0d0
!
      end subroutine allocate_normal_vect_sph_type
!
!------------------------------------------------------------------
!
      subroutine allocate_normal_vect_cyl_type(surf, surf_sph)
!
      type(surface_data), intent(in) :: surf
      type(surface_position_sph), intent(inout) :: surf_sph
!
      allocate( surf_sph%vnorm_surf_cyl(surf%numsurf,3) )
      if ( surf%numsurf .gt. 0 ) surf_sph%vnorm_surf_cyl =  0.0d0
!
      end subroutine allocate_normal_vect_cyl_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surf_spherical_position(surf_sph)
!
      type(surface_position_sph), intent(inout) :: surf_sph
!
      deallocate(surf_sph%r_surf, surf_sph%ar_surf)
      deallocate(surf_sph%phi_surf, surf_sph%theta_surf)
      deallocate(surf_sph%s_surf, surf_sph%as_surf)
!
      end subroutine dealloc_surf_spherical_position
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_normal_vect_sph_type(surf_sph)
!
      type(surface_position_sph), intent(inout) :: surf_sph
!
      deallocate( surf_sph%vnorm_surf_sph )
!
      end subroutine deallocate_normal_vect_sph_type
!
! ------------------------------------------------------
!
      subroutine deallocate_normal_vect_cyl_type(surf_sph)
!
      type(surface_position_sph), intent(inout) :: surf_sph
!
      deallocate( surf_sph%vnorm_surf_cyl )
!
      end subroutine deallocate_normal_vect_cyl_type
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_center_of_surf_sph(surf, surf_sph)
!
      use coordinate_converter
!
      type(surface_data), intent(in) :: surf
      type(surface_position_sph), intent(inout) :: surf_sph
!
!
      call position_2_sph(surf%numsurf, surf%x_surf,                    &
     &    surf_sph%r_surf, surf_sph%theta_surf, surf_sph%phi_surf,      &
     &    surf_sph%ar_surf, surf_sph%s_surf, surf_sph%as_surf)
!
      end subroutine set_center_of_surf_sph
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_normal_vector_spherical(surf, surf_sph)
!
      use cvt_xyz_vector_2_sph_smp
!
      type(surface_data), intent(in) :: surf
      type(surface_position_sph), intent(inout) :: surf_sph
!
!
!$omp parallel
      call cvt_vector_2_sph_smp                                         &
     &   (np_smp, surf%numsurf, surf%istack_surf_smp,                   &
     &    surf%vnorm_surf, surf_sph%vnorm_surf_sph,                     &
     &    surf%x_surf(1:surf%numsurf,1), surf%x_surf(1:surf%numsurf,2), &
     &    surf%x_surf(1:surf%numsurf,3), surf%r_surf, surf%s_surf,      &
     &    surf%ar_surf, surf%as_surf)
!$omp end parallel
!
      end subroutine cal_normal_vector_spherical
!
! -----------------------------------------------------------------------
!
      subroutine cal_normal_vector_cylindrical(surf, surf_sph)
!
      use cvt_xyz_vector_2_cyl_smp
!
      type(surface_data), intent(in) :: surf
      type(surface_position_sph), intent(inout) :: surf_sph
!
!
!$omp parallel
      call cvt_vector_2_cyl_smp                                         &
     &   (np_smp, surf%numsurf, surf%istack_surf_smp,                   &
     &    surf%vnorm_surf, surf_sph%vnorm_surf_cyl,                     &
     &    surf%x_surf(1:surf%numsurf,1), surf%x_surf(1:surf%numsurf,2), &
     &    surf%s_surf, surf%as_surf)
!$omp end parallel
!
      end subroutine cal_normal_vector_cylindrical
!
! -----------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO_sph                           &
     &         (surf, surf_sph, nod_IO, sfed_IO)
!
      type(surface_data), intent(in) :: surf
      type(surface_position_sph), intent(in) :: surf_sph
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: isurf
!
!
      nod_IO%numnod =        surf%numsurf
      nod_IO%internal_node = surf%internal_surf
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!omp parallel do
      do isurf = 1, surf%numsurf
        nod_IO%inod_global(isurf) = surf%isurf_global(isurf)
        nod_IO%xx(isurf,1) = surf%r_surf(isurf)
        nod_IO%xx(isurf,2) = surf%theta_surf(isurf)
        nod_IO%xx(isurf,3) = surf%phi_surf(isurf)
!
        sfed_IO%ele_scalar(isurf) =   surf%area_surf(isurf)
        sfed_IO%ele_vector(isurf,1) = surf_sph%vnorm_surf_sph(isurf,1)
        sfed_IO%ele_vector(isurf,2) = surf_sph%vnorm_surf_sph(isurf,2)
        sfed_IO%ele_vector(isurf,3) = surf_sph%vnorm_surf_sph(isurf,3)
      end do
!omp end parallel do
!
      end subroutine copy_surf_geometry_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO_cyl                      &
     &         (surf, surf_sph, nod_IO, sfed_IO)
!
      type(surface_data), intent(in) :: surf
      type(surface_position_sph), intent(in) :: surf_sph
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: isurf
!
!
      nod_IO%numnod =        surf%numsurf
      nod_IO%internal_node = surf%internal_surf
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!omp parallel do
      do isurf = 1, surf%numsurf
        nod_IO%inod_global(isurf) = surf%isurf_global(isurf)
        nod_IO%xx(isurf,1) = surf%s_surf(isurf)
        nod_IO%xx(isurf,2) = surf%phi_surf(isurf)
        nod_IO%xx(isurf,3) = surf%x_surf(isurf,3)
        sfed_IO%ele_scalar(isurf) =   surf%area_surf(isurf)
        sfed_IO%ele_vector(isurf,1) = surf_sph%vnorm_surf_cyl(isurf,1)
        sfed_IO%ele_vector(isurf,2) = surf_sph%vnorm_surf_cyl(isurf,2)
        sfed_IO%ele_vector(isurf,3) = surf_sph%vnorm_surf_cyl(isurf,3)
      end do
!omp end parallel do
!
      end subroutine copy_surf_geometry_to_IO_cyl
!
!------------------------------------------------------------------
!
      end module t_surf_data_spherical
