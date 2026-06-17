!>@file   t_vart_edge_width.f90
!!        module t_vart_edge_width
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM shape functgions for vertical filter
!!
!!@verbatim
!!      subroutine alloc_edge_vart_width(numnod, numele, dz_plane)
!!      subroutine dealloc_edge_vart_width(dz_plane)
!!        type(edge_z_width), intent(inout) :: dz_plane
!!@endverbatim
!!
      module t_vart_edge_width
!
      use m_precision
!
      implicit none
!
      type edge_z_width
        real(kind = kreal), allocatable :: delta_z(:)
        real(kind = kreal), allocatable :: delta_dz(:)
        real(kind = kreal), allocatable :: d2_dz(:)
!
        real(kind = kreal), allocatable :: delta_z_e(:)
        real(kind = kreal), allocatable :: delta_dz_e(:)
        real(kind = kreal), allocatable :: d2_dz_e(:)
      end type edge_z_width
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_edge_vart_width(numnod, numele, dz_plane)
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(edge_z_width), intent(inout) :: dz_plane
!
      allocate(dz_plane%delta_z(numnod))
      allocate(dz_plane%delta_dz(numnod))
      allocate(dz_plane%d2_dz(numnod))
!
      if(numnod .gt. 0) then
        dz_plane%delta_z(1:numnod)  = 0.0d0
        dz_plane%delta_dz(1:numnod) = 0.0d0
        dz_plane%d2_dz(1:numnod) =    0.0d0
      end if
!
      allocate(dz_plane%delta_z_e(numele))
      allocate(dz_plane%delta_dz_e(numele))
      allocate(dz_plane%d2_dz_e(numele))
!
      if(numele .gt. 0) then
        dz_plane%delta_z_e(1:numele)  = 0.0d0
        dz_plane%delta_dz_e(1:numele) = 0.0d0
        dz_plane%d2_dz_e(1:numele) =    0.0d0
      end if
!
      end subroutine alloc_edge_vart_width
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_edge_vart_width(dz_plane)
!
      type(edge_z_width), intent(inout) :: dz_plane
!
      deallocate(dz_plane%delta_z,  dz_plane%delta_z_e)
      deallocate(dz_plane%delta_dz, dz_plane%delta_dz_e)
      deallocate(dz_plane%d2_dz,    dz_plane%d2_dz_e)
!
      end subroutine dealloc_edge_vart_width
!
! ----------------------------------------------------------------------
!
      end module t_vart_edge_width
