!>@file   copy_edge_data.f90
!!@brief  module copy_edge_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief Routines to copy edge data
!!
!!@verbatim
!!      subroutine dup_edge_data(edge_org, ele_new, surf_new, edge_new)
!!      subroutine dup_derived_edge_data(edge_org, edge_new)
!!        type(edge_data), intent(in) :: edge_org
!!        type(element_data), intent(in) :: ele_new
!!        type(surface_data), intent(in) :: surf_new
!!        type(edge_data), intent(in) :: edge_new
!!
!!      subroutine copy_edge_connect                                    &
!!     &         (edge_org, numedge, nnod_4_edge, ie_edge)
!!        type(edge_data), intent(in) :: edge_org
!!        integer(kind=kint), intent(in) :: numedge, nnod_4_edge
!!        integer(kind=kint), intent(inout)                             &
!!                           :: ie_edge(numedge,nnod_4_edge)
!!      subroutine copy_edge_to_surface(edge_org, numsurf, iedge_4_sf)
!!        type(edge_data), intent(in) :: edge_org
!!        integer(kind=kint), intent(in) :: numsurf
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: iedge_4_sf(numsurf,nedge_4_surf)
!!      subroutine copy_edge_to_element(edge_org, numele, iedge_4_ele)
!!        type(edge_data), intent(in) :: edge_org
!!        integer(kind=kint), intent(in) :: numele
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: iedge_4_ele(numele,nedge_4_ele)
!!      subroutine copy_global_edge_id                                  &
!!     &         (edge_org, numedge, iedge_global, interior_edge)
!!        type(edge_data), intent(in) :: edge_org
!!        integer(kind=kint), intent(in) ::  numedge
!!        integer(kind=kint_gl), intent(inout) :: iedge_global(numedge)
!!        integer(kind=kint), intent(inout) ::    interior_edge(numedge)
!!      subroutine copy_isolate_edge                                    &
!!     &         (edge_org, numedge_iso, iedge_isolate)
!!        type(edge_data), intent(in) :: edge_org
!!        integer(kind=kint), intent(in) ::  numedge_iso
!!        integer(kind=kint), intent(inout) :: iedge_isolate(numedge_iso)
!!      subroutine copy_edge_geometry(edge_org, numedge, x_edge)
!!        type(edge_data), intent(in) :: edge_org
!!        integer(kind=kint), intent(in) ::  numedge
!!        real (kind=kreal), intent(inout) :: x_edge(numedge,3)
!!      subroutine copy_edge_vectors(edge_org, numedge,                 &
!!     &          edge_length, a_edge_length, edge_vect)
!!        type(edge_data), intent(in) :: edge_org
!!        integer(kind=kint), intent(in) ::  numedge
!!        real (kind=kreal), intent(inout) :: edge_length(numedge)
!!        real (kind=kreal), intent(inout) :: a_edge_length(numedge)
!!        real (kind=kreal), intent(inout) :: edge_vect(numedge,3)
!!@endverbatim
!
      module copy_edge_data
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dup_edge_data(edge_org, ele_new, surf_new, edge_new)
!
      use set_local_id_table_4_1ele
      use set_size_4_smp_types
!
      type(edge_data), intent(in) :: edge_org
      type(element_data), intent(in) :: ele_new
      type(surface_data), intent(in) :: surf_new
!
      type(edge_data), intent(inout) :: edge_new
!
!
      edge_new%numedge = edge_org%numedge
      edge_new%nnod_4_edge = edge_org%nnod_4_edge
      call allocate_inod_in_edge(edge_new)
      call copy_inod_in_edge(edge_new%nnod_4_edge,                      &
     &    edge_new%node_on_edge, edge_new%node_on_edge_sf)
!
      call alloc_edge_connect(edge_new, surf_new%numsurf)
      call copy_edge_connect(edge_org,                                  &
     &    edge_new%numedge, edge_new%nnod_4_edge, edge_new%ie_edge)
      call copy_edge_to_surface(edge_org, surf_new%numsurf,             &
     &                          edge_new%iedge_4_sf)
!
      call alloc_edge_4_ele(edge_new, ele_new%numele)
      call copy_edge_to_element(edge_org, ele_new%numele,               &
     &                          edge_new%iedge_4_ele)
!
      call dup_derived_edge_data(edge_org, edge_new)
!
      end subroutine dup_edge_data
!
!-----------------------------------------------------------------------
!
      subroutine dup_derived_edge_data(edge_org, edge_new)
!
      use set_size_4_smp_types
!
      type(edge_data), intent(in) :: edge_org
!
      type(edge_data), intent(inout) :: edge_new
!
!
      call alloc_edge_param_smp(edge_new)
      call count_edge_size_smp(edge_new)
!
      call alloc_interior_edge(edge_new)
      call copy_global_edge_id(edge_org, edge_new%numedge,              &
     &    edge_new%iedge_global, edge_new%interior_edge)
!
      edge_new%numedge_iso = edge_org%numedge_iso
      call alloc_isolate_edge(edge_new)
      call copy_isolate_edge                                            &
     &   (edge_org, edge_new%numedge_iso, edge_new%iedge_isolate)
!
      call alloc_edge_geometory(edge_new)
      call copy_edge_geometry(edge_org, edge_new%numedge,               &
     &                        edge_new%x_edge)
!
      call alloc_edge_vect(edge_new)
      call copy_edge_vectors                                            &
     &   (edge_org, edge_new%numedge, edge_new%edge_length,             &
     &    edge_new%a_edge_length, edge_new%edge_vect)
!
      end subroutine dup_derived_edge_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_edge_connect                                      &
     &         (edge_org, numedge, nnod_4_edge, ie_edge)
!
      type(edge_data), intent(in) :: edge_org
      integer(kind=kint), intent(in) :: numedge, nnod_4_edge
!
      integer(kind=kint), intent(inout)                                 &
     &                   :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint) :: k1
!
      if(numedge .le. 0) return
!$omp parallel private(k1)
      do k1 = 1, nnod_4_edge
!$omp workshare
        ie_edge(1:numedge,k1) =   edge_org%ie_edge(1:numedge,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_edge_connect
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_to_surface(edge_org, numsurf, iedge_4_sf)
!
      type(edge_data), intent(in) :: edge_org
      integer(kind=kint), intent(in) :: numsurf
!
      integer(kind=kint), intent(inout)                                 &
     &                   :: iedge_4_sf(numsurf,nedge_4_surf)
!
      integer(kind = kint) :: k1
!
      if(numsurf .le. 0) return
!$omp parallel private(k1)
      do k1 = 1, nedge_4_surf
!$omp workshare
        iedge_4_sf(1:numsurf,k1) =   edge_org%iedge_4_sf(1:numsurf,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_edge_to_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_to_element(edge_org, numele, iedge_4_ele)
!
      type(edge_data), intent(in) :: edge_org
      integer(kind=kint), intent(in) :: numele
!
      integer(kind=kint), intent(inout)                                 &
     &                   :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint) :: k1
!
      if(numele .le. 0) return
!$omp parallel private(k1)
      do k1 = 1, nedge_4_ele
!$omp workshare
        iedge_4_ele(1:numele,k1) =   edge_org%iedge_4_ele(1:numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_edge_to_element
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_edge_id                                    &
     &         (edge_org, numedge, iedge_global, interior_edge)
!
      type(edge_data), intent(in) :: edge_org
      integer(kind=kint), intent(in) ::  numedge
      integer(kind=kint_gl), intent(inout) :: iedge_global(numedge)
      integer(kind=kint), intent(inout) ::    interior_edge(numedge)
!
!
      if(numedge .le. 0) return
!$omp parallel workshare
      iedge_global(1:numedge) =   edge_org%iedge_global(1:numedge)
      interior_edge(1:numedge) = edge_org%interior_edge(1:numedge)
!$omp end parallel workshare
!
      end subroutine copy_global_edge_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_isolate_edge                                      &
     &         (edge_org, numedge_iso, iedge_isolate)
!
      type(edge_data), intent(in) :: edge_org
      integer(kind=kint), intent(in) ::  numedge_iso
      integer(kind=kint), intent(inout) :: iedge_isolate(numedge_iso)
!
!
      if(numedge_iso .le. 0) return
!$omp parallel workshare
      iedge_isolate(1:numedge_iso)                                      &
     &      = edge_org%iedge_isolate(1:numedge_iso)
!$omp end parallel workshare
!
      end subroutine copy_isolate_edge
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_geometry(edge_org, numedge, x_edge)
!
      type(edge_data), intent(in) :: edge_org
      integer(kind=kint), intent(in) ::  numedge
      real (kind=kreal), intent(inout) :: x_edge(numedge,3)
!
!
      if(numedge .le. 0) return
!$omp parallel workshare
      x_edge(1:numedge,1) = edge_org%x_edge(1:numedge,1)
      x_edge(1:numedge,2) = edge_org%x_edge(1:numedge,2)
      x_edge(1:numedge,3) = edge_org%x_edge(1:numedge,3)
!$omp end parallel workshare
!
      end subroutine copy_edge_geometry
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_vectors(edge_org, numedge,                   &
     &          edge_length, a_edge_length, edge_vect)
!
      type(edge_data), intent(in) :: edge_org
      integer(kind=kint), intent(in) ::  numedge
      real (kind=kreal), intent(inout) :: edge_length(numedge)
      real (kind=kreal), intent(inout) :: a_edge_length(numedge)
      real (kind=kreal), intent(inout) :: edge_vect(numedge,3)
!
!
      if(numedge .le. 0) return
!$omp parallel workshare
      edge_length(1:numedge) =    edge_org%edge_length(1:numedge)
      a_edge_length(1:numedge) =  edge_org%a_edge_length(1:numedge)
      edge_vect(1:numedge,1) =    edge_org%edge_vect(1:numedge,1)
      edge_vect(1:numedge,2) =    edge_org%edge_vect(1:numedge,2)
      edge_vect(1:numedge,3) =    edge_org%edge_vect(1:numedge,3)
!$omp end parallel workshare
!
      end subroutine copy_edge_vectors
!
!-----------------------------------------------------------------------
!
      end module copy_edge_data
