!>@file   copy_edge_data.f90
!!@brief  module copy_edge_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief Routines to copy edge data
!!
!!@verbatim
!!      subroutine dup_edge_data(org_edge, new_ele, new_surf, new_edge)
!!      subroutine dup_derived_edge_data(org_edge, new_edge)
!!        type(edge_data), intent(in) :: org_edge
!!        type(element_data), intent(in) :: new_ele
!!        type(surface_data), intent(in) :: new_surf
!!        type(edge_data), intent(in) :: new_edge
!!
!!      subroutine copy_edge_connect                                    &
!!     &         (org_edge, numedge, nnod_4_edge, ie_edge)
!!        type(edge_data), intent(in) :: org_edge
!!        integer(kind=kint), intent(in) :: numedge, nnod_4_edge
!!        integer(kind=kint), intent(inout)                             &
!!                           :: ie_edge(numedge,nnod_4_edge)
!!      subroutine copy_edge_to_surface(org_edge, numsurf, iedge_4_sf)
!!        type(edge_data), intent(in) :: org_edge
!!        integer(kind=kint), intent(in) :: numsurf
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: iedge_4_sf(numsurf,nedge_4_surf)
!!      subroutine copy_edge_to_element(org_edge, numele, iedge_4_ele)
!!        type(edge_data), intent(in) :: org_edge
!!        integer(kind=kint), intent(in) :: numele
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: iedge_4_ele(numele,nedge_4_ele)
!!      subroutine copy_global_edge_id                                  &
!!     &         (org_edge, numedge, iedge_global, interior_edge)
!!        type(edge_data), intent(in) :: org_edge
!!        integer(kind=kint), intent(in) ::  numedge
!!        integer(kind=kint_gl), intent(inout) :: iedge_global(numedge)
!!        integer(kind=kint), intent(inout) ::    interior_edge(numedge)
!!      subroutine copy_isolate_edge                                    &
!!     &         (org_edge, numedge_iso, iedge_isolate)
!!        type(edge_data), intent(in) :: org_edge
!!        integer(kind=kint), intent(in) ::  numedge_iso
!!        integer(kind=kint), intent(inout) :: iedge_isolate(numedge_iso)
!!      subroutine copy_edge_geometry(org_edge, numedge, x_edge)
!!        type(edge_data), intent(in) :: org_edge
!!        integer(kind=kint), intent(in) ::  numedge
!!        real (kind=kreal), intent(inout) :: x_edge(numedge,3)
!!      subroutine copy_edge_vectors(org_edge, numedge,                 &
!!     &          edge_length, a_edge_length, edge_vect)
!!        type(edge_data), intent(in) :: org_edge
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
      subroutine dup_edge_data(org_edge, new_ele, new_surf, new_edge)
!
      use set_local_id_table_4_1ele
!
      type(edge_data), intent(in) :: org_edge
      type(element_data), intent(in) :: new_ele
      type(surface_data), intent(in) :: new_surf
!
      type(edge_data), intent(inout) :: new_edge
!
!
      new_edge%numedge = org_edge%numedge
      new_edge%nnod_4_edge = org_edge%nnod_4_edge
      call allocate_inod_in_edge(new_edge)
      call copy_inod_in_edge(new_edge%nnod_4_edge,                      &
     &    new_edge%node_on_edge, new_edge%node_on_edge_sf)
!
      call alloc_edge_connect(new_edge, new_surf%numsurf)
      call copy_edge_connect(org_edge,                                  &
     &    new_edge%numedge, new_edge%nnod_4_edge, new_edge%ie_edge)
      call copy_edge_to_surface(org_edge, new_surf%numsurf,             &
     &                          new_edge%iedge_4_sf)
!
      call alloc_edge_4_ele(new_edge, new_ele%numele)
      call copy_edge_to_element(org_edge, new_ele%numele,               &
     &                          new_edge%iedge_4_ele)
!
      call dup_derived_edge_data(org_edge, new_edge)
!
      end subroutine dup_edge_data
!
!-----------------------------------------------------------------------
!
      subroutine dup_derived_edge_data(org_edge, new_edge)
!
      use set_size_4_smp_types
!
      type(edge_data), intent(in) :: org_edge
!
      type(edge_data), intent(inout) :: new_edge
!
!
      call alloc_edge_param_smp(new_edge)
      call count_edge_size_smp(new_edge)
!
      call alloc_interior_edge(new_edge)
      call copy_global_edge_id(org_edge, new_edge%numedge,              &
     &    new_edge%iedge_global, new_edge%interior_edge)
!
      new_edge%numedge_iso = org_edge%numedge_iso
      call alloc_isolate_edge(new_edge)
      call copy_isolate_edge                                            &
     &   (org_edge, new_edge%numedge_iso, new_edge%iedge_isolate)
!
      call alloc_edge_geometory(new_edge)
      call copy_edge_geometry(org_edge, new_edge%numedge,               &
     &                        new_edge%x_edge)
!
      call alloc_edge_vect(new_edge)
      call copy_edge_vectors                                            &
     &   (org_edge, new_edge%numedge, new_edge%edge_length,             &
     &    new_edge%a_edge_length, new_edge%edge_vect)
!
      end subroutine dup_derived_edge_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_edge_connect                                      &
     &         (org_edge, numedge, nnod_4_edge, ie_edge)
!
      type(edge_data), intent(in) :: org_edge
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
        ie_edge(1:numedge,k1) =   org_edge%ie_edge(1:numedge,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_edge_connect
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_to_surface(org_edge, numsurf, iedge_4_sf)
!
      type(edge_data), intent(in) :: org_edge
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
        iedge_4_sf(1:numsurf,k1) =   org_edge%iedge_4_sf(1:numsurf,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_edge_to_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_to_element(org_edge, numele, iedge_4_ele)
!
      type(edge_data), intent(in) :: org_edge
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
        iedge_4_ele(1:numele,k1) =   org_edge%iedge_4_ele(1:numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_edge_to_element
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_edge_id                                    &
     &         (org_edge, numedge, iedge_global, interior_edge)
!
      type(edge_data), intent(in) :: org_edge
      integer(kind=kint), intent(in) ::  numedge
      integer(kind=kint_gl), intent(inout) :: iedge_global(numedge)
      integer(kind=kint), intent(inout) ::    interior_edge(numedge)
!
!
      if(numedge .le. 0) return
!$omp parallel workshare
      iedge_global(1:numedge) =   org_edge%iedge_global(1:numedge)
      interior_edge(1:numedge) = org_edge%interior_edge(1:numedge)
!$omp end parallel workshare
!
      end subroutine copy_global_edge_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_isolate_edge                                      &
     &         (org_edge, numedge_iso, iedge_isolate)
!
      type(edge_data), intent(in) :: org_edge
      integer(kind=kint), intent(in) ::  numedge_iso
      integer(kind=kint), intent(inout) :: iedge_isolate(numedge_iso)
!
!
      if(numedge_iso .le. 0) return
!$omp parallel workshare
      iedge_isolate(1:numedge_iso)                                      &
     &      = org_edge%iedge_isolate(1:numedge_iso)
!$omp end parallel workshare
!
      end subroutine copy_isolate_edge
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_geometry(org_edge, numedge, x_edge)
!
      type(edge_data), intent(in) :: org_edge
      integer(kind=kint), intent(in) ::  numedge
      real (kind=kreal), intent(inout) :: x_edge(numedge,3)
!
!
      if(numedge .le. 0) return
!$omp parallel workshare
      x_edge(1:numedge,1) = org_edge%x_edge(1:numedge,1)
      x_edge(1:numedge,2) = org_edge%x_edge(1:numedge,2)
      x_edge(1:numedge,3) = org_edge%x_edge(1:numedge,3)
!$omp end parallel workshare
!
      end subroutine copy_edge_geometry
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_vectors(org_edge, numedge,                   &
     &          edge_length, a_edge_length, edge_vect)
!
      type(edge_data), intent(in) :: org_edge
      integer(kind=kint), intent(in) ::  numedge
      real (kind=kreal), intent(inout) :: edge_length(numedge)
      real (kind=kreal), intent(inout) :: a_edge_length(numedge)
      real (kind=kreal), intent(inout) :: edge_vect(numedge,3)
!
!
      if(numedge .le. 0) return
!$omp parallel workshare
      edge_length(1:numedge) =    org_edge%edge_length(1:numedge)
      a_edge_length(1:numedge) =  org_edge%a_edge_length(1:numedge)
      edge_vect(1:numedge,1) =    org_edge%edge_vect(1:numedge,1)
      edge_vect(1:numedge,2) =    org_edge%edge_vect(1:numedge,2)
      edge_vect(1:numedge,3) =    org_edge%edge_vect(1:numedge,3)
!$omp end parallel workshare
!
      end subroutine copy_edge_vectors
!
!-----------------------------------------------------------------------
!
      end module copy_edge_data
