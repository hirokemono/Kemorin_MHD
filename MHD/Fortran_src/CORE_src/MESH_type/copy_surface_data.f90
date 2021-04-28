!>@file   copy_surface_data.f90
!!@brief  module copy_surface_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief Routines to copy surface data
!!
!!@verbatim
!!      subroutine dup_surface_data(surf_org, ele_new, surf_new)
!!      subroutine dup_derived_surface_data(surf_org, surf_new)
!!        type(surface_data), intent(in) :: surf_org
!!        type(element_data), intent(in) :: ele_new
!!        type(surface_data), intent(in) :: surf_new
!!
!!      subroutine copy_surface_connect                                 &
!!     &         (surf_org, numsurf, nnod_4_surf, ie_surf)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) :: numsurf, nnod_4_surf
!!        integer(kind=kint), intent(inout)                             &
!!                           :: ie_surf(numsurf,nnod_4_surf)
!!      subroutine copy_surface_to_element                              &
!!     &         (surf_org, numele, isf_4_ele, isf_rot_ele)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) :: numele
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: isf_4_ele(numele,nsurf_4_ele)
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: isf_rot_ele(numele,nsurf_4_ele)
!!      subroutine copy_global_surface_id                               &
!!     &         (surf_org, numsurf, isurf_global, interior_surf)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) ::  numsurf
!!        integer(kind=kint_gl), intent(inout) :: isurf_global(numsurf)
!!        integer(kind=kint), intent(inout) ::    interior_surf(numsurf)
!!      subroutine copy_ext_surface                                     &
!!     &         (surf_org, numsurf_ext, isf_external)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) ::  numsurf_ext
!!        integer(kind=kint), intent(inout) :: isf_external(numsurf_ext)
!!      subroutine copy_isolate_surface                                 &
!!     &         (surf_org, numsurf_iso, isf_isolate)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) ::  numsurf_iso
!!        integer(kind=kint), intent(inout) :: isf_isolate(numsurf_iso)
!!      subroutine copy_element_4_surface                               &
!!     &         (surf_org, numsurf, iele_4_surf)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) ::  numsurf
!!        integer(kind=kint), intent(inout) :: iele_4_surf(numsurf,2,2)
!!      subroutine copy_surface_geometry(surf_org, numsurf, x_surf)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) ::  numsurf
!!        real (kind=kreal), intent(inout) :: x_surf(numsurf,3)
!!      subroutine copy_normal_vectors(surf_org, numsurf,               &
!!     &          area_surf, a_area_surf, vnorm_surf)
!!        type(surface_data), intent(in) :: surf_org
!!        integer(kind=kint), intent(in) ::  numsurf
!!        real (kind=kreal), intent(inout) :: area_surf(numsurf)
!!        real (kind=kreal), intent(inout) :: a_area_surf(numsurf)
!!        real (kind=kreal), intent(inout) :: vnorm_surf(numsurf,3)
!!@endverbatim
!
      module copy_surface_data
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dup_surface_data(surf_org, ele_new, surf_new)
!
      use set_local_id_table_4_1ele
      use set_size_4_smp_types
!
      type(surface_data), intent(in) :: surf_org
      type(element_data), intent(in) :: ele_new
!
      type(surface_data), intent(inout) :: surf_new
!
!
      surf_new%numsurf = surf_org%numsurf
      surf_new%nnod_4_surf = surf_org%nnod_4_surf
      call allocate_inod_in_surf(surf_new)
      call set_inod_in_surf(surf_new%nnod_4_surf,                       &
     &                      surf_new%node_on_sf, surf_new%node_on_sf_n)
!
      call alloc_surface_connect(surf_new, ele_new%numele)
      call copy_surface_connect(surf_org,                               &
     &    surf_new%numsurf, surf_new%nnod_4_surf, surf_new%ie_surf)
      call copy_surface_to_element (surf_org, ele_new%numele,           &
     &    surf_new%isf_4_ele, surf_new%isf_rot_ele)
!
      call dup_derived_surface_data(surf_org, surf_new)
!
      end subroutine dup_surface_data
!
!-----------------------------------------------------------------------
!
      subroutine dup_derived_surface_data(surf_org, surf_new)
!
      use set_size_4_smp_types
!
      type(surface_data), intent(in) :: surf_org
!
      type(surface_data), intent(inout) :: surf_new
!
!
      call alloc_surf_param_smp(surf_new)
      call count_surf_size_smp(surf_new)
!
      call alloc_interior_surf(surf_new)
      call copy_global_surface_id(surf_org, surf_new%numsurf,           &
     &    surf_new%isurf_global, surf_new%interior_surf)
!
      surf_new%numsurf_ext = surf_org%numsurf_ext
      call alloc_ext_surface(surf_new)
      call copy_ext_surface                                             &
     &   (surf_org, surf_new%numsurf_ext, surf_new%isf_external)
!
      surf_new%numsurf_iso = surf_org%numsurf_iso
      call alloc_iso_surface(surf_new)
      call copy_isolate_surface                                         &
     &   (surf_org, surf_new%numsurf_iso, surf_new%isf_isolate)
!
      call alloc_element_4_surface(surf_new)
      call copy_element_4_surface(surf_org, surf_new%numsurf,           &
     &                           surf_new%iele_4_surf)
!
      call alloc_surface_geometory(surf_new)
      call copy_surface_geometry(surf_org, surf_new%numsurf,            &
     &                           surf_new%x_surf)
!
      call alloc_normal_vector(surf_new)
      call copy_normal_vectors(surf_org, surf_new%numsurf,              &
     &   surf_new%area_surf, surf_new%a_area_surf, surf_new%vnorm_surf)
!
      end subroutine dup_derived_surface_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_surface_connect                                   &
     &         (surf_org, numsurf, nnod_4_surf, ie_surf)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) :: numsurf, nnod_4_surf
!
      integer(kind=kint), intent(inout)                                 &
     &                   :: ie_surf(numsurf,nnod_4_surf)
!
      integer(kind = kint) :: k1
!
      if(numsurf .le. 0) return
!$omp parallel private(k1)
      do k1 = 1, nnod_4_surf
!$omp workshare
        ie_surf(1:numsurf,k1) =   surf_org%ie_surf(1:numsurf,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_surface_connect
!
!-----------------------------------------------------------------------
!
      subroutine copy_surface_to_element                                &
     &         (surf_org, numele, isf_4_ele, isf_rot_ele)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) :: numele
!
      integer(kind=kint), intent(inout)                                 &
     &                   :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind=kint), intent(inout)                                 &
     &                   :: isf_rot_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: k1
!
      if(numele .le. 0) return
!$omp parallel private(k1)
      do k1 = 1, nsurf_4_ele
!$omp workshare
        isf_4_ele(1:numele,k1) =   surf_org%isf_4_ele(1:numele,k1)
        isf_rot_ele(1:numele,k1) = surf_org%isf_rot_ele(1:numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_surface_to_element
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_surface_id                                 &
     &         (surf_org, numsurf, isurf_global, interior_surf)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) ::  numsurf
      integer(kind=kint_gl), intent(inout) :: isurf_global(numsurf)
      integer(kind=kint), intent(inout) ::    interior_surf(numsurf)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      isurf_global(1:numsurf) =   surf_org%isurf_global(1:numsurf)
      interior_surf(1:numsurf) = surf_org%interior_surf(1:numsurf)
!$omp end parallel workshare
!
      end subroutine copy_global_surface_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_ext_surface                                       &
     &         (surf_org, numsurf_ext, isf_external)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) ::  numsurf_ext
      integer(kind=kint), intent(inout) :: isf_external(numsurf_ext)
!
!
      if(numsurf_ext .le. 0) return
!$omp parallel workshare
      isf_external(1:numsurf_ext)                                       &
     &        = surf_org%isf_external(1:numsurf_ext)
!$omp end parallel workshare
!
      end subroutine copy_ext_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_isolate_surface                                   &
     &         (surf_org, numsurf_iso, isf_isolate)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) ::  numsurf_iso
      integer(kind=kint), intent(inout) :: isf_isolate(numsurf_iso)
!
!
      if(numsurf_iso .le. 0) return
!$omp parallel workshare
      isf_isolate(1:numsurf_iso) = surf_org%isf_isolate(1:numsurf_iso)
!$omp end parallel workshare
!
      end subroutine copy_isolate_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_element_4_surface                                 &
     &         (surf_org, numsurf, iele_4_surf)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) ::  numsurf
      integer(kind=kint), intent(inout) :: iele_4_surf(numsurf,2,2)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      iele_4_surf(1:numsurf,1,1) = surf_org%iele_4_surf(1:numsurf,1,1)
      iele_4_surf(1:numsurf,2,1) = surf_org%iele_4_surf(1:numsurf,2,1)
      iele_4_surf(1:numsurf,1,2) = surf_org%iele_4_surf(1:numsurf,1,2)
      iele_4_surf(1:numsurf,2,2) = surf_org%iele_4_surf(1:numsurf,1,2)
!$omp end parallel workshare
!
      end subroutine copy_element_4_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_surface_geometry(surf_org, numsurf, x_surf)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) ::  numsurf
      real (kind=kreal), intent(inout) :: x_surf(numsurf,3)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      x_surf(1:numsurf,1) = surf_org%x_surf(1:numsurf,1)
      x_surf(1:numsurf,2) = surf_org%x_surf(1:numsurf,2)
      x_surf(1:numsurf,3) = surf_org%x_surf(1:numsurf,3)
!$omp end parallel workshare
!
      end subroutine copy_surface_geometry
!
!-----------------------------------------------------------------------
!
      subroutine copy_normal_vectors(surf_org, numsurf,                 &
     &          area_surf, a_area_surf, vnorm_surf)
!
      type(surface_data), intent(in) :: surf_org
      integer(kind=kint), intent(in) ::  numsurf
      real (kind=kreal), intent(inout) :: area_surf(numsurf)
      real (kind=kreal), intent(inout) :: a_area_surf(numsurf)
      real (kind=kreal), intent(inout) :: vnorm_surf(numsurf,3)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      area_surf(1:numsurf) =    surf_org%area_surf(1:numsurf)
      a_area_surf(1:numsurf) =  surf_org%a_area_surf(1:numsurf)
      vnorm_surf(1:numsurf,1) = surf_org%vnorm_surf(1:numsurf,1)
      vnorm_surf(1:numsurf,2) = surf_org%vnorm_surf(1:numsurf,2)
      vnorm_surf(1:numsurf,3) = surf_org%vnorm_surf(1:numsurf,3)
!$omp end parallel workshare
!
      end subroutine copy_normal_vectors
!
!-----------------------------------------------------------------------
!
      end module copy_surface_data
