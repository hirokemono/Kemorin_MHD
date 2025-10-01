!>@file   copy_surface_data.f90
!!@brief  module copy_surface_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief Routines to copy surface data
!!
!!@verbatim
!!      subroutine dup_surface_data(org_surf, new_ele, new_surf)
!!      subroutine dup_derived_surface_data(org_surf, new_surf)
!!        type(surface_data), intent(in) :: org_surf
!!        type(element_data), intent(in) :: new_ele
!!        type(surface_data), intent(in) :: new_surf
!!
!!      subroutine copy_surface_connect                                 &
!!     &         (org_surf, numsurf, nnod_4_surf, ie_surf)
!!        type(surface_data), intent(in) :: org_surf
!!        integer(kind=kint), intent(in) :: numsurf, nnod_4_surf
!!        integer(kind=kint), intent(inout)                             &
!!                           :: ie_surf(numsurf,nnod_4_surf)
!!      subroutine copy_surface_to_element                              &
!!     &         (org_surf, numele, isf_4_ele, isf_rot_ele)
!!        type(surface_data), intent(in) :: org_surf
!!        integer(kind=kint), intent(in) :: numele
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: isf_4_ele(numele,nsurf_4_ele)
!!        integer(kind=kint), intent(inout)                             &
!!     &                   :: isf_rot_ele(numele,nsurf_4_ele)
!!      subroutine copy_global_surface_id                               &
!!     &         (org_surf, numsurf, isurf_global, interior_surf)
!!        type(surface_data), intent(in) :: org_surf
!!        integer(kind=kint), intent(in) ::  numsurf
!!        integer(kind=kint_gl), intent(inout) :: isurf_global(numsurf)
!!        integer(kind=kint), intent(inout) ::    interior_surf(numsurf)
!!      subroutine copy_ext_surface                                     &
!!     &         (org_surf, numsurf_ext, isf_external)
!!        type(surface_data), intent(in) :: org_surf
!!        integer(kind=kint), intent(in) ::  numsurf_ext
!!        integer(kind=kint), intent(inout) :: isf_external(numsurf_ext)
!!      subroutine copy_isolate_surface                                 &
!!     &         (org_surf, numsurf_iso, isf_isolate)
!!        type(surface_data), intent(in) :: org_surf
!!        integer(kind=kint), intent(in) ::  numsurf_iso
!!        integer(kind=kint), intent(inout) :: isf_isolate(numsurf_iso)
!!      subroutine copy_element_4_surface                               &
!!     &         (org_surf, numsurf, iele_4_surf)
!!        type(surface_data), intent(in) :: org_surf
!!        integer(kind=kint), intent(in) ::  numsurf
!!        integer(kind=kint), intent(inout) :: iele_4_surf(numsurf,2,2)
!!      subroutine copy_surface_geometry(org_surf, numsurf, x_surf)
!!        type(surface_data), intent(in) :: org_surf
!!        integer(kind=kint), intent(in) ::  numsurf
!!        real (kind=kreal), intent(inout) :: x_surf(numsurf,3)
!!      subroutine copy_normal_vectors(org_surf, numsurf,               &
!!     &          area_surf, a_area_surf, vnorm_surf)
!!        type(surface_data), intent(in) :: org_surf
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
      subroutine dup_surface_data(org_surf, new_ele, new_surf)
!
      use set_local_id_table_4_1ele
      use set_size_4_smp_types
!
      type(surface_data), intent(in) :: org_surf
      type(element_data), intent(in) :: new_ele
!
      type(surface_data), intent(inout) :: new_surf
!
!
      new_surf%numsurf = org_surf%numsurf
      new_surf%nnod_4_surf = org_surf%nnod_4_surf
      call allocate_inod_in_surf(new_surf)
      call set_inod_in_surf(new_surf%nnod_4_surf,                       &
     &                      new_surf%node_on_sf, new_surf%node_on_sf_n)
!
      call alloc_surface_connect(new_surf, new_ele%numele)
      call copy_surface_connect(org_surf,                               &
     &    new_surf%numsurf, new_surf%nnod_4_surf, new_surf%ie_surf)
      call copy_surface_to_element (org_surf, new_ele%numele,           &
     &    new_surf%isf_4_ele, new_surf%isf_rot_ele)
!
      call dup_derived_surface_data(org_surf, new_surf)
!
      end subroutine dup_surface_data
!
!-----------------------------------------------------------------------
!
      subroutine dup_derived_surface_data(org_surf, new_surf)
!
      use set_size_4_smp_types
!
      type(surface_data), intent(in) :: org_surf
!
      type(surface_data), intent(inout) :: new_surf
!
!
      call alloc_surf_param_smp(new_surf)
      call count_surf_size_smp(new_surf)
!
      call alloc_global_surf_id(new_surf)
      call alloc_interior_surf(new_surf)
      call copy_global_surface_id(org_surf, new_surf%numsurf,           &
     &    new_surf%isurf_global, new_surf%interior_surf)
!
      new_surf%numsurf_ext = org_surf%numsurf_ext
      call alloc_ext_surface(new_surf)
      call copy_ext_surface                                             &
     &   (org_surf, new_surf%numsurf_ext, new_surf%isf_external)
!
      new_surf%numsurf_iso = org_surf%numsurf_iso
      call alloc_iso_surface(new_surf)
      call copy_isolate_surface                                         &
     &   (org_surf, new_surf%numsurf_iso, new_surf%isf_isolate)
!
      call alloc_element_4_surface(new_surf)
      call copy_element_4_surface(org_surf, new_surf%numsurf,           &
     &                           new_surf%iele_4_surf)
!
      call alloc_surface_geometory(new_surf)
      call copy_surface_geometry(org_surf, new_surf%numsurf,            &
     &                           new_surf%x_surf)
!
      call alloc_normal_vector(new_surf)
      call copy_normal_vectors(org_surf, new_surf%numsurf,              &
     &   new_surf%area_surf, new_surf%a_area_surf, new_surf%vnorm_surf)
!
      end subroutine dup_derived_surface_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_surface_connect                                   &
     &         (org_surf, numsurf, nnod_4_surf, ie_surf)
!
      type(surface_data), intent(in) :: org_surf
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
        ie_surf(1:numsurf,k1) =   org_surf%ie_surf(1:numsurf,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_surface_connect
!
!-----------------------------------------------------------------------
!
      subroutine copy_surface_to_element                                &
     &         (org_surf, numele, isf_4_ele, isf_rot_ele)
!
      type(surface_data), intent(in) :: org_surf
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
        isf_4_ele(1:numele,k1) =   org_surf%isf_4_ele(1:numele,k1)
        isf_rot_ele(1:numele,k1) = org_surf%isf_rot_ele(1:numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_surface_to_element
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_surface_id                                 &
     &         (org_surf, numsurf, isurf_global, interior_surf)
!
      type(surface_data), intent(in) :: org_surf
      integer(kind=kint), intent(in) ::  numsurf
      integer(kind=kint_gl), intent(inout) :: isurf_global(numsurf)
      integer(kind=kint), intent(inout) ::    interior_surf(numsurf)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      isurf_global(1:numsurf) =   org_surf%isurf_global(1:numsurf)
      interior_surf(1:numsurf) = org_surf%interior_surf(1:numsurf)
!$omp end parallel workshare
!
      end subroutine copy_global_surface_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_ext_surface                                       &
     &         (org_surf, numsurf_ext, isf_external)
!
      type(surface_data), intent(in) :: org_surf
      integer(kind=kint), intent(in) ::  numsurf_ext
      integer(kind=kint), intent(inout) :: isf_external(numsurf_ext)
!
!
      if(numsurf_ext .le. 0) return
!$omp parallel workshare
      isf_external(1:numsurf_ext)                                       &
     &        = org_surf%isf_external(1:numsurf_ext)
!$omp end parallel workshare
!
      end subroutine copy_ext_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_isolate_surface                                   &
     &         (org_surf, numsurf_iso, isf_isolate)
!
      type(surface_data), intent(in) :: org_surf
      integer(kind=kint), intent(in) ::  numsurf_iso
      integer(kind=kint), intent(inout) :: isf_isolate(numsurf_iso)
!
!
      if(numsurf_iso .le. 0) return
!$omp parallel workshare
      isf_isolate(1:numsurf_iso) = org_surf%isf_isolate(1:numsurf_iso)
!$omp end parallel workshare
!
      end subroutine copy_isolate_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_element_4_surface                                 &
     &         (org_surf, numsurf, iele_4_surf)
!
      type(surface_data), intent(in) :: org_surf
      integer(kind=kint), intent(in) ::  numsurf
      integer(kind=kint), intent(inout) :: iele_4_surf(numsurf,2,2)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      iele_4_surf(1:numsurf,1,1) = org_surf%iele_4_surf(1:numsurf,1,1)
      iele_4_surf(1:numsurf,2,1) = org_surf%iele_4_surf(1:numsurf,2,1)
      iele_4_surf(1:numsurf,1,2) = org_surf%iele_4_surf(1:numsurf,1,2)
      iele_4_surf(1:numsurf,2,2) = org_surf%iele_4_surf(1:numsurf,1,2)
!$omp end parallel workshare
!
      end subroutine copy_element_4_surface
!
!-----------------------------------------------------------------------
!
      subroutine copy_surface_geometry(org_surf, numsurf, x_surf)
!
      type(surface_data), intent(in) :: org_surf
      integer(kind=kint), intent(in) ::  numsurf
      real (kind=kreal), intent(inout) :: x_surf(numsurf,3)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      x_surf(1:numsurf,1) = org_surf%x_surf(1:numsurf,1)
      x_surf(1:numsurf,2) = org_surf%x_surf(1:numsurf,2)
      x_surf(1:numsurf,3) = org_surf%x_surf(1:numsurf,3)
!$omp end parallel workshare
!
      end subroutine copy_surface_geometry
!
!-----------------------------------------------------------------------
!
      subroutine copy_normal_vectors(org_surf, numsurf,                 &
     &          area_surf, a_area_surf, vnorm_surf)
!
      type(surface_data), intent(in) :: org_surf
      integer(kind=kint), intent(in) ::  numsurf
      real (kind=kreal), intent(inout) :: area_surf(numsurf)
      real (kind=kreal), intent(inout) :: a_area_surf(numsurf)
      real (kind=kreal), intent(inout) :: vnorm_surf(numsurf,3)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      area_surf(1:numsurf) =    org_surf%area_surf(1:numsurf)
      a_area_surf(1:numsurf) =  org_surf%a_area_surf(1:numsurf)
      vnorm_surf(1:numsurf,1) = org_surf%vnorm_surf(1:numsurf,1)
      vnorm_surf(1:numsurf,2) = org_surf%vnorm_surf(1:numsurf,2)
      vnorm_surf(1:numsurf,3) = org_surf%vnorm_surf(1:numsurf,3)
!$omp end parallel workshare
!
      end subroutine copy_normal_vectors
!
!-----------------------------------------------------------------------
!
      end module copy_surface_data
