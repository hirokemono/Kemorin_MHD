!>@file   copy_element_data.f90
!!@brief  module copy_element_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief Routines to copy element data
!!
!!@verbatim
!!        subroutine dup_element_data(nprocs, org_ele, new_ele)
!!        subroutine dup_derived_element_data(nprocs, org_ele, new_ele)
!!        integer, intent(in) :: nprocs
!!        type(element_data), intent(in) :: org_ele
!!        type(element_data), intent(inout) :: new_ele
!!
!!      subroutine copy_ele_connect(org_ele, numele, nnod_4_ele, ie)
!!        type(element_data), intent(in) :: org_ele
!!        integer(kind=kint), intent(in) :: numele, nnod_4_ele
!!        integer(kind=kint), intent(inout) :: ie(numele,nnod_4_ele)
!!      subroutine copy_global_element_id(org_ele, numele,              &
!!     &          first_ele_type, iele_global, elmtyp, nodelm)
!!        type(element_data), intent(in) :: org_ele
!!        integer(kind=kint), intent(in) ::  numele
!!        integer(kind=kint_gl), intent(inout) :: iele_global(numele)
!!        integer(kind=kint), intent(inout) :: elmtyp(numele)
!!        integer(kind=kint), intent(inout) :: nodelm(numele)
!!        integer(kind=kint), intent(inout) :: first_ele_type
!!      subroutine copy_overlapped_ele                                  &
!!     &         (org_ele, numele, internal_ele, interior_ele)
!!        type(element_data), intent(in) :: org_ele
!!        integer(kind=kint), intent(in) :: numele
!!        integer(kind=kint), intent(inout) :: interior_ele(numele)
!!        integer(kind=kint), intent(inout) :: internal_ele
!!      subroutine copy_element_geometry(org_ele, numele, x_ele,        &
!!     &          r_ele, ar_ele, theta_ele, phi_ele, s_ele, as_ele,     &
!!     &          volume_ele, a_vol_ele, volume, a_vol)
!!        type(element_data), intent(in) :: org_ele
!!        integer(kind=kint), intent(in) :: numele
!!        real (kind=kreal), intent(inout) :: x_ele(numele,3)
!!        real (kind=kreal), intent(inout) :: r_ele(numele)
!!        real (kind=kreal), intent(inout) :: ar_ele(numele)
!!        real (kind=kreal), intent(inout) :: theta_ele(numele)
!!        real (kind=kreal), intent(inout) :: phi_ele(numele)
!!        real (kind=kreal), intent(inout) :: s_ele(numele)
!!        real (kind=kreal), intent(inout) :: as_ele(numele)
!!        real (kind=kreal), intent(inout) :: volume_ele(numele)
!!        real (kind=kreal), intent(inout) :: a_vol_ele(numele)
!!        real (kind=kreal), intent(inout) :: volume, a_vol
!!      subroutine copy_global_numele_list                              &
!!     &         (org_ele, nprocs, istack_numele, istack_interele)
!!        type(element_data), intent(in) :: org_ele
!!        integer, intent(in) :: nprocs
!!      integer(kind=kint_gl), intent(inout)                            &
!!     &                      :: istack_numele(0:nprocs)
!!      integer(kind=kint_gl), intent(inout)                            &
!!     &                      :: istack_interele(0:nprocs)
!!@endverbatim
!
      module copy_element_data
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dup_element_data(nprocs, org_ele, new_ele)
!
      integer, intent(in) :: nprocs
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
!
      new_ele%numele =     org_ele%numele
      new_ele%nnod_4_ele = org_ele%nnod_4_ele
      call alloc_element_types(new_ele)
      call copy_global_element_id                                       &
     &   (org_ele, new_ele%numele, new_ele%first_ele_type,              &
     &    new_ele%iele_global, new_ele%elmtyp, new_ele%nodelm)
!
      call alloc_ele_connectivity(new_ele)
      call copy_ele_connect                                             &
     &   (org_ele, new_ele%numele, new_ele%nnod_4_ele, new_ele%ie)
!
      call dup_derived_element_data(nprocs, org_ele, new_ele)
!
      end subroutine dup_element_data
!
!-----------------------------------------------------------------------
!
      subroutine dup_derived_element_data(nprocs, org_ele, new_ele)
!
      use set_size_4_smp_types
!
      integer, intent(in) :: nprocs
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
!
      call alloc_ele_param_smp(new_ele)
      call count_ele_4_smp_mesh(new_ele)
!
      call alloc_overlapped_ele(new_ele)
      call copy_overlapped_ele(org_ele, new_ele%numele,                 &
     &    new_ele%internal_ele, new_ele%interior_ele)
!
      call alloc_ele_geometry(new_ele)
      call copy_element_geometry                                        &
     &   (org_ele, new_ele%numele, new_ele%x_ele,                       &
     &    new_ele%r_ele, new_ele%ar_ele, new_ele%theta_ele,             &
     &    new_ele%phi_ele, new_ele%s_ele, new_ele%as_ele,               &
     &    new_ele%volume_ele, new_ele%a_vol_ele,                        &
     &    new_ele%volume, new_ele%a_vol)
!
      call alloc_numele_stack(nprocs, new_ele)
      call copy_global_numele_list(org_ele, nprocs,                     &
     &    new_ele%istack_numele, new_ele%istack_interele)
!
      end subroutine dup_derived_element_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_ele_connect(org_ele, numele, nnod_4_ele, ie)
!
      type(element_data), intent(in) :: org_ele
      integer(kind=kint), intent(in) :: numele, nnod_4_ele
!
      integer(kind=kint), intent(inout) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint) :: k1
!
      if(numele .le. 0) return
!$omp parallel private(k1)
      do k1 = 1, nnod_4_ele
!$omp workshare
        ie(1:numele,k1) =   org_ele%ie(1:numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_ele_connect
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_element_id(org_ele, numele,                &
     &          first_ele_type, iele_global, elmtyp, nodelm)
!
      type(element_data), intent(in) :: org_ele
      integer(kind=kint), intent(in) ::  numele
      integer(kind=kint_gl), intent(inout) :: iele_global(numele)
      integer(kind=kint), intent(inout) :: elmtyp(numele)
      integer(kind=kint), intent(inout) :: nodelm(numele)
      integer(kind=kint), intent(inout) :: first_ele_type
!
      first_ele_type = org_ele%first_ele_type
!
      if(numele .le. 0) return
!$omp parallel workshare
      iele_global(1:numele) = org_ele%iele_global(1:numele)
      elmtyp(1:numele) =      org_ele%elmtyp(1:numele)
      nodelm(1:numele) =      org_ele%nodelm(1:numele)
!$omp end parallel workshare
!
      end subroutine copy_global_element_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_overlapped_ele                                    &
     &         (org_ele, numele, internal_ele, interior_ele)
!
      type(element_data), intent(in) :: org_ele
      integer(kind=kint), intent(in) :: numele
      integer(kind=kint), intent(inout) :: interior_ele(numele)
      integer(kind=kint), intent(inout) :: internal_ele
!
!
      internal_ele = org_ele%internal_ele
!
      if(numele .le. 0) return
!$omp parallel workshare
      interior_ele(1:numele) = org_ele%interior_ele(1:numele)
!$omp end parallel workshare
!
      end subroutine copy_overlapped_ele
!
!-----------------------------------------------------------------------
!
      subroutine copy_element_geometry(org_ele, numele, x_ele,          &
     &          r_ele, ar_ele, theta_ele, phi_ele, s_ele, as_ele,       &
     &          volume_ele, a_vol_ele, volume, a_vol)
!
      type(element_data), intent(in) :: org_ele
      integer(kind=kint), intent(in) :: numele
!
      real (kind=kreal), intent(inout) :: x_ele(numele,3)
!
      real (kind=kreal), intent(inout) :: r_ele(numele)
      real (kind=kreal), intent(inout) :: ar_ele(numele)
      real (kind=kreal), intent(inout) :: theta_ele(numele)
      real (kind=kreal), intent(inout) :: phi_ele(numele)
      real (kind=kreal), intent(inout) :: s_ele(numele)
      real (kind=kreal), intent(inout) :: as_ele(numele)
!
      real (kind=kreal), intent(inout) :: volume_ele(numele)
      real (kind=kreal), intent(inout) :: a_vol_ele(numele)
      real (kind=kreal), intent(inout) :: volume, a_vol
!
!
      volume = org_ele%volume
      a_vol =  org_ele%a_vol
!
      if(numele .le. 0) return
!$omp parallel workshare
      x_ele(1:numele,1) = org_ele%x_ele(1:numele,1)
      x_ele(1:numele,2) = org_ele%x_ele(1:numele,2)
      x_ele(1:numele,3) = org_ele%x_ele(1:numele,3)
!
      r_ele(1:numele) = org_ele%r_ele(1:numele)
      ar_ele(1:numele) = org_ele%ar_ele(1:numele)
      phi_ele(1:numele) = org_ele%phi_ele(1:numele)
      theta_ele(1:numele) = org_ele%theta_ele(1:numele)
      s_ele(1:numele) = org_ele%s_ele(1:numele)
      as_ele(1:numele) = org_ele%as_ele(1:numele)
!
      volume_ele(1:numele) = org_ele%volume_ele(1:numele)
      a_vol_ele(1:numele) =  org_ele%a_vol_ele(1:numele)
!$omp end parallel workshare
!
      end subroutine copy_element_geometry
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_numele_list                                &
     &         (org_ele, nprocs, istack_numele, istack_interele)
!
      type(element_data), intent(in) :: org_ele
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(inout)                              &
     &                      :: istack_numele(0:nprocs)
      integer(kind=kint_gl), intent(inout)                              &
     &                      :: istack_interele(0:nprocs)
!
!
!$omp parallel workshare
      istack_numele(0:nprocs) = org_ele%istack_numele(0:nprocs)
      istack_interele(0:nprocs) = org_ele%istack_interele(0:nprocs)
!$omp end parallel workshare
!
      end subroutine copy_global_numele_list
!
!-----------------------------------------------------------------------
!
      end module copy_element_data
