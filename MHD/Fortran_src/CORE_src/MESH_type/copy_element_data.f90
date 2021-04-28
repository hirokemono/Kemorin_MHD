!>@file   copy_element_data.f90
!!@brief  module copy_element_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief Routines to copy element data
!!
!!@verbatim
!!        subroutine dup_element_data(nprocs, ele_org, ele_new)
!!        integer, intent(in) :: nprocs
!!        type(element_data), intent(in) :: ele_org
!!        type(element_data), intent(inout) :: ele_new
!!
!!      subroutine copy_ele_connect(ele_org, numele, nnod_4_ele, ie)
!!        type(element_data), intent(in) :: ele_org
!!        integer(kind=kint), intent(in) :: numele, nnod_4_ele
!!        integer(kind=kint), intent(inout) :: ie(numele,nnod_4_ele)
!!      subroutine copy_global_element_id(ele_org, numele,              &
!!     &          first_ele_type, iele_global, elmtyp, nodelm)
!!        type(element_data), intent(in) :: ele_org
!!        integer(kind=kint), intent(in) ::  numele
!!        integer(kind=kint_gl), intent(inout) :: iele_global(numele)
!!        integer(kind=kint), intent(inout) :: elmtyp(numele)
!!        integer(kind=kint), intent(inout) :: nodelm(numele)
!!        integer(kind=kint), intent(inout) :: first_ele_type
!!      subroutine copy_overlapped_ele                                  &
!!     &         (ele_org, numele, internal_ele, interior_ele)
!!        type(element_data), intent(in) :: ele_org
!!        integer(kind=kint), intent(in) :: numele
!!        integer(kind=kint), intent(inout) :: interior_ele(numele)
!!        integer(kind=kint), intent(inout) :: internal_ele
!!      subroutine copy_element_geometry(ele_org, numele, x_ele,        &
!!     &          r_ele, ar_ele, theta_ele, phi_ele, s_ele, as_ele,     &
!!     &          volume_ele, a_vol_ele, volume, a_vol)
!!        type(element_data), intent(in) :: ele_org
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
!!     &         (ele_org, nprocs, istack_numele, istack_interele)
!!        type(element_data), intent(in) :: ele_org
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
      subroutine dup_element_data(nprocs, ele_org, ele_new)
!
      use set_local_id_table_4_1ele
      use set_size_4_smp_types
!
      integer, intent(in) :: nprocs
      type(element_data), intent(in) :: ele_org
      type(element_data), intent(inout) :: ele_new
!
!
      ele_new%numele =     ele_org%numele
      ele_new%nnod_4_ele = ele_org%nnod_4_ele
      call alloc_ele_param_smp(ele_new)
      call count_ele_4_smp_mesh(ele_new)
!
      call alloc_element_types(ele_new)
      call copy_global_element_id                                       &
     &   (ele_org, ele_new%numele, ele_new%first_ele_type,              &
     &    ele_new%iele_global, ele_new%elmtyp, ele_new%nodelm)
!
      call alloc_ele_connectivity(ele_new)
      call copy_ele_connect                                             &
     &   (ele_org, ele_new%numele, ele_new%nnod_4_ele, ele_new%ie)
!
      call alloc_overlapped_ele(ele_new)
      call copy_overlapped_ele(ele_org, ele_new%numele,                 &
     &    ele_new%internal_ele, ele_new%interior_ele)
!
      call alloc_ele_geometry(ele_new)
      call copy_element_geometry                                        &
     &   (ele_org, ele_new%numele, ele_new%x_ele,                       &
     &    ele_new%r_ele, ele_new%ar_ele, ele_new%theta_ele,             &
     &    ele_new%phi_ele, ele_new%s_ele, ele_new%as_ele,               &
     &    ele_new%volume_ele, ele_new%a_vol_ele,                        &
     &    ele_new%volume, ele_new%a_vol)
!
      call alloc_numele_stack(nprocs, ele_new)
      call copy_global_numele_list(ele_org, nprocs,                     &
     &    ele_new%istack_numele, ele_new%istack_interele)
!
      end subroutine dup_element_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_ele_connect(ele_org, numele, nnod_4_ele, ie)
!
      type(element_data), intent(in) :: ele_org
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
        ie(1:numele,k1) =   ele_org%ie(1:numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine copy_ele_connect
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_element_id(ele_org, numele,                &
     &          first_ele_type, iele_global, elmtyp, nodelm)
!
      type(element_data), intent(in) :: ele_org
      integer(kind=kint), intent(in) ::  numele
      integer(kind=kint_gl), intent(inout) :: iele_global(numele)
      integer(kind=kint), intent(inout) :: elmtyp(numele)
      integer(kind=kint), intent(inout) :: nodelm(numele)
      integer(kind=kint), intent(inout) :: first_ele_type
!
      first_ele_type = ele_org%first_ele_type
!
      if(numele .le. 0) return
!$omp parallel workshare
      iele_global(1:numele) = ele_org%iele_global(1:numele)
      elmtyp(1:numele) =      ele_org%elmtyp(1:numele)
      nodelm(1:numele) =      ele_org%nodelm(1:numele)
!$omp end parallel workshare
!
      end subroutine copy_global_element_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_overlapped_ele                                    &
     &         (ele_org, numele, internal_ele, interior_ele)
!
      type(element_data), intent(in) :: ele_org
      integer(kind=kint), intent(in) :: numele
      integer(kind=kint), intent(inout) :: interior_ele(numele)
      integer(kind=kint), intent(inout) :: internal_ele
!
!
      internal_ele = ele_org%internal_ele
!
      if(numele .le. 0) return
!$omp parallel workshare
      interior_ele(1:numele) = ele_org%interior_ele(1:numele)
!$omp end parallel workshare
!
      end subroutine copy_overlapped_ele
!
!-----------------------------------------------------------------------
!
      subroutine copy_element_geometry(ele_org, numele, x_ele,          &
     &          r_ele, ar_ele, theta_ele, phi_ele, s_ele, as_ele,       &
     &          volume_ele, a_vol_ele, volume, a_vol)
!
      type(element_data), intent(in) :: ele_org
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
      volume = ele_org%volume
      a_vol =  ele_org%a_vol
!
      if(numele .le. 0) return
!$omp parallel workshare
      x_ele(1:numele,1) = ele_org%x_ele(1:numele,1)
      x_ele(1:numele,2) = ele_org%x_ele(1:numele,2)
      x_ele(1:numele,3) = ele_org%x_ele(1:numele,3)
!
      r_ele(1:numele) = ele_org%r_ele(1:numele)
      ar_ele(1:numele) = ele_org%ar_ele(1:numele)
      phi_ele(1:numele) = ele_org%phi_ele(1:numele)
      theta_ele(1:numele) = ele_org%theta_ele(1:numele)
      s_ele(1:numele) = ele_org%s_ele(1:numele)
      as_ele(1:numele) = ele_org%as_ele(1:numele)
!
      volume_ele(1:numele) = ele_org%volume_ele(1:numele)
      a_vol_ele(1:numele) =  ele_org%a_vol_ele(1:numele)
!$omp end parallel workshare
!
      end subroutine copy_element_geometry
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_numele_list                                &
     &         (ele_org, nprocs, istack_numele, istack_interele)
!
      type(element_data), intent(in) :: ele_org
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(inout)                              &
     &                      :: istack_numele(0:nprocs)
      integer(kind=kint_gl), intent(inout)                              &
     &                      :: istack_interele(0:nprocs)
!
!
!$omp parallel workshare
      istack_numele(0:nprocs) = ele_org%istack_numele(0:nprocs)
      istack_interele(0:nprocs) = ele_org%istack_interele(0:nprocs)
!$omp end parallel workshare
!
      end subroutine copy_global_numele_list
!
!-----------------------------------------------------------------------
!
      end module copy_element_data
