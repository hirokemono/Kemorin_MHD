!>@file   t_element_hash.f90
!!@brief  module t_element_hash
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in March, 2006
!
!>@brief  routines to make hash table for elements
!!
!!@verbatim
!!      subroutine alloc_ele_hash_by_nod(numnod, numele)
!!      subroutine alloc_ele_hash_by_shell(numele)
!!        type(element_hash), intent(inout) :: e_hash
!!
!!      subroutine dealloc_elment_hash
!!      subroutine dealloc_position_4_hash
!!        type(element_hash), intent(inout) :: e_hash
!!
!!      subroutine count_ele_hash_by_nod                                &
!!     &         (numnod, numele, nnod_4_ele, ie, e_hash)
!!      subroutine set_element_hash_by_nod                              &
!!     &         (numele, nnod_4_ele, ie, e_hash)
!!        type(element_hash), intent(inout) :: e_hash
!!
!!      subroutine set_border_4_hash_sph(e_hash)
!!      subroutine count_element_hash_by_sph(numnod, numele, nnod_4_ele,&
!!     &          radius, theta, phi, ie, e_hash)
!!      subroutine set_element_hash_by_sph(numnod, numele, nnod_4_ele,  &
!!     &          radius, theta, phi, ie, e_hash)
!!        type(element_hash), intent(inout) :: e_hash
!!@endverbatim
!
      module t_element_hash
!
      use m_precision
      use m_constants
!
      implicit none
!
      type element_hash
        integer(kind = kint) :: ncomp_ele_hash
        integer(kind = kint) :: iend_ele_hash
        integer(kind = kint), allocatable :: inum_ele_hash(:)
        integer(kind = kint), allocatable :: istack_ele_hash(:)
        integer(kind = kint), allocatable :: iele_hash(:)
        integer(kind = kint), allocatable :: iele_flag(:)
!
        integer(kind = kint) :: nr_ele_hash
        integer(kind = kint) :: nth_ele_hash
        integer(kind = kint) :: nphi_ele_hash
        real(kind = kreal), allocatable :: theta_4_hash(:)
        real(kind = kreal), allocatable :: phi_4_hash(:)
        real(kind = kreal), allocatable :: r_4_hash(:)
      end type element_hash
!
      private :: set_hash_id_by_sph
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_ele_hash_by_nod(numnod, numele, e_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(element_hash), intent(inout) :: e_hash
!
!
      e_hash%ncomp_ele_hash = numnod
!
      allocate(e_hash%inum_ele_hash(e_hash%ncomp_ele_hash))
      allocate(e_hash%istack_ele_hash(0:e_hash%ncomp_ele_hash))
      allocate(e_hash%iele_hash(numele) )
      allocate(e_hash%iele_flag(numele) )
      e_hash%inum_ele_hash = 0
      e_hash%istack_ele_hash = 0
      e_hash%iele_hash = 0
      e_hash%iele_flag = 0
!
      end subroutine alloc_ele_hash_by_nod
!
!------------------------------------------------------------------
!
      subroutine alloc_ele_hash_by_shell(numele, e_hash)
!
      integer(kind = kint), intent(in) :: numele
      type(element_hash), intent(inout) :: e_hash
!
!
      e_hash%nphi_ele_hash = 2 * e_hash%nth_ele_hash
      e_hash%ncomp_ele_hash = e_hash%nr_ele_hash                        &
     &                       * e_hash%nth_ele_hash                      &
     &                       * e_hash%nphi_ele_hash
!
      allocate(e_hash%inum_ele_hash(e_hash%ncomp_ele_hash))
      allocate(e_hash%istack_ele_hash(0:e_hash%ncomp_ele_hash))
      allocate(e_hash%iele_hash(numele) )
      allocate(e_hash%iele_flag(numele) )
!
      allocate(e_hash%r_4_hash(e_hash%nr_ele_hash))
      allocate(e_hash%theta_4_hash(e_hash%nth_ele_hash+1))
      allocate(e_hash%phi_4_hash(e_hash%nphi_ele_hash))
!
      e_hash%istack_ele_hash = 0
      if(e_hash%ncomp_ele_hash .gt. 0) e_hash%inum_ele_hash = 0
      if(numele .gt. 0) e_hash%iele_hash = 0
      if(numele .gt. 0) e_hash%iele_flag = 0
!
      e_hash%theta_4_hash = 0.0d0
      if(e_hash%nphi_ele_hash .gt. 0) e_hash%phi_4_hash = 0.0d0
      if(e_hash%nr_ele_hash .gt. 0) e_hash%r_4_hash = 0.0d0
!
      end subroutine alloc_ele_hash_by_shell
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_elment_hash(e_hash)
!
      type(element_hash), intent(inout) :: e_hash
!
      deallocate(e_hash%iele_hash,     e_hash%iele_flag)
      deallocate(e_hash%inum_ele_hash, e_hash%istack_ele_hash)
!
      end subroutine dealloc_elment_hash
!
!------------------------------------------------------------------
!
      subroutine dealloc_position_4_hash(e_hash)
!
      type(element_hash), intent(inout) :: e_hash
!
      deallocate(e_hash%r_4_hash, e_hash%theta_4_hash)
      deallocate(e_hash%phi_4_hash)
!
      end subroutine dealloc_position_4_hash
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_ele_hash_by_nod                                  &
     &         (numnod, numele, nnod_4_ele, ie, e_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      type(element_hash), intent(inout) :: e_hash
!
      integer(kind = kint) :: iele, ihash
!
!
      e_hash%inum_ele_hash = 0
      do iele = 1, numele
        ihash = ie(iele,1)
        e_hash%inum_ele_hash(ihash) = e_hash%inum_ele_hash(ihash) + 1
      end do
!
      e_hash%istack_ele_hash = 0
      do ihash = 1, numnod
        e_hash%istack_ele_hash(ihash) = e_hash%istack_ele_hash(ihash-1) &
     &                                 + e_hash%inum_ele_hash(ihash)
        if(e_hash%istack_ele_hash(ihash) .le. numele ) then
          e_hash%iend_ele_hash = ihash
        end if
      end do
!
!
      end subroutine count_ele_hash_by_nod
!
!------------------------------------------------------------------
!
      subroutine set_element_hash_by_nod                                &
     &         (numele, nnod_4_ele, ie, e_hash)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      type(element_hash), intent(inout) :: e_hash
!
      integer(kind = kint) :: j, iele, ihash
!
!
      e_hash%inum_ele_hash = 0
      e_hash%iele_hash = 0
      do iele = 1, numele
        ihash = ie(iele,1)
        e_hash%inum_ele_hash(ihash) = e_hash%inum_ele_hash(ihash) + 1
        j = e_hash%iele_hash(ihash-1) + e_hash%inum_ele_hash(ihash)
        e_hash%iele_hash(j) = iele
      end do
!
      end subroutine set_element_hash_by_nod
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_border_4_hash_sph(e_hash)
!
      type(element_hash), intent(inout) :: e_hash
!
      integer(kind = kint) :: k
      real(kind = kreal) :: pi
!
!
      pi = four*atan(one)
      do k = 1, e_hash%nth_ele_hash+1
        e_hash%theta_4_hash(k) = pi * dble(k-1)                         &
     &                          / dble(e_hash%nth_ele_hash)
      end do
!
      do k = 1, e_hash%nphi_ele_hash
        e_hash%phi_4_hash(k) = two * pi * dble(k-1)                     &
     &                        / dble(e_hash%nphi_ele_hash)
      end do
!
      end subroutine set_border_4_hash_sph
!
!------------------------------------------------------------------
!
      subroutine count_element_hash_by_sph(numnod, numele, nnod_4_ele,  &
     &          radius, theta, phi, ie, e_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: theta(numnod)
      real(kind = kreal), intent(in) :: phi(numnod)
!
      type(element_hash), intent(inout) :: e_hash
!
      integer(kind = kint) :: inod, iele, ihash
!
!
      e_hash%inum_ele_hash = 0
      do iele = 1, numele
        inod = ie(iele,1)
        ihash = set_hash_id_by_sph(radius(inod), theta(inod),           &
     &         phi(inod), e_hash)
        e_hash%inum_ele_hash(ihash) = e_hash%inum_ele_hash(ihash) + 1
      end do
!
      e_hash%istack_ele_hash = 0
      do ihash = 1, numnod
        e_hash%istack_ele_hash(ihash) = e_hash%istack_ele_hash(ihash-1) &
     &                                 + e_hash%inum_ele_hash(ihash)
        if(e_hash%istack_ele_hash(ihash) .le. numele) then
          e_hash%iend_ele_hash = ihash
        end if
      end do
!
      end subroutine count_element_hash_by_sph
!
!------------------------------------------------------------------
!
      subroutine set_element_hash_by_sph(numnod, numele, nnod_4_ele,    &
     &          radius, theta, phi, ie, e_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: theta(numnod)
      real(kind = kreal), intent(in) :: phi(numnod)
!
      type(element_hash), intent(inout) :: e_hash
!
      integer(kind = kint) :: inod, iele, ihash, j
!
!
      e_hash%inum_ele_hash = 0
      e_hash%iele_hash = 0
      do iele = 1, numele
        inod = ie(iele,1)
        ihash = set_hash_id_by_sph(radius(inod), theta(inod),           &
     &                             phi(inod), e_hash)
!
        e_hash%inum_ele_hash(ihash) = e_hash%inum_ele_hash(ihash) + 1
        j = e_hash%iele_hash(ihash-1) + e_hash%inum_ele_hash(ihash)
        e_hash%iele_hash(j) = iele
      end do
!
      end subroutine set_element_hash_by_sph
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &        set_hash_id_by_sph(r_ele, theta_ele, phi_ele, e_hash)
!
      real(kind = kreal), intent(in) :: r_ele, theta_ele, phi_ele
      type(element_hash), intent(in) :: e_hash
!
      integer(kind = kint) :: i0, i_r, j_theta, k_phi
      real(kind = kreal) :: pi
!
!
      pi = four*atan(one)
      k_phi = int(aint(phi_ele * dble(e_hash%nphi_ele_hash)             &
     &       / (two * pi) ), KIND(k_phi))
      j_theta = int( aint( theta_ele * dble(e_hash%nth_ele_hash) / pi), &
     &         KIND(j_theta))
!
      i_r = 0
      do i0 = 1, e_hash%nr_ele_hash-1
        if(r_ele .le. e_hash%r_4_hash(i0+1)                             &
     &       .and. r_ele .gt. e_hash%r_4_hash(i0) ) then
          i_r = i0
          exit
        end if
      end do
      if (i_r .eq. 0) i_r = e_hash%nr_ele_hash
!
      set_hash_id_by_sph = k_phi+ e_hash%nphi_ele_hash * (j_theta-1)    &
     &   + e_hash%nphi_ele_hash * (e_hash%nth_ele_hash+1) * (i_r-1)
!
      end function set_hash_id_by_sph
!
!------------------------------------------------------------------
!
      end module t_element_hash
