!
!      module find_local_elements
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine CRE_LOCAL_DATA(Ndomain, included_ele)
!
      module find_local_elements
!
      use m_precision
      use m_constants
!
      implicit  none
!
      integer(kind = kint), allocatable, private :: imark_ele(:)
!
      private :: count_ele_in_subdomain, set_ele_in_subdomain
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine CRE_LOCAL_DATA(Ndomain, numnod, ele, included_ele)
!
      use t_geometry_data
      use t_near_mesh_id_4_node
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: Ndomain, numnod
      type(element_data), intent(in) :: ele
      type(near_mesh), intent(inout) :: included_ele
!
!
      call alloc_num_4_near_nod(Ndomain, included_ele)
      allocate (imark_ele(ele%numele))
!
      write(*,*) 'count_ele_in_subdomain', 'Ndomain', Ndomain
      write(*,*) 'nnod_s_domain', nod_d_grp1%num_s_domin, 'numnod', numnod
! find each subdomain's neighbor elements number stored in included_ele%num_nod(1:Ndomain)
      call count_ele_in_subdomain                                       &
     &   (Ndomain, nod_d_grp1%num_s_domin, IGROUP_nod, numnod,          &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%nodelm,               &
     &    included_ele%ntot, included_ele%num_nod,                      &
     &    included_ele%istack_nod)
write(*,*) 'included_ele%num_nod', included_ele%num_nod(1:10)
write(*,*) 'included_ele%istack_nod', included_ele%istack_nod(1:10)

!
      call alloc_near_element(included_ele)
!
! store each subdomain's neighbor elements id in included_ele%id_near_nod
      call set_ele_in_subdomain                                         &
     &   (Ndomain, nod_d_grp1%num_s_domin, IGROUP_nod, numnod,          &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%nodelm,               &
     &    included_ele%ntot, included_ele%istack_nod,                   &
     &    included_ele%id_near_nod)

      deallocate (imark_ele)

      end subroutine CRE_LOCAL_DATA
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_ele_in_subdomain                                 &
     &         (Ndomain, nnod_s_domin, IGROUP_nod,                      &
     &          numnod, numele, nnod_4_ele, ie, nodelm,                 &
     &          ntot_ele_near_nod, nele_near_nod, iele_stack_near_nod)
!
      integer(kind = kint), intent(in) :: Ndomain
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: nodelm(numele)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: nnod_s_domin
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_s_domin)
!
      integer(kind = kint), intent(inout) :: ntot_ele_near_nod
      integer(kind = kint), intent(inout) :: nele_near_nod(numnod)
      integer(kind = kint), intent(inout)                               &
     &              :: iele_stack_near_nod(0:numnod)
!
      integer(kind = kint) :: ip, icel, in, k
!
!
      iele_stack_near_nod(0)= 0
      do ip= 1, Ndomain
!
        imark_ele(1:numele)= 0
        do icel= 1, numele
          do k= 1, nodelm(icel)
            in= ie(icel,k)
            if (IGROUP_nod(in).eq.ip) imark_ele(icel) = 1
          end do
        end do

        do icel= 1, numele
          nele_near_nod(ip)  = nele_near_nod(ip) + imark_ele(icel)
        enddo
        iele_stack_near_nod(ip) = iele_stack_near_nod(ip-1)             &
     &                           + nele_near_nod(ip)
      end do
      ntot_ele_near_nod = iele_stack_near_nod(Ndomain)
!
      end subroutine count_ele_in_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine set_ele_in_subdomain                                   &
     &         (Ndomain, nnod_s_domin, IGROUP_nod,                      &
     &          numnod, numele, nnod_4_ele, ie, nodelm,                 &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod)
!
      integer(kind = kint), intent(in) :: Ndomain
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: nodelm(numele)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: nnod_s_domin
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_s_domin)
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in)                                  &
     &              :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(inout)                               &
     &              :: iele_near_nod(ntot_ele_near_nod)
!
      integer(kind = kint) :: ip, icel, in, k, icou
!
!
      do ip= 1, Ndomain
        imark_ele(1:numele)= 0
        do icel= 1, numele
          do k= 1, nodelm(icel)
            in= ie(icel,k)
            if (IGROUP_nod(in).eq.ip) imark_ele(icel) = 1
          enddo
        enddo

        icou= iele_stack_near_nod(ip-1)
        do icel= 1, numele
          if (imark_ele(icel).eq.1) then
            icou = icou + 1
              iele_near_nod(icou) =       icel
          endif
        end do
      end do
!
      end subroutine set_ele_in_subdomain
!
!   --------------------------------------------------------------------
!
      end module find_local_elements
