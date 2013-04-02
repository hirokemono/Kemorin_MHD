!
!      module find_local_elements
!
      module find_local_elements
!
!     Written by H. Matsui on Sep., 2007
!
      use m_precision
      use m_constants
!
      implicit  none
!
      integer(kind = kint), allocatable, private :: imark_ele(:)
!
!      subroutine CRE_LOCAL_DATA(NP)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine CRE_LOCAL_DATA(NP)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_near_element_id_4_node
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint) :: ip, icel, icou, in, k
!
!
      call allocate_num_4_near_ele(NP)
      allocate (imark_ele(numele))

      iele_stack_near_nod(0)= 0
      do ip= 1, NP
!
        imark_ele(1:numele)= 0
        do icel= 1, numele
          do k= 1, nodelm(icel)
            in= ie(icel,k)
            if (IGROUP_nod(in).eq.ip) imark_ele(icel) = 1
          enddo
        enddo

        do icel= 1, numele
          nele_near_nod(ip)  = nele_near_nod(ip) + imark_ele(icel)
        enddo
        iele_stack_near_nod(ip) = iele_stack_near_nod(ip-1)             &
     &                           + nele_near_nod(ip)
      end do
      ntot_ele_near_nod = iele_stack_near_nod(NP)
!
!
     call allocate_near_element
!
      do ip= 1, NP
!
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
        enddo
      enddo

      deallocate (imark_ele)

      end subroutine CRE_LOCAL_DATA
!
!   --------------------------------------------------------------------
!
      end module find_local_elements
