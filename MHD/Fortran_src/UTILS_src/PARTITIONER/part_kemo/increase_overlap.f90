!
!     module increase_overlap
!
!     written by H. Matsui on Sep., 2007
!
!      subroutine increase_overlapping(NP, n_overlap, i_sleeve_ele,     &
!     &          included_ele)
!
      module increase_overlap
!
      use m_precision
      use m_constants
!
      use t_near_mesh_id_4_node
!
      implicit  none
!
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele_tmp
!
      integer(kind= kint) :: nele_subdomain
      integer(kind= kint), allocatable :: iflag_nod(:), iflag_ele(:)
      integer(kind= kint), allocatable :: item_tmp_e(:)
      integer(kind= kint), allocatable :: NPC_tmp2(:)
!
      private :: nele_subdomain, iflag_nod, iflag_ele
      private :: item_tmp_e, NPC_tmp2
!
      private :: mark_extented_overlap
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine increase_overlapping(NP, n_overlap, i_sleeve_ele,      &
     &          included_ele)
!
      use m_geometry_data
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: n_overlap, i_sleeve_ele
      type(near_mesh), intent(inout) :: included_ele
!
      integer(kind= kint) :: ip, inum, icel
!
!
      included_ele%ntot = included_ele%istack_nod(NP)
      near_ele_tmp%ntot = included_ele%ntot
      call alloc_num_4_near_nod(NP, near_ele_tmp)
      call alloc_near_element(near_ele_tmp)
!
      allocate( iflag_nod(node1%numnod) )
      allocate( iflag_ele(ele1%numele) )
      allocate( item_tmp_e(ele1%numele) )
!
      iflag_nod = 0
      iflag_ele = 0
      item_tmp_e = 0
      do ip= 1, NP
        call mark_extented_overlap                                      &
     &     (ip, n_overlap, i_sleeve_ele, node1%numnod,                  &
     &      ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%nodelm,         &
     &      included_ele%ntot, included_ele%istack_nod,                 &
     &      included_ele%id_near_nod, nnod_s_domin, IGROUP_nod)
!
        allocate(NPC_tmp2(near_ele_tmp%istack_nod(ip-1)) )
!
        near_ele_tmp%istack_nod(ip) = near_ele_tmp%istack_nod(ip-1)     &
     &                                 + nele_subdomain
        do icel = 1, near_ele_tmp%istack_nod(ip-1)
          NPC_tmp2(icel) = near_ele_tmp%id_near_nod(icel)
        end do
!
        near_ele_tmp%ntot = near_ele_tmp%istack_nod(ip)
        call dealloc_near_node(near_ele_tmp)
        call alloc_near_element(near_ele_tmp)
!
        do icel = 1, near_ele_tmp%istack_nod(ip-1)
          near_ele_tmp%id_near_nod(icel) = NPC_tmp2(icel)
        end do
        do icel = 1, nele_subdomain
          inum = near_ele_tmp%istack_nod(ip-1) + icel
          near_ele_tmp%id_near_nod(inum) = item_tmp_e(icel)
        end do
        near_ele_tmp%ntot = near_ele_tmp%istack_nod(NP)
!
        write(*,*) 'ip, nele_subdomain',                                &
     &             ip, nele_subdomain, near_ele_tmp%istack_nod(ip)
!
        deallocate( NPC_tmp2 )
!
      end do
!
!    copy from work array
!
      included_ele%nmax = 0
      included_ele%nmin = near_ele_tmp%ntot
      do ip= 1, NP
        included_ele%istack_nod(ip) = near_ele_tmp%istack_nod(ip)
        included_ele%num_nod(ip) = included_ele%istack_nod(ip)          &
     &                             - included_ele%istack_nod(ip-1)
        included_ele%nmax                                               &
     &             = max(included_ele%nmax,included_ele%num_nod(ip))
        included_ele%nmin                                               &
     &             = min(included_ele%nmin,included_ele%num_nod(ip))
      end do
      included_ele%ntot = near_ele_tmp%ntot
!
      call dealloc_near_node(included_ele)
      call alloc_near_element(included_ele)
!
      do inum = 1, included_ele%istack_nod(NP)
        included_ele%id_near_nod(inum)                                  &
     &              = near_ele_tmp%id_near_nod(inum)
      end do
!
      deallocate( iflag_nod )
      deallocate( iflag_ele )
      deallocate( item_tmp_e )
      call dealloc_near_node(near_ele_tmp)
      call dealloc_num_4_near_node(near_ele_tmp)
!
      end subroutine increase_overlapping
!
!   --------------------------------------------------------------------
!
      subroutine mark_extented_overlap(ip, n_overlap, i_sleeve_ele,     &
     &          numnod, numele, nnod_4_ele, ie, nodelm,                 &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod,  &
     &          nnod_s_domin, IGROUP_nod)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint), intent(in) :: n_overlap, i_sleeve_ele
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: nodelm(numele)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &              :: iele_near_nod(ntot_ele_near_nod)
!
      integer(kind = kint), intent(in) :: nnod_s_domin
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_s_domin)
!
      integer(kind= kint) :: iwidth, inum, icel, inod, k, icou
      integer(kind= kint) :: ist, ied
!
!
        icou = 0
        item_tmp_e = 0
        ist = iele_stack_near_nod(ip-1) + 1
        ied = iele_stack_near_nod(ip)
        do inum = ist, ied
          icou = icou + 1
          item_tmp_e(icou) = iele_near_nod(inum)
        end do
        nele_subdomain = icou
!
        do iwidth = 2, n_overlap
!
!   Mark belonged node include internal and external
!
          iflag_nod = 0
          do inum = 1, nele_subdomain
            icel = item_tmp_e(inum)
!
            if (iwidth.eq.2 .and. i_sleeve_ele.eq.1) then
!
              inod = ie(icel,1)
              if (IGROUP_nod(inod) .eq. ip) then
                do k =1, nodelm(icel)
                  inod = ie(icel,k)
                  iflag_nod(inod) = 1
                end do
              end if
!
            else
              do k =1, nodelm(icel)
                inod = ie(icel,k)
                iflag_nod(inod) = 1
              end do
            end if
!
          end do
!
!    Mark number of belonged element
!
          iflag_ele = 0
          do icel= 1, numele
            do k =1, nodelm(icel)
              inod = ie(icel,k)
              if ( iflag_nod(inod) .eq. 1) then
                iflag_ele(icel) = 1
              end if
            end do
          end do
!
          icou = 0
          item_tmp_e = 0
          do icel= 1, numele
            if ( iflag_ele(icel) .eq. 1) then
              icou = icou + 1
              item_tmp_e(icou) = icel
            end if
          end do
          nele_subdomain = icou
        end do
!
      end subroutine mark_extented_overlap
!
!   --------------------------------------------------------------------
!
      end module increase_overlap
