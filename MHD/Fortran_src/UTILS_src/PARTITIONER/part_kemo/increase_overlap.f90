!
!     module increase_overlap
!
!     written by H. Matsui on Sep., 2007
!
!      subroutine increase_overlapping(NP, n_overlap, i_sleeve_ele)
!
      module increase_overlap
!
      use m_precision
      use m_constants
!
      implicit  none
!
      integer(kind= kint), allocatable :: nele_subdomain
      integer(kind= kint), allocatable :: iflag_nod(:), iflag_ele(:)
      integer(kind= kint), allocatable :: item_tmp_e(:)
      integer(kind= kint), allocatable :: NPC_tmp2(:)
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
      subroutine increase_overlapping(NP, n_overlap, i_sleeve_ele)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_near_element_id_4_node
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: n_overlap, i_sleeve_ele
!
      integer(kind= kint) :: ip, inum, icel
!
!
      ntot_ele_near_nod = iele_stack_near_nod(NP)
      ntot_ele_near_nod_w = ntot_ele_near_nod
      allocate( iflag_nod(numnod) )
      allocate( iflag_ele(numele) )
      allocate( item_tmp_e(numele) )
      call allocate_num_4_near_ele_w(NP)
      call allocate_near_element_w
!
      iflag_nod = 0
      iflag_ele = 0
      item_tmp_e = 0
      do ip= 1, NP
        call mark_extented_overlap(ip, n_overlap, i_sleeve_ele,         &
     &          numnod, numele, nnod_4_ele, ie, nodelm,                 &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod,  &
     &          nnod_s_domin, IGROUP_nod)
!
        allocate( NPC_tmp2(iele_stack_near_nod_w(ip-1)) )
!
        iele_stack_near_nod_w(ip) = iele_stack_near_nod_w(ip-1)         &
     &                             + nele_subdomain
        do icel = 1, iele_stack_near_nod_w(ip-1)
          NPC_tmp2(icel) = iele_near_nod_w(icel)
        end do
!
        ntot_ele_near_nod_w = iele_stack_near_nod_w(ip)
        call deallocate_near_element_w
        call allocate_near_element_w
!
        do icel = 1, iele_stack_near_nod_w(ip-1)
          iele_near_nod_w(icel) = NPC_tmp2(icel)
        end do
        do icel = 1, nele_subdomain
          inum = iele_stack_near_nod_w(ip-1) + icel
          iele_near_nod_w(inum) = item_tmp_e(icel)
        end do
        ntot_ele_near_nod_w = iele_stack_near_nod_w(NP)
!
        write(*,*) 'ip, nele_subdomain',                               &
     &             ip, nele_subdomain, iele_stack_near_nod_w(ip)
!
        deallocate( NPC_tmp2 )
!
      end do
!
!    copy from work array
!
      nmax_ele_near_nod = 0
      nmin_ele_near_nod = ntot_ele_near_nod_w
      do ip= 1, NP
        iele_stack_near_nod(ip) = iele_stack_near_nod_w(ip)
        nele_near_nod(ip) = iele_stack_near_nod(ip)                     &
     &                     - iele_stack_near_nod(ip-1)
        nmax_ele_near_nod = max(nmax_ele_near_nod,nele_near_nod(ip))
        nmin_ele_near_nod = min(nmin_ele_near_nod,nele_near_nod(ip))
      end do
      ntot_ele_near_nod = ntot_ele_near_nod_w
!
      call deallocate_near_element
      call allocate_near_element
!
      do inum = 1, iele_stack_near_nod(NP)
        iele_near_nod(inum)= iele_near_nod_w(inum)
      end do
!
      deallocate( iflag_nod )
      deallocate( iflag_ele )
      deallocate( item_tmp_e )
      call deallocate_near_element_w
      call deallocate_num_4_near_ele_w
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
