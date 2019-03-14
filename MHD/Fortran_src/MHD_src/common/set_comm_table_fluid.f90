!
!      module set_comm_table_fluid
!
!     Programmed by H.Matsui on July, 2002
!     Modified by H. Matsui on Sep., 2007
!     Modified by H. Matsui on Apr., 2008
!
!      subroutine allocate_flags_reduced_comm(num_pe, numnod)
!      subroutine deallocate_flags_reduced_comm
!
!      subroutine mark_4_fluid_nod_by_ele(numele, nnod_4_ele, ie,       &
!     &          istack_0, istack_mp)
!      subroutine mark_reduced_neib_domain(num_neib, ntot_import,       &
!     &          ntot_export, id_neib, istack_import, istack_export,    &
!     &          item_import, item_export)
!
!      subroutine count_reduced_neib_domain(num_neib, id_neib,          &
!     &          num_neib_new)
!      subroutine set_reduced_neib_domain(num_neib, id_neib,            &
!     &          num_neib_new, id_neib_new)
!      subroutine count_reduced_comm_stack(num_neib, id_neib,           &
!     &          istack_comm, num_neib_new, ntot_comm_new,              &
!     &          num_comm_new, istack_comm_new)
!      subroutine set_reduced_comm_item(num_neib, ntot_comm, id_neib,   &
!     &          istack_comm, item_comm, num_neib_new,                  &
!     &          ntot_comm_new, istack_comm_new, item_comm_new)
!
      module set_comm_table_fluid
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), allocatable :: iflag_pe(:)
      integer(kind = kint), allocatable :: iflag_nod(:)
      private :: iflag_pe
!
! ---------------------------------------------------------------------
!
      contains
!
! ---------------------------------------------------------------------
!
      subroutine allocate_flags_reduced_comm(num_pe, numnod)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate( iflag_pe(0:num_pe-1) )
      allocate( iflag_nod(numnod) )
!
      iflag_pe =  0
      iflag_nod = 0
!
      end subroutine allocate_flags_reduced_comm
!
! ---------------------------------------------------------------------
!
      subroutine deallocate_flags_reduced_comm
!
!
      deallocate(iflag_pe, iflag_nod)
!
      end subroutine deallocate_flags_reduced_comm
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine mark_4_fluid_nod_by_ele(numele, nnod_4_ele, ie,        &
     &          istack_0, istack_mp)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: istack_0, istack_mp
!
      integer(kind = kint) :: inod, k1, iele
!
      iflag_nod = 0
      do k1 = 1, nnod_4_ele
        do iele = istack_0+1, istack_mp
          inod = ie(iele,k1)
          iflag_nod(inod) = 1
        end do
      end do
!
      end subroutine mark_4_fluid_nod_by_ele
!
! ---------------------------------------------------------------------
!
      subroutine mark_reduced_neib_domain(num_neib, ntot_import,        &
     &          ntot_export, id_neib, istack_import, istack_export,     &
     &          item_import, item_export)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: ntot_import, ntot_export
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in) :: item_import(ntot_import)
      integer(kind = kint), intent(in) :: item_export(ntot_export)
!
      integer(kind = kint) :: ip, ip_domain, inum, inod
!
!
      iflag_pe = 0
      do ip = 1, num_neib
        do inum = istack_import(ip-1)+1, istack_import(ip)
          inod = item_import(inum)
          if (iflag_nod(inod).gt.0) then
            ip_domain = id_neib(ip)
            iflag_pe(ip_domain) = 1
            exit
          end if
        end do
        do inum = istack_export(ip-1)+1, istack_export(ip)
          inod = item_export(inum)
          if (iflag_nod(inod) .gt. 0) then
            ip_domain = id_neib(ip)
            iflag_pe(ip_domain) = 1
            exit
          end if
        end do
      end do
!
      end subroutine mark_reduced_neib_domain
!
! ---------------------------------------------------------------------
!
      subroutine count_reduced_neib_domain(num_neib, id_neib,           &
     &          num_neib_new)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(inout) :: num_neib_new
!
      integer(kind = kint) :: ip, ip_domain
!
!
      num_neib_new = 0
      do ip = 1, num_neib
        ip_domain = id_neib(ip)
        num_neib_new = num_neib_new + iflag_pe(ip_domain)
      end do
!
      end subroutine count_reduced_neib_domain
!
! ---------------------------------------------------------------------
!
      subroutine set_reduced_neib_domain(num_neib, id_neib,             &
     &          num_neib_new, id_neib_new)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: num_neib_new
!
      integer(kind = kint), intent(inout) :: id_neib_new(num_neib_new)
!
      integer(kind = kint) :: ip, ip_domain, icou_pe
!
!
      icou_pe = 0
      do ip = 1, num_neib
        ip_domain = id_neib(ip)
        if (iflag_pe(ip_domain) .gt. 0) then
          icou_pe = icou_pe + 1
          id_neib_new(icou_pe) = ip_domain
        end if
      end do
!
      end subroutine set_reduced_neib_domain
!
! ---------------------------------------------------------------------
!
      subroutine count_reduced_comm_stack(num_neib, ntot_comm, id_neib, &
     &          istack_comm, item_comm, num_neib_new, ntot_comm_new,    &
     &          num_comm_new, istack_comm_new)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_comm
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_comm(0:num_neib)
      integer(kind = kint), intent(in) :: item_comm(ntot_comm)
!
      integer(kind = kint), intent(in) :: num_neib_new
!
      integer(kind = kint), intent(inout) :: num_comm_new(num_neib_new)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_comm_new(0:num_neib_new)
      integer(kind = kint), intent(inout) :: ntot_comm_new
!
      integer(kind = kint) :: ip, ip_domain, icou_pe, inum, inod
!
!
      icou_pe = 0
      do ip = 1, num_neib
        ip_domain = id_neib(ip)
        if ( iflag_pe(ip_domain) .gt. 0) then
          icou_pe = icou_pe + 1
          do inum = istack_comm(ip-1)+1, istack_comm(ip)
            inod = item_comm(inum)
            num_comm_new(icou_pe)  = num_comm_new(icou_pe)              &
     &                              + iflag_nod(inod)
          end do
          istack_comm_new(icou_pe) = istack_comm_new(icou_pe-1)         &
     &                              + num_comm_new(icou_pe)
        end if
      end do
      ntot_comm_new = istack_comm_new(num_neib_new)
!
      end subroutine count_reduced_comm_stack
!
! ---------------------------------------------------------------------
!
      subroutine set_reduced_comm_item(num_neib, ntot_comm, id_neib,    &
     &          istack_comm, item_comm, num_neib_new,                   &
     &          ntot_comm_new, istack_comm_new, item_comm_new)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_comm
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_comm(0:num_neib)
      integer(kind = kint), intent(in) :: item_comm(ntot_comm)
!
      integer(kind = kint), intent(in) :: num_neib_new, ntot_comm_new
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_comm_new(0:num_neib_new)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: item_comm_new(ntot_comm_new)
!
      integer(kind = kint) :: ip, ip_domain, icou, icou_pe, inum, inod
!
!
      icou_pe = 0
      do ip = 1, num_neib
        ip_domain = id_neib(ip)
        if ( iflag_pe(ip_domain) .gt. 0) then
          icou_pe = icou_pe + 1
          icou = istack_comm_new(icou_pe-1)
          do inum = istack_comm(ip-1)+1, istack_comm(ip)
            inod = item_comm(inum)
            if( iflag_nod(inod) .gt. 0) then
              icou = icou + 1
              item_comm_new(icou)  = inod
            end if
          end do
        end if
      end do
!
      end subroutine set_reduced_comm_item
!
! ---------------------------------------------------------------------
!
      end module set_comm_table_fluid
