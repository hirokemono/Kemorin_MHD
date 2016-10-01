!const_parallel_itp_tbl_dest.f90
!      module const_parallel_itp_tbl_dest
!
!      Written by Kemorin on May, 2010
!
!      subroutine alloc_itp_stack_dest_wk_rfne(num_org_domain, dest_wk)
!      subroutine dealloc_itp_stack_dest_wk_rfne(dest_wk)
!
!      subroutine count_num_org_domain_para_itp(ip_tgt, nprocs_org,     &
!     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,  &
!     &        ipe_nod_local_tgt,  num_org_domain)
!        integer(kind = kint), intent(inout) :: num_org_domain
!      subroutine set_id_org_domain_para_itp(ip_tgt, nprocs_org,        &
!     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,  &
!     &        ipe_nod_local_tgt, itp_dst_para)
!        type(interpolate_table_dest), intent(inout) :: itp_dst_para
!      subroutine set_num_nod_tbl_dest_para_itp(ip_tgt, itp_sgl,        &
!     &          nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,         &
!     &          ipe_nod_local_tgt, itp_dst_para)
!        type(interpolate_table_dest), intent(inout) :: itp_dst_para
!      subroutine set_node_tbl_dest_para_itp(ip_tgt, itp_sgl,           &
!     &          nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,         &
!     &          inod_local_tgt, ipe_nod_local_tgt, itp_dst_para)
!        type(interpolate_table_dest), intent(inout) :: itp_dst_para
!
      module const_parallel_itp_tbl_dest
!
      use m_precision
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit    none
!
!> Structure of interpolation table for target grid
      type itp_stack_dest_wk_refine
!>   end address to receive interpolated data including interpolate type
        integer(kind = kint), allocatable :: istack_tbl_wt_dest(:)
      end type itp_stack_dest_wk_refine
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_itp_stack_dest_wk_rfne(num_org_domain, dest_wk)
!
      integer(kind = kint), intent(in) :: num_org_domain
      type(itp_stack_dest_wk_refine), intent(inout) :: dest_wk
!
!
      allocate( dest_wk%istack_tbl_wt_dest(0:4*num_org_domain) )
      dest_wk%istack_tbl_wt_dest = 0
!
      end subroutine alloc_itp_stack_dest_wk_rfne
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_stack_dest_wk_rfne(dest_wk)
!
      type(itp_stack_dest_wk_refine), intent(inout) :: dest_wk
!
!
      deallocate( dest_wk%istack_tbl_wt_dest )
!
      end subroutine dealloc_itp_stack_dest_wk_rfne
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_org_domain_para_itp(ip_tgt, nprocs_org,      &
     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,   &
     &        ipe_nod_local_tgt, num_org_domain)
!
      integer(kind = kint), intent(in) :: ip_tgt
      integer(kind = kint), intent(in) :: nprocs_org
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      integer(kind = kint), intent(inout) :: num_org_domain
!
!
      integer(kind = kint) :: ip_org, jp, inum, inod_tgt, iele_org
!
!
      num_org_domain = 0
      do jp = 1, nprocs_org
        ip_org = mod(ip_tgt+jp-1,nprocs_org) + 1
        do inum = 1, itp_sgl%tbl_dest%ntot_table_dest
          inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
          iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
          if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                  &
     &       .and. ipe_ele_local_org(iele_org).eq.ip_org) then
              num_org_domain = num_org_domain+1
              exit
          end if
        end do
      end do
!
      end subroutine count_num_org_domain_para_itp
!
! -----------------------------------------------------------------------
!
      subroutine set_id_org_domain_para_itp(ip_tgt, nprocs_org,         &
     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,   &
     &        ipe_nod_local_tgt, itp_dst_para)
!
      integer(kind = kint), intent(in) :: ip_tgt
      integer(kind = kint), intent(in) :: nprocs_org
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      type(interpolate_table_dest), intent(inout) :: itp_dst_para
!
      integer(kind = kint) :: ip_org, jp, inum, icou
      integer(kind = kint) :: inod_tgt, iele_org
!
!
      icou = 0
      do jp = 1, nprocs_org
        ip_org = mod(ip_tgt+jp-1,nprocs_org) + 1
        do inum = 1, itp_sgl%tbl_dest%ntot_table_dest
          inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
          iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
          if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                  &
     &       .and. ipe_ele_local_org(iele_org).eq.ip_org) then
              icou = icou+1
              itp_dst_para%id_org_domain(icou) = ip_org - 1
              exit
          end if
        end do
      end do
!
      end subroutine set_id_org_domain_para_itp
!
! -----------------------------------------------------------------------
!
      subroutine set_num_nod_tbl_dest_para_itp(ip_tgt, itp_sgl,         &
     &        dest_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,  &
     &        ipe_nod_local_tgt, dest_para, itp_dst_para)
!
      type(interpolate_table), intent(in) :: itp_sgl
      integer(kind = kint), intent(in) :: ip_tgt
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
      type(itp_stack_dest_wk_refine), intent(in) :: dest_sgl
!
      type(itp_stack_dest_wk_refine), intent(inout) :: dest_para
      type(interpolate_table_dest), intent(inout) :: itp_dst_para
!
      integer(kind = kint) :: jp, jp0, jnum, ncou
      integer(kind = kint) :: i, ist, ied, inum, inod_tgt, iele_org
!
!
      dest_para%istack_tbl_wt_dest(0) = 0
      do jp0 = 1, itp_dst_para%num_org_domain
        jp = itp_dst_para%id_org_domain(jp0) + 1
        do i = 1, 4
          jnum = 4*(jp0-1) + i
          ist = dest_sgl%istack_tbl_wt_dest(i-1) + 1
          ied = dest_sgl%istack_tbl_wt_dest(i)
!
          ncou = 0
          do inum = ist, ied
            inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
            iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
            if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                &
     &       .and. ipe_ele_local_org(iele_org).eq.jp) then
               ncou = ncou + 1
            end if
          end do
          dest_para%istack_tbl_wt_dest(jnum)                            &
     &      = dest_para%istack_tbl_wt_dest(jnum-1) + ncou
        end do
      end do
!
      itp_dst_para%istack_nod_tbl_dest(0) = 0
      do jp0 = 1, itp_dst_para%num_org_domain
        itp_dst_para%istack_nod_tbl_dest(jp0)                           &
     &     = dest_para%istack_tbl_wt_dest(4*jp0)
      end do
      jp0 = itp_dst_para%num_org_domain
      itp_dst_para%ntot_table_dest                                      &
     &    = itp_dst_para%istack_nod_tbl_dest(jp0)
!
      end subroutine set_num_nod_tbl_dest_para_itp
!
! -----------------------------------------------------------------------
!
      subroutine set_node_tbl_dest_para_itp(ip_tgt, itp_sgl,            &
     &        dest_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,  &
     &        inod_local_tgt, ipe_nod_local_tgt, dest_para,             &
     &        itp_dst_para)
!
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: ip_tgt
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
      integer(kind = kint), intent(in)                                  &
     &              :: inod_local_tgt(nnod_tgt_1pe)
      type(itp_stack_dest_wk_refine), intent(in) :: dest_sgl
      type(itp_stack_dest_wk_refine), intent(in) :: dest_para
!
      type(interpolate_table_dest), intent(inout) :: itp_dst_para
!
      integer(kind = kint) :: ip_org, jp, jnum, jcou
      integer(kind = kint) :: i, ist, ied, inum, inod_tgt, iele_org
!
!
      do jp = 1, itp_dst_para%num_org_domain
        ip_org = itp_dst_para%id_org_domain(jp) + 1
!
        do i = 1, 4
          jnum = 4*(jp-1) + i
          ist = dest_sgl%istack_tbl_wt_dest(i-1) + 1
          ied = dest_sgl%istack_tbl_wt_dest(i)
!
          jcou = dest_para%istack_tbl_wt_dest(jnum-1)
          do inum = ist, ied
            inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
            iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
            if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                &
     &       .and. ipe_ele_local_org(iele_org).eq.ip_org) then
              jcou = jcou + 1
              itp_dst_para%inod_dest_4_dest(jcou)                       &
     &             = inod_local_tgt(inod_tgt)
            end if
          end do
        end do
      end do
!
      end subroutine set_node_tbl_dest_para_itp
!
! -----------------------------------------------------------------------
!
      end module const_parallel_itp_tbl_dest
