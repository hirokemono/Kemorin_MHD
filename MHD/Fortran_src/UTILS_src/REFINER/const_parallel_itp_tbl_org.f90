!const_parallel_itp_tbl_org.f90
!      module const_parallel_itp_tbl_org
!
      module const_parallel_itp_tbl_org
!
!      Written by Kemorin on May, 2010
!
      use m_precision
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit none
!
!      subroutine count_num_dest_domain_para_itp(ip_org, nprocs_tgt,    &
!     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,  &
!     &        ipe_nod_local_tgt, num_dest_domain)
!        integer(kind = kint), intent(inout) :: num_dest_domain
!
!      subroutine set_id_dest_domain_para_itp(ip_org, nprocs_tgt,       &
!     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,  &
!     &        ipe_nod_local_tgt, itp_org_para)
!        type(interpolate_table_org), intent(inout) :: itp_org_para
!
!      subroutine set_num_nod_tbl_org_para_itp(ip_org, itp_sgl,         &
!     &          nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,         &
!     &          ipe_nod_local_tgt, itp_org_para)
!        type(interpolate_table_org), intent(inout) :: itp_org_para
!
!      subroutine set_elem_tbl_org_para_itp(ip_org, itp_sgl,            &
!     &          nele_org_1pe, iele_local_org, ipe_ele_local_org,       &
!     &          nnod_tgt_1pe, ipe_nod_local_tgt, itp_org_para)
!        type(interpolate_table_org), intent(inout) :: itp_org_para
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_num_dest_domain_para_itp(ip_org, nprocs_tgt,     &
     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,   &
     &        ipe_nod_local_tgt, num_dest_domain)
!
      integer(kind = kint), intent(in) :: ip_org
      integer(kind = kint), intent(in) :: nprocs_tgt
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      integer(kind = kint), intent(inout) :: num_dest_domain
!
!
      integer(kind = kint) :: ip_tgt, jp, inum, inod_tgt, iele_org
!
!
      num_dest_domain = 0
      do jp = 1, nprocs_tgt
        ip_tgt = mod(ip_org+jp-1,nprocs_tgt) + 1
        do inum = 1, itp_sgl%tbl_dest%ntot_table_dest
          inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
          iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
          if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                  &
     &       .and. ipe_ele_local_org(iele_org).eq.ip_org) then
              num_dest_domain = num_dest_domain+1
              exit
          end if
        end do
      end do
!
      end subroutine count_num_dest_domain_para_itp
!
! -----------------------------------------------------------------------
!
      subroutine set_id_dest_domain_para_itp(ip_org, nprocs_tgt,        &
     &        itp_sgl, nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,   &
     &        ipe_nod_local_tgt, itp_org_para)
!
      integer(kind = kint), intent(in) :: ip_org
      integer(kind = kint), intent(in) :: nprocs_tgt
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      type(interpolate_table_org), intent(inout) :: itp_org_para
!
!
      integer(kind = kint) :: ip_tgt, jp, inum, icou
      integer(kind = kint) :: inod_tgt, iele_org
!
!
      icou = 0
      do jp = 1, nprocs_tgt
        ip_tgt = mod(ip_org+jp-1,nprocs_tgt) + 1
        do inum = 1, itp_sgl%tbl_dest%ntot_table_dest
          inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
          iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
          if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                  &
     &       .and. ipe_ele_local_org(iele_org).eq.ip_org) then
              icou = icou+1
              itp_org_para%id_dest_domain(icou) = ip_tgt - 1
              exit
          end if
        end do
      end do
!
      end subroutine set_id_dest_domain_para_itp
!
! -----------------------------------------------------------------------
!
      subroutine set_num_nod_tbl_org_para_itp(ip_org, itp_sgl,          &
     &          nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,          &
     &          ipe_nod_local_tgt, itp_org_para)
!
      type(interpolate_table), intent(in) :: itp_sgl
      integer(kind = kint), intent(in) :: ip_org
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      type(interpolate_table_org), intent(inout) :: itp_org_para
!
      integer(kind = kint) :: ip_tgt, jp, jnum, ncou
      integer(kind = kint) :: i, ist, ied, inum, inod_tgt, iele_org
!
!
      itp_org_para%istack_nod_tbl_org(0) = 0
      itp_org_para%istack_nod_tbl_wtype_org(0) = 0
      do jp = 1, itp_org_para%num_dest_domain
        ip_tgt = itp_org_para%id_dest_domain(jp) + 1
!
        do i = 1, 4
          jnum = 4*(jp-1) + i
          ist = itp_sgl%tbl_org%istack_nod_tbl_wtype_org(i-1) + 1
          ied = itp_sgl%tbl_org%istack_nod_tbl_wtype_org(i)
!
          ncou = 0
          do inum = ist, ied
            inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
            iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
            if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                &
     &       .and. ipe_ele_local_org(iele_org).eq.ip_org) then
               ncou = ncou + 1
            end if
          end do
          itp_org_para%istack_nod_tbl_wtype_org(jnum)                   &
     &      = itp_org_para%istack_nod_tbl_wtype_org(jnum-1) + ncou
        end do
!
        itp_org_para%istack_nod_tbl_org(jp)                             &
     &       = itp_org_para%istack_nod_tbl_wtype_org(4*jp)
      end do
!
      jp = itp_org_para%num_dest_domain
      itp_org_para%ntot_table_org                                       &
     &    =  itp_org_para%istack_nod_tbl_org(jp)
!
      end subroutine set_num_nod_tbl_org_para_itp
!
! -----------------------------------------------------------------------
!
      subroutine set_elem_tbl_org_para_itp(ip_org, itp_sgl,             &
     &          nele_org_1pe, iele_local_org, ipe_ele_local_org,        &
     &          nnod_tgt_1pe, ipe_nod_local_tgt, itp_org_para)
!
      type(interpolate_table), intent(in) :: itp_sgl
      integer(kind = kint), intent(in) :: ip_org
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in) :: iele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in)                                  &
     &              :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      type(interpolate_table_org), intent(inout) :: itp_org_para
!
      integer(kind = kint) :: ip_tgt, jp, jnum, jcou
      integer(kind = kint) :: i, ist, ied, inum, inod_tgt, iele_org
!
!
      do jp = 1, itp_org_para%num_dest_domain
        ip_tgt = itp_org_para%id_dest_domain(jp) + 1
!
        do i = 1, 4
          jnum = 4*(jp-1) + i
          ist = itp_sgl%tbl_org%istack_nod_tbl_wtype_org(i-1) + 1
          ied = itp_sgl%tbl_org%istack_nod_tbl_wtype_org(i)
!
          jcou = itp_org_para%istack_nod_tbl_wtype_org(jnum-1)
          do inum = ist, ied
            inod_tgt = itp_sgl%tbl_dest%inod_dest_4_dest(inum)
            iele_org = itp_sgl%tbl_org%iele_org_4_org(inum)
            if(    ipe_nod_local_tgt(inod_tgt).eq.ip_tgt                &
     &       .and. ipe_ele_local_org(iele_org).eq.ip_org) then
               jcou = jcou + 1
               itp_org_para%inod_itp_send(jcou) = jcou
               itp_org_para%inod_gl_dest_4_org(jcou)                    &
     &             = inod_tgt
               itp_org_para%iele_org_4_org(jcou)                        &
     &             = iele_local_org(iele_org)
               itp_org_para%itype_inter_org(jcou)                       &
     &             = itp_sgl%tbl_org%itype_inter_org(inum)
               itp_org_para%coef_inter_org(jcou,1)                      &
     &             = itp_sgl%tbl_org%coef_inter_org(inum,1)
               itp_org_para%coef_inter_org(jcou,2)                      &
     &             = itp_sgl%tbl_org%coef_inter_org(inum,2)
               itp_org_para%coef_inter_org(jcou,3)                      &
     &             = itp_sgl%tbl_org%coef_inter_org(inum,3)
            end if
          end do
        end do
      end do
!
      end subroutine set_elem_tbl_org_para_itp
!
! -----------------------------------------------------------------------
!
      end module const_parallel_itp_tbl_org
