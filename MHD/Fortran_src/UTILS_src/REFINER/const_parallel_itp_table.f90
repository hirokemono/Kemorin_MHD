!const_parallel_itp_table.f90
!      module const_parallel_itp_table
!
      module const_parallel_itp_table
!
!      Written by Kemorin on May, 2010
!
      use m_precision
!
      use m_machine_parameter
      use t_interpolate_table
      use t_interpolate_table_orgin
      use t_interpolate_tbl_dest
!
      implicit    none
!
      private :: s_const_parallel_itp_tbl_dest
      private :: s_const_parallel_itp_tbl_org
!
!      subroutine s_const_parallel_itp_table(nprocs_org, nprocs_tgt,    &
!     &          nprocs_larger, itp_sgl, itp_para, nele_org_1pe,        &
!     &          iele_local_org, ipe_ele_local_org, nnod_tgt_1pe,       &
!     &          inod_local_tgt,  ipe_nod_local_tgt)
!       type(interpolate_table), intent(inout) :: itp_para(nprocs_larger)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_parallel_itp_table(nprocs_org, nprocs_tgt,     &
     &          nprocs_larger, itp_sgl, itp_para, nele_org_1pe,         &
     &          iele_local_org, ipe_ele_local_org, nnod_tgt_1pe,        &
     &          inod_local_tgt,  ipe_nod_local_tgt)
!
      integer(kind = kint), intent(in) :: nprocs_org
      integer(kind = kint), intent(in) :: nprocs_tgt
      integer(kind = kint), intent(in) :: nprocs_larger
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in) :: iele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: ipe_ele_local_org(nele_org_1pe)
!
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in) :: inod_local_tgt(nnod_tgt_1pe)
      integer(kind = kint), intent(in) :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      type(interpolate_table), intent(inout) :: itp_para(nprocs_larger)
!
!
      call s_const_parallel_itp_tbl_dest(nprocs_org,                    &
     &          nprocs_larger, itp_sgl, itp_para, nele_org_1pe,         &
     &          ipe_ele_local_org, nnod_tgt_1pe, inod_local_tgt,        &
     &          ipe_nod_local_tgt)
      call s_const_parallel_itp_tbl_org(nprocs_tgt,                     &
     &          nprocs_larger, itp_sgl, itp_para,                       &
     &          nele_org_1pe, iele_local_org, ipe_ele_local_org,        &
     &          nnod_tgt_1pe, ipe_nod_local_tgt)
!
      end subroutine s_const_parallel_itp_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_const_parallel_itp_tbl_dest(nprocs_org,              &
     &          nprocs_larger, itp_sgl, itp_para, nele_org_1pe,         &
     &          ipe_ele_local_org, nnod_tgt_1pe, inod_local_tgt,        &
     &          ipe_nod_local_tgt)
!
      use const_parallel_itp_tbl_dest
!
      integer(kind = kint), intent(in) :: nprocs_org, nprocs_larger
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in) :: ipe_ele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in) :: inod_local_tgt(nnod_tgt_1pe)
      integer(kind = kint), intent(in) :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      type(interpolate_table), intent(inout) :: itp_para(nprocs_larger)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs_larger
        call count_num_org_domain_para_itp(ip, nprocs_org, itp_sgl,     &
     &      nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,              &
     &      ipe_nod_local_tgt, itp_para(ip)%tbl_dest%num_org_domain)
!
        call alloc_type_itp_num_dest(itp_para(ip)%tbl_dest)
!
        call set_id_org_domain_para_itp(ip, nprocs_org, itp_sgl,        &
     &      nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,              &
     &      ipe_nod_local_tgt, itp_para(ip)%tbl_dest)
        call set_num_nod_tbl_dest_para_itp(ip, itp_sgl,                 &
     &      nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,              &
     &      ipe_nod_local_tgt, itp_para(ip)%tbl_dest)
!
        call alloc_type_itp_table_dest(itp_para(ip)%tbl_dest)
!
        call set_node_tbl_dest_para_itp(ip, itp_sgl,                    &
     &      nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,              &
     &      inod_local_tgt, ipe_nod_local_tgt, itp_para(ip)%tbl_dest)
      end do
!
      end subroutine s_const_parallel_itp_tbl_dest
!
! -----------------------------------------------------------------------
!
      subroutine s_const_parallel_itp_tbl_org(nprocs_tgt,               &
     &          nprocs_larger, itp_sgl, itp_para,                       &
     &          nele_org_1pe, iele_local_org, ipe_ele_local_org,        &
     &          nnod_tgt_1pe, ipe_nod_local_tgt)
!
      use const_parallel_itp_tbl_org
!
      integer(kind = kint), intent(in) :: nprocs_tgt
      integer(kind = kint), intent(in) :: nprocs_larger
      type(interpolate_table), intent(in) :: itp_sgl
!
      integer(kind = kint), intent(in) :: nele_org_1pe
      integer(kind = kint), intent(in) :: iele_local_org(nele_org_1pe)
      integer(kind = kint), intent(in) :: ipe_ele_local_org(nele_org_1pe)
!
      integer(kind = kint), intent(in) :: nnod_tgt_1pe
      integer(kind = kint), intent(in) :: ipe_nod_local_tgt(nnod_tgt_1pe)
!
      type(interpolate_table), intent(inout) :: itp_para(nprocs_larger)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs_larger
        call count_num_dest_domain_para_itp(ip, nprocs_tgt, itp_sgl,    &
     &      nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,              &
     &      ipe_nod_local_tgt, itp_para(ip)%tbl_org%num_dest_domain)
!
        call alloc_type_itp_num_org(itp_para(ip)%tbl_org)
!
        call set_id_dest_domain_para_itp(ip, nprocs_tgt, itp_sgl,       &
     &      nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,              &
     &      ipe_nod_local_tgt, itp_para(ip)%tbl_org)
        call set_num_nod_tbl_org_para_itp(ip, itp_sgl,                  &
     &      nele_org_1pe, ipe_ele_local_org, nnod_tgt_1pe,              &
     &      ipe_nod_local_tgt, itp_para(ip)%tbl_org)
!
        call alloc_type_itp_table_org(itp_para(ip)%tbl_org)
!
        call set_elem_tbl_org_para_itp(ip, itp_sgl,                     &
     &      nele_org_1pe, iele_local_org, ipe_ele_local_org,            &
     &      nnod_tgt_1pe, ipe_nod_local_tgt, itp_para(ip)%tbl_org)
!
        call alloc_type_istack_tbl_wtp_smp(np_smp,                      &
     &      itp_para(ip)%tbl_org)
      end do
!
      end subroutine s_const_parallel_itp_tbl_org
!
! -----------------------------------------------------------------------
!
      end module const_parallel_itp_table
