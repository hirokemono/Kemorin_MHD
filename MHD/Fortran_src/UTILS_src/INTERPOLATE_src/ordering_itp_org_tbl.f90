!>@file   ordering_itp_org_tbl.f90
!!@brief  module ordering_itp_org_tbl
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Oct., 2013
!
!> @brief Ordering interpolation table
!!        by interpolation type
!!
!!@verbatim
!!      subroutine ordering_itp_orgin_tbl_t(cst_itp_wk, itp_org)
!!        type(work_const_itp_table), intent(inout) :: cst_itp_wk
!!        type(interpolate_table_org), intent(inout) :: itp_org
!!@endverbatim
!
      module ordering_itp_org_tbl
!
      use m_precision
!
      implicit none
!
      private :: ordering_itp_orgin_table
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine ordering_itp_orgin_tbl_t(cst_itp_wk, itp_org)
!
      use t_work_const_itp_table
      use t_interpolate_tbl_org
      use copy_interpolate_types
!
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
      type(interpolate_table_org), intent(inout) :: itp_org
!
!
      call ordering_itp_orgin_table                                     &
     &   (itp_org%num_dest_domain, itp_org%ntot_table_org,              &
     &    cst_itp_wk%istack_org_para_type,                              &
     &    itp_org%istack_itp_type_org, itp_org%inod_itp_send,           &
     &    itp_org%inod_gl_dest_4_org, itp_org%iele_org_4_org,           &
     &    itp_org%itype_inter_org, itp_org%coef_inter_org)
      call dealloc_istack_org_ptype(cst_itp_wk)
!
      end subroutine ordering_itp_orgin_tbl_t
!
!-----------------------------------------------------------------------
!
      subroutine ordering_itp_orgin_table                               &
     &         (num_dest_domain, ntot_table_org, istack_org_para_type,  &
     &          istack_itp_type_org, inod_itp_send,                     &
     &          inod_gl_dest_4_org, iele_org_4_org,                     &
     &          itype_inter_org, coef_inter_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: num_dest_domain
      integer(kind = kint), intent(in) :: ntot_table_org
      integer(kind = kint), intent(in)                                  &
     &            :: istack_org_para_type(0:4*num_dest_domain)
!
      integer(kind = kint), intent(inout) :: istack_itp_type_org(0:4)
      integer(kind = kint), intent(inout)                               &
     &                     :: inod_itp_send(ntot_table_org)
      integer(kind = kint), intent(inout)                               &
     &                     :: inod_gl_dest_4_org(ntot_table_org)
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_org_4_org(ntot_table_org)
      integer(kind = kint), intent(inout)                               &
     &                     :: itype_inter_org(ntot_table_org)
      real(kind = kreal), intent(inout)                                 &
     &                     :: coef_inter_org(ntot_table_org,3)
!
      type(interpolate_table_org) :: itp_otmp
      integer(kind = kint) :: ip, j, icou, num, ist, iorg, inum
!
!
      if(num_dest_domain .eq. 0) return
!
      itp_otmp%ntot_table_org = ntot_table_org
      call alloc_itp_table_org(itp_otmp)
!
      do inum = 1, ntot_table_org
        itp_otmp%inod_itp_send(inum) = inod_itp_send(inum)
        itp_otmp%inod_gl_dest_4_org(inum) = inod_gl_dest_4_org(inum)
        itp_otmp%iele_org_4_org(inum) = iele_org_4_org(inum)
        itp_otmp%itype_inter_org(inum) = itype_inter_org(inum)
        itp_otmp%coef_inter_org(inum,1) = coef_inter_org(inum,1)
        itp_otmp%coef_inter_org(inum,2) = coef_inter_org(inum,2)
        itp_otmp%coef_inter_org(inum,3) = coef_inter_org(inum,3)
      end do
!
      istack_itp_type_org = 0
      do j = 1, 4
        istack_itp_type_org(j) = istack_itp_type_org(j-1)
        do ip = 1, num_dest_domain
          istack_itp_type_org(j) = istack_itp_type_org(j)               &
     &            + istack_org_para_type(4*(ip-1)+j)                    &
     &            - istack_org_para_type(4*(ip-1)+j-1)
        end do
      end do
!
      do j = 1, 4
        icou = istack_itp_type_org(j-1)
        do ip = 1, num_dest_domain
          num = istack_org_para_type(4*(ip-1)+j)                        &
     &         - istack_org_para_type(4*(ip-1)+j-1)
          ist = istack_org_para_type(4*(ip-1)+j-1)
          do inum = 1, num
            iorg = ist + inum
            icou = icou + 1
!
            inod_itp_send(iorg) = icou
            inod_gl_dest_4_org(icou)                                    &
     &                           = itp_otmp%inod_gl_dest_4_org(iorg)
            iele_org_4_org(icou) =  itp_otmp%iele_org_4_org(iorg)
            itype_inter_org(icou) = itp_otmp%itype_inter_org(iorg)
            coef_inter_org(icou,1)  = itp_otmp%coef_inter_org(iorg,1)
            coef_inter_org(icou,2)  = itp_otmp%coef_inter_org(iorg,2)
            coef_inter_org(icou,3)  = itp_otmp%coef_inter_org(iorg,3)
          end do
        end do
      end do
!
      call dealloc_itp_table_org(itp_otmp)
!
      end subroutine ordering_itp_orgin_table
!
!-----------------------------------------------------------------------
!
      end module ordering_itp_org_tbl
