!copy_num_itp_wtype_org2dst.f90
!
!      module copy_num_itp_wtype_org2dst
!
      module copy_num_itp_wtype_org2dst
!
!     Written by H. Matsui on May, 2010
!
      use m_precision
!
      implicit none
!
      private :: copy_istack_itp_wtype_org2dest
!
!      subroutine copy_stk_type_itp_wtp_org2dest(itp_info)
!      subroutine copy_istack_itp_wtype_org2dest
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_stk_type_itp_wtp_org2dest(itp_info)
!
      use t_interpolate_table
!
      type(interpolate_table), intent(inout) :: itp_info
!
!
      call copy_stk_itp_wtp_org2dest                                    &
     &   (itp_info%tbl_org%num_dest_domain,                             &
     &    itp_info%tbl_org%istack_nod_tbl_wtype_org,                    &
     &    itp_info%tbl_dest%num_org_domain,                             &
     &    itp_info%tbl_dest%istack_nod_tbl_wtype_dest)
!
      end subroutine copy_stk_type_itp_wtp_org2dest
!
!------------------------------------------------------------------
!
      subroutine copy_istack_itp_wtype_org2dest
!
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
!
      call copy_stk_itp_wtp_org2dest                                    &
     &    (num_dest_domain, istack_nod_table_wtype_org,                 &
     &     num_org_domain, istack_nod_table_wtype_dest)
!
      end subroutine copy_istack_itp_wtype_org2dest
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_stk_itp_wtp_org2dest(num_dest_domain,             &
     &        istack_nod_tbl_wtype_org, num_org_domain,                 &
     &        istack_nod_tbl_wtype_dest)
!
      integer(kind = kint), intent(in) :: num_dest_domain
      integer(kind = kint), intent(in)                                  &
     &         :: istack_nod_tbl_wtype_org(0:4*num_dest_domain)
      integer(kind = kint), intent(in) :: num_org_domain
      integer(kind = kint), intent(inout)                               &
     &         :: istack_nod_tbl_wtype_dest(0:4*num_org_domain)
!
      integer(kind = kint) :: i
!
!
      istack_nod_tbl_wtype_dest(0) = istack_nod_tbl_wtype_org(0)
      do i = 1, num_dest_domain
        istack_nod_tbl_wtype_dest(4*i-3)                                &
     &     = istack_nod_tbl_wtype_org(4*i-3)
        istack_nod_tbl_wtype_dest(4*i-2)                                &
     &     = istack_nod_tbl_wtype_org(4*i-2)
        istack_nod_tbl_wtype_dest(4*i-1)                                &
     &     = istack_nod_tbl_wtype_org(4*i-1)
        istack_nod_tbl_wtype_dest(4*i  )                                &
     &     = istack_nod_tbl_wtype_org(4*i  )
      end do
!
      end subroutine copy_stk_itp_wtp_org2dest
!
!------------------------------------------------------------------
!
      end module copy_num_itp_wtype_org2dst
