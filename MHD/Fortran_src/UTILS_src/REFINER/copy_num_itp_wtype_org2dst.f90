!copy_num_itp_wtype_org2dst.f90
!
!      module copy_num_itp_wtype_org2dst
!
!     Written by H. Matsui on May, 2010
!
!      subroutine copy_stk_type_itp_wtp_org2dest(itp_info)
!
      module copy_num_itp_wtype_org2dst
!
      use m_precision
!
      implicit none
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
      itp_info%tbl_dest%istack_nod_tbl_wtype_dest(0:4)                  &
     &   = itp_info%tbl_org%istack_itp_type_org(0:4)
!
      end subroutine copy_stk_type_itp_wtp_org2dest
!
!------------------------------------------------------------------
!
      end module copy_num_itp_wtype_org2dst
