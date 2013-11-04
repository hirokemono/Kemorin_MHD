!t_interpolate_matrix.f90
!     module t_interpolate_matrix
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine deallocate_itp_mat_type(itp)
!!      subroutine const_interporate_mat_type(org_ele, tbl_org, itp)
!!        type(element_data), intent(in) :: org_ele
!!        type(interpolate_table_org), intent(in) :: tbl_org
!!        type(DJORS_ITP_MATRIX), intent(inout) :: itp
!
      module t_interpolate_matrix
!
      use m_precision
!
      use t_solver_ordered_crs
!
      implicit none
!
      type DJORS_ITP_MATRIX
        type(DJORS_CONNECT) ::       itp_tbl
        type(DJORS_MATRIX) ::        itp_mat
      end type DJORS_ITP_MATRIX
!
      private :: allocate_itp_mat_num, allocate_itp_mat_item
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_mat_num(itp)
!
      use m_machine_parameter
!
      type(DJORS_ITP_MATRIX), intent(inout) :: itp
!
!
      call alloc_type_djo_num(np_smp, itp%itp_tbl)
!
      end subroutine allocate_itp_mat_num
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_itp_mat_item(itp)
!
      type(DJORS_ITP_MATRIX), intent(inout) :: itp
!
!
      call alloc_type_djo_table(itp%itp_tbl)
      call alloc_type_djo11_mat(itp%itp_tbl, itp%itp_mat)
!
      end subroutine allocate_itp_mat_item
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_mat_type(itp)
!
      type(DJORS_ITP_MATRIX), intent(inout) :: itp
!
!
      call dealloc_type_djo_mat(itp%itp_mat)
      call dealloc_type_connect_4_djo(itp%itp_tbl)
!
      end subroutine deallocate_itp_mat_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_interporate_mat_type(org_ele, tbl_org, itp)
!
      use m_machine_parameter
      use t_geometry_data
      use t_interpolate_tbl_org
      use interpolate_matrix_para
!
      type(element_data), intent(in) :: org_ele
      type(interpolate_table_org), intent(in) :: tbl_org
!
      type(DJORS_ITP_MATRIX), intent(inout) :: itp
!
!
      itp%itp_tbl%NC =          tbl_org%ntot_table_org
      itp%itp_tbl%NUM_NCOMP = 4*tbl_org%num_dest_domain
!
      call allocate_itp_mat_num(itp)
!
      call count_interporate_mat_para                                   &
     &   (np_smp, org_ele%nnod_4_ele, tbl_org%istack_tbl_type_org_smp,  &
     &    itp%itp_tbl%NC, itp%itp_tbl%NUM_NCOMP, itp%itp_tbl%NCM,       &
     &    itp%itp_tbl%INOD_DJO, itp%itp_tbl%INM,                        &
     &    itp%itp_tbl%NUM_SUM, itp%itp_tbl%IEND_SUM,                    &
     &    itp%itp_tbl%IEND_SUM_smp)
!
      call allocate_itp_mat_item(itp)
!
      call set_interporate_mat_para(np_smp,                             &
     &    org_ele%numele, org_ele%nnod_4_ele, org_ele%ie,               &
     &    tbl_org%iele_org_4_org, tbl_org%itype_inter_org,              &
     &    tbl_org%coef_inter_org, itp%itp_tbl%NC, itp%itp_tbl%NCM,      &
     &    itp%itp_tbl%INM, itp%itp_tbl%IAM, itp%itp_mat%AM,             &
     &    itp%itp_tbl%IEND_SUM_smp)
!
      end subroutine const_interporate_mat_type
!
! ----------------------------------------------------------------------
!
      end module t_interpolate_matrix
