!t_interpolate_table.f90
!      module t_interpolate_table
!
!> @brief Structure of interpolation table
!
!      Written by H.Matsui on Dec., 2008
!
!!      subroutine dealloc_interpolate_tbl_type(itp_info)
!!      subroutine alloc_zero_itp_tables(inp_smp, tp_info)
!!      subroutine const_interporate_mat_type(ele_org, tbl_org, mat)
!!        type(interpolate_table), intent(inout) :: itp_info
!!        type(element_data), intent(in) :: ele_org
!!        type(interpolate_table_org), intent(in) :: tbl_org
!!        type(CRS_SMP_CONNECT_MATRIX), intent(inout) :: mat
!
!
      module t_interpolate_table
!
      use m_precision
!
      use m_constants
      use t_comm_table
      use t_geometry_data
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_solver_ordered_crs
!
      implicit none
!
!
!> Structure of interpolation table
      type interpolate_table
!> Structure of interpolation table for source grid
        type(interpolate_table_org) ::  tbl_org
!> Structure of interpolation table for target grid
        type(interpolate_table_dest) :: tbl_dest
!> Structure of interpolation matrix
        type(CRS_SMP_CONNECT_MATRIX) :: mat
!> Structure of commnucation table for target grid
        type(communication_table) ::    comm_dest
      end type interpolate_table
!
!> Structure of interpolation table for both way
      type MG_itp_table
        type(interpolate_table) :: f2c
!< Structure of interpolation table for restoriction
        type(interpolate_table) :: c2f
!< Structure of interpolation table for prolongation
      end type MG_itp_table
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_interpolate_tbl_type(itp_info)
!
      type(interpolate_table), intent(inout) :: itp_info
!
!
      call dealloc_type_itp_table_dest(itp_info%tbl_dest)
      call dealloc_type_itp_num_dest(itp_info%tbl_dest)
!
      call dealloc_type_itp_table_org(itp_info%tbl_org)
      call deallocate_itp_num_org(itp_info%tbl_org)
!
      call dealloc_crs_smp_mat(itp_info%mat)
!
      end subroutine dealloc_interpolate_tbl_type
!
!------------------------------------------------------------------
!
      subroutine alloc_zero_itp_tables(np_smp, itp_info)
!
      integer(kind = kint), intent(in) :: np_smp
      type(interpolate_table), intent(inout) :: itp_info
!
!
      call alloc_type_zero_itp_tbl_org(np_smp, itp_info%tbl_org)
      call alloc_type_zero_itp_tbl_dest(itp_info%tbl_dest)
!
      call alloc_zero_crs_smp_mat(np_smp, itp_info%mat)
!
      end subroutine alloc_zero_itp_tables
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_interporate_mat_type(ele_org, tbl_org, mat)
!
      use m_machine_parameter
      use interpolate_matrix_para
!
      type(element_data), intent(in) :: ele_org
      type(interpolate_table_org), intent(in) :: tbl_org
!
      type(CRS_SMP_CONNECT_MATRIX), intent(inout) :: mat
!
!
      mat%NC =        tbl_org%ntot_table_org
      mat%NUM_NCOMP = 4
!
      call alloc_crs_smp_num(np_smp, mat)
!
      call count_interporate_mat_para                                   &
     &   (np_smp, ele_org%nnod_4_ele, tbl_org%istack_tbl_type_org_smp,  &
     &    mat%NC, mat%NUM_NCOMP, mat%NCM, mat%INOD_DJO,  mat%INM,       &
     &    mat%NUM_SUM, mat%IEND_SUM, mat%IEND_SUM_smp)
!
      call alloc_crs_smp_mat(mat)
!
      call set_interporate_mat_para(np_smp,                             &
     &    ele_org%numele, ele_org%nnod_4_ele, ele_org%ie,               &
     &    tbl_org%iele_org_4_org, tbl_org%itype_inter_org,              &
     &    tbl_org%coef_inter_org, mat%NC, mat%NCM,                      &
     &    mat%INM, mat%IAM, mat%AM, mat%IEND_SUM_smp)
!
      end subroutine const_interporate_mat_type
!
! ----------------------------------------------------------------------
!
      end module t_interpolate_table
