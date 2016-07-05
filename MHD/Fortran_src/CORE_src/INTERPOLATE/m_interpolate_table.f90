!m_interpolate_table.f90
!      module m_interpolate_table
!
!> @brief Structure of interpolation table
!
!      Written by H.Matsui on Dec., 2008
!
!!      subroutine deallocate_interpolate_tbl(inp_smp, tp_info)
!!      subroutine allocate_zero_itp_tables(np_smp, itp_info)
!!      subroutine const_interporate_matrix(ele_org, tbl_org, mat)
!        type(interpolate_table), intent(inout) :: itp_info
!
!
      module m_interpolate_table
!
      use m_precision
!
      use m_constants
      use t_comm_table
      use t_geometry_data
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_solver_ordered_crs
!
      implicit none
!
!
      type(interpolate_table), save :: itp1_info
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine deallocate_interpolate_tbl
!
!
      call dealloc_interpolate_tbl_type(itp1_info)
!
      end subroutine deallocate_interpolate_tbl
!
!------------------------------------------------------------------
!
      subroutine allocate_zero_itp_tables(np_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
!
      call alloc_zero_itp_tables(np_smp, itp1_info)
!
      end subroutine allocate_zero_itp_tables
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_interporate_matrix(ele_org)
!
      use m_machine_parameter
      use interpolate_matrix_para
!
      type(element_data), intent(in) :: ele_org
!
!
      call const_interporate_mat_type                                   &
     &  (ele_org, itp1_info%tbl_org, itp1_info%mat)
!
      end subroutine const_interporate_matrix
!
! ----------------------------------------------------------------------
!
      end module m_interpolate_table
