!>@file   copy_interpolate_type_raw.f90
!!@brief  module copy_interpolate_type_raw
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!>@brief  Copy interpolation table to structures
!!
!!@verbatim
!!      subroutine copy_interpolate_types_from_raw(my_rank, itp_table)
!!@endverbatim
!
      module copy_interpolate_type_raw
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_interpolate_types_from_raw(my_rank, itp_table)
!
      use m_machine_parameter
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use t_interpolate_table
      use t_interpolate_tbl_org
      use copy_interpolate_types
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table), intent(inout) :: itp_table

!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_type_dst_from_raw'
      call copy_itp_tbl_types_dst                                       &
     &   (my_rank, itp1_dest, itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_types_org'
      call copy_itp_tbl_types_org(my_rank, itp1_org, itp_table%tbl_org)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_stack_tbl_org_smp_type'
      call set_stack_tbl_wtype_org_smp(itp_table%tbl_org)
!
      end subroutine copy_interpolate_types_from_raw
!
!-----------------------------------------------------------------------
!
      end module copy_interpolate_type_raw
