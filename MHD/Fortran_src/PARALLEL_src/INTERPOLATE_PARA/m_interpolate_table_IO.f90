!>@file   m_interpolate_table_IO
!!@brief  module m_interpolate_table_IO
!!
!!@author H. Matsui
!!@date Programmed on Dec., 2008
!
!>@brief  Load and output interpolation table
!!
!!@verbatim
!!      subroutine load_interpolate_table                               &
!!     &         (id_rank, table_file_IO, itp_table)
!!      subroutine load_zero_interpolate_table(itp_table)
!!      subroutine output_interpolate_table                             &
!!     &         (id_rank, table_file_IO, itp_table)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_table
!!@endverbatim
!
      module m_interpolate_table_IO
!
      use m_precision
      use m_machine_parameter
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_file_IO_parameter
!
      implicit none
!
!> Structure of interpolation table for source grid
      type(interpolate_table), save :: itp_tbl_IO
!
!> Structure of interpolation coefficients for target grid
      type(interpolate_coefs_dest), save :: IO_itp_c_dest
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine load_interpolate_table                                 &
     &         (id_rank, table_file_IO, itp_table)
!
      use copy_interpolate_types
      use itp_table_IO_select_4_zlib
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table), intent(inout) :: itp_table
!
      integer(kind = kint) :: ierr
!
!
      call sel_read_interpolate_table                                   &
     &   (id_rank, table_file_IO, itp_tbl_IO, ierr)
!
      call copy_itp_tbl_types_dst                                       &
     &   (id_rank, itp_tbl_IO%tbl_dest, itp_table%tbl_dest)
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_types_org'
      call copy_itp_tbl_types_org                                       &
     &   (id_rank, itp_tbl_IO%tbl_org, itp_table%tbl_org)
      call dealloc_itp_table_org(itp_tbl_IO%tbl_org)
      call dealloc_itp_num_org(itp_tbl_IO%tbl_org)
!
      call set_stack_tbl_wtype_org_smp(itp_table%tbl_org)
!
      end subroutine load_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine load_zero_interpolate_table(itp_table)
!
      type(interpolate_table), intent(inout) :: itp_table
!
!
      call alloc_zero_itp_tables(np_smp, itp_table)
      call set_stack_tbl_wtype_org_smp(itp_table%tbl_org)
!
      end subroutine load_zero_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine output_interpolate_table                               &
     &         (id_rank, table_file_IO, itp_table)
!
      use copy_interpolate_types
      use itp_table_IO_select_4_zlib
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table), intent(inout) :: itp_table
!
!
      call copy_itp_tbl_types_dst                                       &
     &   (id_rank, itp_table%tbl_dest, itp_tbl_IO%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_types_org'
      call copy_itp_tbl_types_org                                       &
     &   (id_rank, itp_table%tbl_org, itp_tbl_IO%tbl_org)
      call dealloc_itp_table_org(itp_table%tbl_org)
      call dealloc_itp_num_org(itp_table%tbl_org)
!
      call sel_write_interpolate_table                                  &
     &   (id_rank, table_file_IO, itp_tbl_IO)
!
      end subroutine output_interpolate_table
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_IO
