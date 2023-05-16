!>@file   itrplte_table_data_IO_b.f90
!!@brief  module itrplte_table_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary interpolation file IO
!!
!!@verbatim
!!      subroutine write_each_itp_coef_table_b                          &
!!     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!!      subroutine write_each_itp_idx_table_b                           &
!!     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!!      subroutine read_each_itp_coef_table_b                           &
!!     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!!      subroutine read_each_itp_idx_table_b                            &
!!     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!!@endverbatim
!
      module itrplte_table_data_IO_b
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_binary_IO_buffer
!
      use binary_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_each_itp_coef_table_b                            &
     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!
      use itp_table_org_data_IO_b
      use itp_table_dest_data_IO_b
!
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
      type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!
!
      call write_each_itp_idx_table_b(id_rank, itp_tbl_IO, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) return
!
      call write_interpolate_coefs_org_b(itp_tbl_IO%tbl_org, bbuf_tbl)
!
      end subroutine write_each_itp_coef_table_b
!
!-----------------------------------------------------------------------
!
      subroutine write_each_itp_idx_table_b                             &
     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!
      use itp_table_org_data_IO_b
      use itp_table_dest_data_IO_b
!
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
      type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!
!
      call write_interpolate_table_dest_b                               &
     &   (id_rank, itp_tbl_IO%tbl_dest, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) return
!
      call write_interpolate_table_org_b                                &
     &   (id_rank, itp_tbl_IO%tbl_org, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) return
      call write_interpolate_idx_org_b(itp_tbl_IO%tbl_org, bbuf_tbl)
!
      end subroutine write_each_itp_idx_table_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_each_itp_coef_table_b                             &
     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!
      use itp_table_org_data_IO_b
      use itp_table_dest_data_IO_b
!
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!
!
      call read_each_itp_idx_table_b(id_rank, itp_tbl_IO, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) return
!
      call read_interpolate_coefs_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
!
      end subroutine read_each_itp_coef_table_b
!
!-----------------------------------------------------------------------
!
      subroutine read_each_itp_idx_table_b                              &
     &         (id_rank, itp_tbl_IO, bbuf_tbl)
!
      use itp_table_org_data_IO_b
      use itp_table_dest_data_IO_b
!
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_interpolate_domain_dest_b                               &
     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_dest)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
      call read_interpolate_table_dest_b(bbuf_tbl, itp_tbl_IO%tbl_dest)
      if(bbuf_tbl%ierr_bin .ne. 0) go to 99
!
      call read_interpolate_domain_org_b                                &
     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_org)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
      call read_interpolate_table_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
      if(bbuf_tbl%ierr_bin .ne. 0) go to 99
!
      call read_interpolate_idx_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
  99  continue
      if(n_rank_file .ne. id_rank) bbuf_tbl%ierr_bin = n_rank_file
!
      end subroutine read_each_itp_idx_table_b
!
!-----------------------------------------------------------------------
!
      end module itrplte_table_data_IO_b
