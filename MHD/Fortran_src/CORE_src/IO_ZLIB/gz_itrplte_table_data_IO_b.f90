!>@file  gz_itrplte_table_data_IO_b.f90
!!       module gz_itrplte_table_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped binary file IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_each_itp_coef_table_b                       &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!!      subroutine write_gz_each_itp_idx_table_b                        &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf_itp
!!      subroutine read_gz_each_itp_coef_table_b                        &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!!      subroutine read_gz_each_itp_idx_table_b                         &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf_itp
!!@endverbatim
!
      module gz_itrplte_table_data_IO_b
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
      use t_buffer_4_gzip
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_each_itp_coef_table_b                         &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_org_data_IO_b
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
!
      call write_gz_each_itp_idx_table_b                                &
     &   (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) return
!
      call write_gz_itp_coefs_org_b(FPz_f, itp_tbl_IO%tbl_org,          &
     &                              zbuf_itp)
!
      end subroutine write_gz_each_itp_coef_table_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_each_itp_idx_table_b                          &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_org_data_IO_b
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
!
      call write_gz_itp_table_dest_b                                    &
     &   (FPz_f, id_rank, itp_tbl_IO%tbl_dest, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) return
!
      call write_gz_itp_table_org_b                                     &
     &    (FPz_f, id_rank, itp_tbl_IO%tbl_org, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) return
      call write_gz_itp_idx_org_b(FPz_f, itp_tbl_IO%tbl_org,            &
     &                              zbuf_itp)
!
      end subroutine write_gz_each_itp_idx_table_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_each_itp_coef_table_b                          &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_org_data_IO_b
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
!
      call read_gz_each_itp_idx_table_b                                 &
     &   (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
      if(zbuf_itp%ierr_zlib .ne. 0) return
!
      call read_gz_itp_coefs_org_b(FPz_f, zbuf_itp, itp_tbl_IO%tbl_org)
!
      end subroutine read_gz_each_itp_coef_table_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_each_itp_idx_table_b                           &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_org_data_IO_b
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_gz_itp_domain_dest_b                                    &
     &   (FPz_f, zbuf_itp, n_rank_file, itp_tbl_IO%tbl_dest)
      if(zbuf_itp%ierr_zlib .gt. 0) return
!
      call read_gz_itp_table_dest_b                                     &
     &   (FPz_f, zbuf_itp, itp_tbl_IO%tbl_dest)
      if(zbuf_itp%ierr_zlib .ne. 0) return
!
      call read_gz_itp_domain_org_b                                     &
     &    (FPz_f, zbuf_itp, n_rank_file, itp_tbl_IO%tbl_org)
      if(zbuf_itp%ierr_zlib .gt. 0) return
!
      call read_gz_itp_table_org_b(FPz_f, zbuf_itp, itp_tbl_IO%tbl_org)
      if(zbuf_itp%ierr_zlib .ne. 0) return
!
      call read_gz_itp_idx_org_b(FPz_f, zbuf_itp, itp_tbl_IO%tbl_org)
      if(zbuf_itp%ierr_zlib .ne. 0) return
      if (n_rank_file .ne. id_rank) zbuf_itp%ierr_zlib = n_rank_file
!
      end subroutine read_gz_each_itp_idx_table_b
!
!-----------------------------------------------------------------------
!
      end module gz_itrplte_table_data_IO_b
