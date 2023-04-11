!>@file  gz_itrplte_table_data_IO.f90
!!       module gz_itrplte_table_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped file IO for interpolation
!!
!!@verbatim
!!      subroutine gz_write_each_itp_coef_table_a                       &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!!      subroutine gz_write_each_itp_idx_table_a                        &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!!        character, pointer, intent(inout) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf_itp
!!      subroutine gz_read_each_itp_coef_table_a                        &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp, ierr)
!!      subroutine gz_read_each_itp_idx_table_a                         &
!!     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp, ierr)
!!        character, pointer, intent(inout) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf_itp
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module gz_itrplte_table_data_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use gz_binary_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_each_itp_coef_table_a                         &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_org_data_IO
      use gz_itp_table_dsst_data_IO
!
      character, pointer, intent(inout) :: FPz_f
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
!
      call write_gz_itp_table_dest                                      &
     &   (FPz_f, id_rank, itp_tbl_IO%tbl_dest, zbuf_itp)
!
      call write_gz_itp_table_org                                       &
     &   (FPz_f, id_rank, itp_tbl_IO%tbl_org, zbuf_itp)
      call write_gz_itp_coefs_org(FPz_f, itp_tbl_IO%tbl_org, zbuf_itp)
!
      end subroutine gz_write_each_itp_coef_table_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_each_itp_idx_table_a                          &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_org_data_IO
      use gz_itp_table_dsst_data_IO
!
      character, pointer, intent(inout) :: FPz_f
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
!
      call write_gz_itp_table_dest                                      &
     &   (FPz_f, id_rank, itp_tbl_IO%tbl_dest, zbuf_itp)
!
      call write_gz_itp_table_org                                       &
     &   (FPz_f, id_rank, itp_tbl_IO%tbl_org, zbuf_itp)
      call write_gz_itp_coefs_org(FPz_f, itp_tbl_IO%tbl_org, zbuf_itp)
!
      end subroutine gz_write_each_itp_idx_table_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_read_each_itp_coef_table_a                          &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp, ierr)
!
      use gz_itp_table_org_data_IO
      use gz_itp_table_dsst_data_IO
!
      character, pointer, intent(inout) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
!        write(*,*) 'read_gz_itp_domain_dest'
      call read_gz_itp_domain_dest                                      &
     &   (FPz_f, n_rank_file, itp_tbl_IO%tbl_dest, zbuf_itp)
!        write(*,*) 'read_gz_itp_table_dest'
      call read_gz_itp_table_dest(FPz_f, itp_tbl_IO%tbl_dest, zbuf_itp)
!
!        write(*,*) 'read_gz_itp_domain_org'
      call read_gz_itp_domain_org                                       &
     &   (FPz_f, n_rank_file, itp_tbl_IO%tbl_org, zbuf_itp)
!        write(*,*) 'read_gz_itp_coefs_org'
      call read_gz_itp_table_org(FPz_f, itp_tbl_IO%tbl_org, zbuf_itp)
      call read_gz_itp_coefs_org(FPz_f, itp_tbl_IO%tbl_org, zbuf_itp)
!
      if(n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine gz_read_each_itp_coef_table_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_each_itp_idx_table_a                           &
     &         (FPz_f, id_rank, itp_tbl_IO, zbuf_itp, ierr)
!
      use gz_itp_table_org_data_IO
      use gz_itp_table_dsst_data_IO
!
      character, pointer, intent(inout) :: FPz_f
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
!        write(*,*) 'read_gz_itp_domain_dest'
      call read_gz_itp_domain_dest                                      &
     &   (FPz_f, n_rank_file, itp_tbl_IO%tbl_dest, zbuf_itp)
!        write(*,*) 'read_gz_itp_table_dest'
      call read_gz_itp_table_dest(FPz_f, itp_tbl_IO%tbl_dest, zbuf_itp)
!
!        write(*,*) 'read_gz_itp_domain_org'
      call read_gz_itp_domain_org                                       &
     &   (FPz_f, n_rank_file, itp_tbl_IO%tbl_org, zbuf_itp)
!        write(*,*) 'read_gz_itp_coefs_org'
      call read_gz_itp_table_org(FPz_f, itp_tbl_IO%tbl_org, zbuf_itp)
      call read_gz_itp_coefs_org(FPz_f, itp_tbl_IO%tbl_org, zbuf_itp)
!
      if(n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine gz_read_each_itp_idx_table_a
!
!-----------------------------------------------------------------------
!
      end module gz_itrplte_table_data_IO
