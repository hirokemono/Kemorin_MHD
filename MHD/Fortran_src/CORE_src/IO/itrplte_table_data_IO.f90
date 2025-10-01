!>@file   itrplte_table_data_IO.f90
!!@brief  module itrplte_table_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!
!>@brief ASCII Interpolation table file IO
!!
!!@verbatim
!!      subroutine write_each_itp_coef_table_a                          &
!!     &        (id_tbl_file, id_rank, itp_tbl_IO)
!!      subroutine write_each_itp_idx_table_a                           &
!!     &        (id_tbl_file, id_rank, itp_tbl_IO)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: id_tbl_file
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!
!!      subroutine read_each_itp_coef_table_a                           &
!!     &         (id_tbl_file, id_rank, itp_tbl_IO, ierr)
!!      subroutine read_each_itp_idx_table_a                            &
!!     &         (id_tbl_file, id_rank, itp_tbl_IO, ierr)
!!        integer(kind = kint), intent(in) :: id_tbl_file
!!        integer, intent(in) :: id_rank
!!@endverbatim
!
      module itrplte_table_data_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_each_itp_coef_table_a                            &
     &        (id_tbl_file, id_rank, itp_tbl_IO)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_tbl_file
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      call write_interpolate_table_dest                                 &
     &   (id_tbl_file, id_rank, itp_tbl_IO%tbl_dest)
!
      call write_interpolate_table_org                                  &
     &   (id_tbl_file, id_rank, itp_tbl_IO%tbl_org)
      call write_interpolate_coefs_org(id_tbl_file, itp_tbl_IO%tbl_org)
!
      end subroutine write_each_itp_coef_table_a
!
!-----------------------------------------------------------------------
!
      subroutine write_each_itp_idx_table_a                             &
     &        (id_tbl_file, id_rank, itp_tbl_IO)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_tbl_file
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      call write_interpolate_table_dest                                 &
     &   (id_tbl_file, id_rank, itp_tbl_IO%tbl_dest)
!
      call write_interpolate_table_org                                  &
     &   (id_tbl_file, id_rank, itp_tbl_IO%tbl_org)
      call write_interpolate_coefs_org(id_tbl_file, itp_tbl_IO%tbl_org)
!
      end subroutine write_each_itp_idx_table_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_each_itp_coef_table_a                             &
     &         (id_tbl_file, id_rank, itp_tbl_IO, ierr)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer(kind = kint), intent(in) :: id_tbl_file
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_dest, ierr)
      if(ierr .gt. 0) return
!        write(*,*) 'read_interpolate_table_dest'
      call read_interpolate_table_dest                                  &
     &    (id_tbl_file, itp_tbl_IO%tbl_dest, ierr)
      if(ierr .gt. 0) return
!
!        write(*,*) 'read_interpolate_domain_org'
      call read_interpolate_domain_org                                  &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_org, ierr)
      if(ierr .gt. 0) return
!        write(*,*) 'read_interpolate_table_org'
      call read_interpolate_table_org(id_tbl_file,                      &
     &                                itp_tbl_IO%tbl_org, ierr)
      if(ierr .gt. 0) return
!        write(*,*) 'read_interpolate_coefs_org'
      call read_interpolate_coefs_org(id_tbl_file,                      &
     &                                itp_tbl_IO%tbl_org, ierr)
      if(ierr .gt. 0) return
!
      if(n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine read_each_itp_coef_table_a
!
!-----------------------------------------------------------------------
!
      subroutine read_each_itp_idx_table_a                              &
     &         (id_tbl_file, id_rank, itp_tbl_IO, ierr)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_tbl_file
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_dest, ierr)
      if(ierr .gt. 0) return
!        write(*,*) 'read_interpolate_table_dest'
      call read_interpolate_table_dest                                  &
     &    (id_tbl_file, itp_tbl_IO%tbl_dest, ierr)
      if(ierr .gt. 0) return
!
!        write(*,*) 'read_interpolate_domain_org'
      call read_interpolate_domain_org                                  &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_org, ierr)
      if(ierr .gt. 0) return
!        write(*,*) 'read_interpolate_table_org'
      call read_interpolate_table_org(id_tbl_file,                      &
     &                                itp_tbl_IO%tbl_org, ierr)
      if(ierr .gt. 0) return
!        write(*,*) 'read_interpolate_idx_org'
      call read_interpolate_idx_org(id_tbl_file,                        &
     &                              itp_tbl_IO%tbl_org, ierr)
      if(ierr .gt. 0) return
!
      if(n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine read_each_itp_idx_table_a
!
!-----------------------------------------------------------------------
!
      end module itrplte_table_data_IO
