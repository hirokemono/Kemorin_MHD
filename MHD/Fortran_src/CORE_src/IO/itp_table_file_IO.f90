!>@file   itp_table_file_IO.f90
!!@brief  module itp_table_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!
!>@brief ASCII Interpolation table file IO
!!
!!@verbatim
!!      subroutine write_itp_table_coef_file_a                          &
!!     &         (file_name, my_rankt, itp_tbl_IO)
!!      subroutine write_itp_table_idx_file_a                           &
!!     &         (file_name, my_rankt, itp_tbl_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine write_dbl_itp_tbl_coef_file_a                        &
!!     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine write_dbl_itp_tbl_idx_file_a                         &
!!     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!
!!      subroutine read_itp_table_coef_file_a                           &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!      subroutine read_itp_table_idx_file_a                            &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine read_dbl_itp_tbl_coef_file_a                         &
!!     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!      subroutine read_dbl_itp_tbl_idx_file_a                          &
!!     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module itp_table_file_IO
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
      integer(kind = kint), parameter :: id_tbl_file = 19
      private :: id_tbl_file
!
      private:: write_each_itp_coef_table_a, write_each_itp_idx_table_a
      private:: read_each_itp_coef_table_a,  read_each_itp_idx_table_a
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_table_coef_file_a                            &
     &         (file_name, id_rank, itp_tbl_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_each_itp_coef_table_a(id_rank, itp_tbl_IO)
      close(id_tbl_file)
!
      end subroutine write_itp_table_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_table_idx_file_a                             &
     &         (file_name, id_rank, itp_tbl_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_each_itp_idx_table_a(id_rank, itp_tbl_IO)
      close(id_tbl_file)
!
      end subroutine write_itp_table_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_dbl_itp_tbl_coef_file_a                          &
     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_each_itp_coef_table_a(id_rank, itp_tbl1_IO)
      call write_each_itp_coef_table_a(id_rank, itp_tbl2_IO)
      close(id_tbl_file)
!
      end subroutine write_dbl_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine write_dbl_itp_tbl_idx_file_a                           &
     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_each_itp_idx_table_a(id_rank, itp_tbl1_IO)
      call write_each_itp_idx_table_a(id_rank, itp_tbl2_IO)
      close(id_tbl_file)
!
      end subroutine write_dbl_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_coef_file_a                             &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
!
      call read_each_itp_coef_table_a(id_rank, itp_tbl_IO, ierr)
!
      close(id_tbl_file)
!
      end subroutine read_itp_table_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_idx_file_a                              &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
!
      call read_each_itp_idx_table_a(id_rank, itp_tbl_IO, ierr)
!
      close(id_tbl_file)
!
      end subroutine read_itp_table_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_dbl_itp_tbl_coef_file_a                           &
     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
!
      call read_each_itp_coef_table_a(id_rank, itp_tbl1_IO, ierr)
      call read_each_itp_coef_table_a(id_rank, itp_tbl2_IO, ierr)
!
      close(id_tbl_file)
!
      end subroutine read_dbl_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_dbl_itp_tbl_idx_file_a                            &
     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read ASCII interpolation table file: ', trim(file_name)
      open (id_tbl_file, file = file_name, form = 'formatted')
!
      call read_each_itp_idx_table_a(id_rank, itp_tbl1_IO, ierr)
      call read_each_itp_idx_table_a(id_rank, itp_tbl2_IO, ierr)
!
      close(id_tbl_file)
!
      end subroutine read_dbl_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_each_itp_coef_table_a(id_rank, itp_tbl_IO)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer, intent(in) :: id_rank
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
      subroutine write_each_itp_idx_table_a(id_rank, itp_tbl_IO)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer, intent(in) :: id_rank
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
      subroutine read_each_itp_coef_table_a(id_rank, itp_tbl_IO, ierr)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_dest)
!        write(*,*) 'read_interpolate_table_dest'
      call read_interpolate_table_dest                                  &
     &    (id_tbl_file, itp_tbl_IO%tbl_dest)
!
!        write(*,*) 'read_interpolate_domain_org'
      call read_interpolate_domain_org                                  &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_org)
!        write(*,*) 'read_interpolate_table_org'
      call read_interpolate_table_org(id_tbl_file, itp_tbl_IO%tbl_org)
!        write(*,*) 'read_interpolate_coefs_org'
      call read_interpolate_coefs_org(id_tbl_file, itp_tbl_IO%tbl_org)
!
      if(n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine read_each_itp_coef_table_a
!
!-----------------------------------------------------------------------
!
      subroutine read_each_itp_idx_table_a(id_rank, itp_tbl_IO, ierr)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
!
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_dest)
!        write(*,*) 'read_interpolate_table_dest'
      call read_interpolate_table_dest                                  &
     &    (id_tbl_file, itp_tbl_IO%tbl_dest)
!
!        write(*,*) 'read_interpolate_domain_org'
      call read_interpolate_domain_org                                  &
     &   (id_tbl_file, n_rank_file, itp_tbl_IO%tbl_org)
!        write(*,*) 'read_interpolate_table_org'
      call read_interpolate_table_org(id_tbl_file, itp_tbl_IO%tbl_org)
!        write(*,*) 'read_interpolate_idx_org'
      call read_interpolate_idx_org(id_tbl_file, itp_tbl_IO%tbl_org)
!
      if(n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine read_each_itp_idx_table_a
!
!-----------------------------------------------------------------------
!
      end module itp_table_file_IO
