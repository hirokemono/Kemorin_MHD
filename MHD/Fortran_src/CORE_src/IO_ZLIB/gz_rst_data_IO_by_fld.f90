!> @file  gz_rst_data_IO_by_fld.f90
!!      module gz_rst_data_IO_by_fld
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief read gzipped restart file
!!
!!@verbatim
!!      subroutine read_gz_rst_file(my_rank, file_name, fld_IO)
!!      subroutine read_gz_rst_comps(my_rank, file_name, fld_IO)
!!@endverbatim
!
      module gz_rst_data_IO_by_fld
!
      use m_precision
      use m_machine_parameter
!
      use t_field_data_IO
      use gz_field_data_IO
      use skip_gz_comment
      use set_parallel_file_name
!
!
      implicit none
!
      private :: read_gz_rst_field_comps
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_gz_rst_file(my_rank, file_name, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: gzip_name
      integer(kind = kint) :: id_rank
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped restart file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_step_data(id_rank)
      call skip_gz_comment_int(fld_IO%num_field_IO)
      call read_gz_field_data                                           &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile_f
!
      end subroutine read_gz_rst_file
!
!------------------------------------------------------------------
!
      subroutine read_gz_rst_comps(my_rank, file_name, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: gzip_name
      integer(kind = kint) :: id_rank
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped restart file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_step_data(id_rank)
      call skip_gz_comment_int(fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_gz_rst_field_comps(fld_IO)
!
      call close_gzfile_f
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine read_gz_rst_comps
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_rst_field_comps(fld_IO)
!
      use set_restart_data
!
      integer(kind=kint)  :: i, inod, nchara
      type(field_IO), intent(inout) :: fld_IO
!
!
      do i = 1, fld_IO%num_field_IO
        call skip_gz_comment_chara( fld_IO%fld_name(i) )
        call set_num_comps_4_rst(fld_IO%fld_name(i),                    &
     &      fld_IO%num_comp_IO(i) )
!
        do inod = 1, fld_IO%nnod_IO
          call get_one_line_from_gz_f
        end do
      end do
!
      end subroutine read_gz_rst_field_comps
!
!------------------------------------------------------------------
!
      end module gz_rst_data_IO_by_fld
