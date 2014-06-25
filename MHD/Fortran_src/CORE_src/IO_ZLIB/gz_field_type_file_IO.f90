!> @file  gz_field_type_file_IO.f90
!!      module gz_field_type_file_IO
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2007
!
!> @brief Field data IO using zlib
!!
!!@verbatim
!!      subroutine write_step_fld_type_file_gz                          &
!!     &         (gzip_name, my_rank, fld_IO)
!!
!!      subroutine read_step_fld_type_file_gz(gzip_name, my_rank, fld_IO)
!!      subroutine read_and_alloc_step_fld_t_gz                         &
!!     &          (gzip_name, my_rank, fld_IO)
!!
!!      subroutine read_alloc_step_fld_t_head_gz                        &
!!     &         (gzip_name, my_rank, fld_IO)
!!@endverbatim
!
      module gz_field_type_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_field_data_IO
      use gz_field_data_IO
      use skip_gz_comment
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_step_fld_type_file_gz                            &
     &         (gzip_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
      type(field_IO), intent(in) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write gzipped field file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
!
      call write_gz_step_data(my_rank)
      call write_gz_field_data                                          &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile
!
      end subroutine write_step_fld_type_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_step_fld_type_file_gz(gzip_name, my_rank, fld_IO)
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(fld_IO%nnod_IO, fld_IO%num_field_IO)
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call read_gz_field_data                                           &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile
!
      end subroutine read_step_fld_type_file_gz
!
!------------------------------------------------------------------
!
      subroutine read_and_alloc_step_fld_t_gz                           &
     &          (gzip_name, my_rank, fld_IO)
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(fld_IO%nnod_IO, fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_gz_field_data                                           &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_gzfile
!
      end subroutine read_and_alloc_step_fld_t_gz
!
!------------------------------------------------------------------
!
      subroutine read_alloc_step_fld_t_head_gz                          &
     &         (gzip_name, my_rank, fld_IO)
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(fld_IO%nnod_IO, fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call close_gzfile
!
      call cal_istack_comp_IO(fld_IO)
!
      end subroutine read_alloc_step_fld_t_head_gz
!
!------------------------------------------------------------------
!
      end module gz_field_type_file_IO
