!gz_rst_data_IO_by_fld.f90
!     module gz_rst_data_IO_by_fld
!
!> @file  gz_rst_data_IO_by_fld.f90
!!      module gz_rst_data_IO_by_fld
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief read gzipped restart file
!!
!!@verbatim
!!      subroutine read_gz_rst_file(my_rank, file_name)
!!      subroutine read_gz_rst_comps(my_rank, file_name)
!!@endverbatim
!
      module gz_rst_data_IO_by_fld
!
      use m_precision
      use m_machine_parameter
!
      use m_field_data_IO
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
      subroutine read_gz_rst_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped restart file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int(num_phys_data_IO)
      call read_gz_field_data                                           &
     &         (numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          num_phys_comp_IO, phys_data_name_IO, phys_data_IO)
!
      call close_gzfile
!
      end subroutine read_gz_rst_file
!
!------------------------------------------------------------------
!
      subroutine read_gz_rst_comps(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped restart file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int(num_phys_data_IO)
!
      call allocate_phys_data_name_IO
      call read_gz_rst_field_comps
!
      call close_gzfile
!
      call cal_istack_phys_comp_IO
!
      end subroutine read_gz_rst_comps
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_rst_field_comps
!
      use set_restart_data
!
      integer(kind=kint)  :: i, inod, nchara
!
!
      do i = 1, num_phys_data_IO
        call skip_gz_comment_chara( phys_data_name_IO(i) )
        call set_num_comps_4_rst(phys_data_name_IO(i),                  &
     &      num_phys_comp_IO(i) )
!
        do inod = 1, numgrid_phys_IO
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        end do
      end do
!
      end subroutine read_gz_rst_field_comps
!
!------------------------------------------------------------------
!
      end module gz_rst_data_IO_by_fld
