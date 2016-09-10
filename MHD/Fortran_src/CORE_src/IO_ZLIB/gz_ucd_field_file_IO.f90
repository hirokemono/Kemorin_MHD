!>@file  gz_ucd_field_file_IO.f90
!!       module gz_ucd_field_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief gzipped UCD ascii data IO
!!
!!@verbatim
!!      subroutine write_ucd_2_gz_fld_file(my_rank, istep, ucd)
!!
!!      subroutine read_ucd_2_gz_fld_file(my_rank, istep, ucd)
!!      subroutine read_alloc_ucd_2_gz_fld_file(my_rank, istep, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param my_rank  process ID
!!@param istep    step number for output
!!@param ucd      Structure for FEM field data IO
!
      module gz_ucd_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_time_data_IO
      use m_field_file_format
!
      use t_ucd_data
!
      use gz_field_data_IO
      use skip_gz_comment
      use set_ucd_file_names
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_gz_fld_file(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: gzip_name
      integer(kind = kint) :: nnod4
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_fld_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped step data file: ', trim(gzip_name)
!
      call open_wt_gzfile_f(gzip_name)
!
      nnod4 = int(ucd%nnod)
      call write_gz_step_data(my_rank)
      call write_gz_field_data                                          &
     &         (nnod4, ucd%num_field, ucd%ntot_comp,                    &
     &          ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      call close_gzfile_f
!
      end subroutine write_ucd_2_gz_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_gz_fld_file(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: gzip_name
      integer(kind = kint) :: nnod4, id_rank
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_fld_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_step_data(id_rank)
      call skip_gz_comment_int2(nnod4, ucd%num_field)
      call read_gz_multi_int(ucd%num_field, ucd%num_comp)
!
      ucd%nnod = nnod4
      call read_gz_field_data                                           &
     &         (nnod4, ucd%num_field, ucd%ntot_comp,                    &
     &          ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      call close_gzfile_f
!
      end subroutine read_ucd_2_gz_fld_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_gz_fld_file(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: gzip_name
      integer(kind = kint) :: nnod4, id_rank
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_fld_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile_f(gzip_name)
!
      call read_gz_step_data(id_rank)
      call skip_gz_comment_int2(nnod4, ucd%num_field)
      ucd%nnod = nnod4
!
      call allocate_ucd_phys_name(ucd)
!
      call read_gz_multi_int(ucd%num_field, ucd%num_comp)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_gz_field_data                                           &
     &         (nnod4, ucd%num_field, ucd%ntot_comp,                    &
     &          ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      call close_gzfile_f
!
      end subroutine read_alloc_ucd_2_gz_fld_file
!
!------------------------------------------------------------------
!
      end module gz_ucd_field_file_IO
