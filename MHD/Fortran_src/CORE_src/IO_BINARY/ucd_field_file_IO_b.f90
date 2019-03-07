!>@file  ucd_field_file_IO_b.f90
!!       module ucd_field_file_IO_b
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief binary format data IO
!!
!!@verbatim
!!      subroutine write_ucd_2_fld_file_b(my_rank, file_name, t_IO, ucd)
!!
!!      subroutine read_ucd_2_fld_file_b                                &
!!     &         (my_rank, file_name, t_IO, ucd, ierr)
!!      subroutine read_alloc_ucd_2_fld_file_b                          &
!!     &         (my_rank, file_name, t_IO, ucd, ierr)
!!
!!      subroutine read_alloc_ucd_2_fld_header_b                        &
!!     &         (my_rank, file_name, t_IO, ucd, ierr)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param my_rank      process ID
!!@param file_name    file name
!!@param ucd          Structure for FEM field data IO
!
      module ucd_field_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_field_file_format
!
      use t_time_data
      use t_ucd_data
      use set_ucd_file_names
      use field_data_IO_b
      use binary_IO
      use transfer_to_long_integers
!
      implicit none
!
      type(file_IO_flags), private :: bin_ucdflags
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_fld_file_b(my_rank, file_name, t_IO, ucd)
!
      character(len=kchara), intent(in)  :: file_name
      integer(kind=kint), intent(in) :: my_rank
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write binary data file: ', trim(file_name)
!
      call open_write_binary_file(file_name)
!
      call write_step_data_b                                            &
     &   (my_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      call write_field_data_b(ucd%nnod, ucd%num_field,                  &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd)
      call close_binary_file
!
      end subroutine write_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_fld_file_b                                  &
     &         (my_rank, file_name, t_IO, ucd, ierr)
!
      character(len=kchara), intent(in)  :: file_name
      integer(kind=kint), intent(in) :: my_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind=kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file                                        &
     &   (file_name, my_rank, bin_ucdflags%iflag_bin_swap)
      call read_step_data_b(bin_ucdflags%iflag_bin_swap,                &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt,                         &
     &    istack_merged, ucd%num_field, bin_ucdflags%ierr_IO)
      ucd%nnod = istack_merged(1)
      if(bin_ucdflags%ierr_IO .gt. 0) goto 99
!
      call read_mul_integer_b(bin_ucdflags%iflag_bin_swap,              &
     &    cast_long(ucd%num_field), ucd%num_comp, bin_ucdflags%ierr_IO)
      if(bin_ucdflags%ierr_IO .gt. 0) goto 99
      call read_field_data_b(bin_ucdflags%iflag_bin_swap,               &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp,                       &
     &    ucd%phys_name, ucd%d_ucd, bin_ucdflags%ierr_IO)
!
  99  continue
      call close_binary_file
      ierr = bin_ucdflags%iflag_bin_swap
!
      end subroutine read_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_file_b                            &
     &         (my_rank, file_name, t_IO, ucd, ierr)
!
      character(len=kchara), intent(in)  :: file_name
      integer(kind=kint), intent(in) :: my_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind=kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file                                        &
     &   (file_name, my_rank, bin_ucdflags%iflag_bin_swap)
      call read_step_data_b(bin_ucdflags%iflag_bin_swap,                &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt,                         &
     &    istack_merged, ucd%num_field, bin_ucdflags%ierr_IO)
      ucd%nnod = istack_merged(1)
      if(bin_ucdflags%ierr_IO .gt. 0) goto 99
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b(bin_ucdflags%iflag_bin_swap,              &
     &    cast_long(ucd%num_field), ucd%num_comp, bin_ucdflags%ierr_IO)
      if(bin_ucdflags%ierr_IO .gt. 0) goto 99
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_field_data_b(bin_ucdflags%iflag_bin_swap,               &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp,                       &
     &    ucd%phys_name, ucd%d_ucd, bin_ucdflags%ierr_IO)
!
  99  continue
      call close_binary_file
      ierr = bin_ucdflags%iflag_bin_swap
!
      end subroutine read_alloc_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_header_b                          &
     &         (my_rank, file_name, t_IO, ucd, ierr)
!
      character(len=kchara), intent(in)  :: file_name
      integer(kind=kint), intent(in) :: my_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind=kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file                                        &
     &   (file_name, my_rank, bin_ucdflags%iflag_bin_swap)
      call read_step_data_b(bin_ucdflags%iflag_bin_swap,                &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt,                         &
     &    istack_merged, ucd%num_field, bin_ucdflags%ierr_IO)
      ucd%nnod = istack_merged(1)
      if(bin_ucdflags%ierr_IO .gt. 0) goto 99
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b(bin_ucdflags%iflag_bin_swap,              &
     &    cast_long(ucd%num_field), ucd%num_comp, bin_ucdflags%ierr_IO)
!
  99  continue
      call close_binary_file
      ierr = bin_ucdflags%iflag_bin_swap
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      end subroutine read_alloc_ucd_2_fld_header_b
!
!------------------------------------------------------------------
!
      end module ucd_field_file_IO_b
