!>@file  field_file_IO_b.f90
!!       module field_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_field_file_b                              &
!!     &         (file_name, my_rank, t_IO, fld_IO)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine read_step_field_file_b                               &
!!     &         (file_name, my_rank, t_IO, fld_IO, ierr)
!!      subroutine read_and_allocate_step_field_b                       &
!!     &         (file_name, my_rank, t_IO, fld_IO, ierr)
!!      subroutine read_and_allocate_step_head_b                        &
!!     &         (file_name, my_rank, t_IO, fld_IO, ierr)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module field_file_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_field_data_IO
      use field_data_IO_b
      use binary_IO
!
      implicit none
!
      type(file_IO_flags), private :: bin_fldflags
!
      private :: read_and_allocate_step_b
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_step_field_file_b                                &
     &         (file_name, my_rank, t_IO, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary data file: ', trim(file_name)
!
      call open_write_binary_file(file_name)
!
      call write_step_data_b                                            &
     &   (my_rank, t_IO%i_time_step, t_IO%time, t_IO%dt)
      call write_field_data_b                                           &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      call close_binary_file
!
      end subroutine write_step_field_file_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_b                                 &
     &         (file_name, my_rank, t_IO, fld_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file                                        &
     &   (file_name, my_rank, bin_fldflags%iflag_bin_swap)
      call read_step_data_b(bin_fldflags%iflag_bin_swap,                &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt,                         &
     &    istack_merged, fld_IO%num_field_IO, bin_fldflags%ierr_IO)
      if(bin_fldflags%ierr_IO .gt. 0) goto 99
!
      num64 = fld_IO%num_field_IO
      call read_mul_integer_b(bin_fldflags%iflag_bin_swap,              &
     &    num64, fld_IO%num_comp_IO,                                    &
    &   bin_fldflags%ierr_IO)
      if(bin_fldflags%ierr_IO .gt. 0) goto 99
!
      call read_field_data_b(bin_fldflags%iflag_bin_swap,               &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO, bin_fldflags%ierr_IO)
!
  99  continue
      call close_binary_file
      ierr = bin_fldflags%ierr_IO
!
      end subroutine read_step_field_file_b
!
! -----------------------------------------------------------------------
!
      subroutine read_and_allocate_step_field_b                         &
     &         (file_name, my_rank, t_IO, fld_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file                                        &
     &   (file_name, my_rank, bin_fldflags%iflag_bin_swap)
      call read_and_allocate_step_b(bin_fldflags, t_IO, fld_IO)
      if(bin_fldflags%ierr_IO .gt. 0) goto 99
!
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_b(bin_fldflags%iflag_bin_swap,               &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO, bin_fldflags%ierr_IO)
!
  99  continue
      call close_binary_file
      ierr = bin_fldflags%ierr_IO
!
      end subroutine read_and_allocate_step_field_b
!
! -----------------------------------------------------------------------
!
      subroutine read_and_allocate_step_head_b                          &
     &         (file_name, my_rank, t_IO, fld_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file                                        &
     &   (file_name, my_rank, bin_fldflags%iflag_bin_swap)
      call read_and_allocate_step_b(bin_fldflags, t_IO, fld_IO)
      call close_binary_file
      ierr = bin_fldflags%ierr_IO
!
      end subroutine read_and_allocate_step_head_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_and_allocate_step_b(bin_flags, t_IO, fld_IO)
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      call read_step_data_b(bin_flags%iflag_bin_swap,                   &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt,                         &
     &    istack_merged, fld_IO%num_field_IO, bin_flags%ierr_IO)
      fld_IO%nnod_IO = int(istack_merged(1))
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_phys_name_IO(fld_IO)
!
      num64 = fld_IO%num_field_IO
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    num64, fld_IO%num_comp_IO, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine read_and_allocate_step_b
!
! -----------------------------------------------------------------------
!
      end module field_file_IO_b
