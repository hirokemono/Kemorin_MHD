!> @file  field_file_IO_b.f90
!!      module field_file_IO_b
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2007
!
!> @brief BINARY Field data IO
!!
!!@verbatim
!!      subroutine write_step_field_file_b(file_name, my_rank, fld_IO)
!!
!!      subroutine read_and_allocate_field_file_b                       &
!!     &         (file_name, my_rank, fld_IO)
!!
!!      subroutine read_step_field_file_b(file_name, my_rank, fld_IO)
!!      subroutine read_and_allocate_step_field_b                       &
!!     &         (file_name, my_rank, fld_IO)
!!
!!      subroutine read_and_allocate_step_head_b                        &
!!     &         (file_name, my_rank, fld_IO)
!!@endverbatim
!
      module field_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_time_data_IO
      use t_field_data_IO
      use field_data_IO
      use set_parallel_file_name
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_step_field_file_b(file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(in) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
!
      call write_step_data_b(id_phys_file, my_rank)
      call write_field_data_b(id_phys_file,                             &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      close (id_phys_file)
!
      end subroutine write_step_field_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_and_allocate_field_file_b                         &
     &         (file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      read(id_phys_file) fld_IO%nnod_IO, fld_IO%num_field_IO
!
      call alloc_phys_name_IO(fld_IO)
      read(id_phys_file) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_b(id_phys_file,                              &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO)
      close (id_phys_file)
!
      end subroutine read_and_allocate_field_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_step_field_file_b(file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) fld_IO%nnod_IO, fld_IO%num_field_IO
      read(id_phys_file) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      call read_field_data_b(id_phys_file,                              &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO)
      close (id_phys_file)
!
      end subroutine read_step_field_file_b
!
!------------------------------------------------------------------
!
      subroutine read_and_allocate_step_field_b                         &
     &         (file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) fld_IO%nnod_IO, fld_IO%num_field_IO
!
      call alloc_phys_name_IO(fld_IO)
      read(id_phys_file) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_b(id_phys_file,                              &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO)
      close (id_phys_file)
!
      end subroutine read_and_allocate_step_field_b
!
!------------------------------------------------------------------
!
      subroutine read_and_allocate_step_head_b                          &
     &         (file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) fld_IO%nnod_IO, fld_IO%num_field_IO
!
      call alloc_phys_name_IO(fld_IO)
      read(id_phys_file) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      close(id_phys_file)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine read_and_allocate_step_head_b
!
!------------------------------------------------------------------
!
      end module field_file_IO_b
