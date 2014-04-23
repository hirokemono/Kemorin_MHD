!field_file_IO_b.f90
!      module field_file_IO_b
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine write_step_field_file_b(file_name, my_rank)
!      subroutine read_and_allocate_field_file_b(file_name, my_rank)
!
!      subroutine read_step_field_file_b(file_name, my_rank)
!      subroutine read_and_allocate_step_field_b(file_name, my_rank)
!
!      subroutine read_and_allocate_step_head_b(file_name, my_rank)
!
      module field_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_time_data_IO
      use m_field_data_IO
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
      subroutine write_step_field_file_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
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
     &          numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          num_phys_comp_IO, phys_data_name_IO, phys_data_IO)
!
      close (id_phys_file)
!
      end subroutine write_step_field_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_and_allocate_field_file_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      read(id_phys_file) numgrid_phys_IO, num_phys_data_IO
!
      call allocate_phys_data_name_IO
      read(id_phys_file) num_phys_comp_IO(1:num_phys_data_IO)
!
      call cal_istack_phys_comp_IO
      call allocate_phys_data_IO
!
      call read_field_data_b(id_phys_file,                              &
     &          numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          phys_data_name_IO, phys_data_IO)
      close (id_phys_file)
!
      end subroutine read_and_allocate_field_file_b
!
!------------------------------------------------------------------
!
      subroutine read_step_field_file_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) numgrid_phys_IO, num_phys_data_IO
      read(id_phys_file) num_phys_comp_IO(1:num_phys_data_IO)
!
      call read_field_data_b(id_phys_file,                              &
     &          numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          phys_data_name_IO, phys_data_IO)
      close (id_phys_file)
!
      end subroutine read_step_field_file_b
!
!------------------------------------------------------------------
!
      subroutine read_and_allocate_step_field_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) numgrid_phys_IO, num_phys_data_IO
!
      call allocate_phys_data_name_IO
      read(id_phys_file) num_phys_comp_IO(1:num_phys_data_IO)
!
      call cal_istack_phys_comp_IO
      call allocate_phys_data_IO
!
      call read_field_data_b(id_phys_file,                              &
     &          numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          phys_data_name_IO, phys_data_IO)
      close (id_phys_file)
!
      end subroutine read_and_allocate_step_field_b
!
!------------------------------------------------------------------
!
      subroutine read_and_allocate_step_head_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) numgrid_phys_IO, num_phys_data_IO
!
      call allocate_phys_data_name_IO
      read(id_phys_file) num_phys_comp_IO(1:num_phys_data_IO)
!
      close (id_phys_file)
!
      call cal_istack_phys_comp_IO
!
      end subroutine read_and_allocate_step_head_b
!
!------------------------------------------------------------------
!
      end module field_file_IO_b
