!field_2nd_IO_b.f90
!      module field_2nd_IO_b
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine write_step_field2_file_b(file_name, my_rank)
!
!      subroutine read_step_field2_file_b(file_name, my_rank)
!      subroutine read_and_alloc_step_field2_b(file_name, my_rank)
!
!      subroutine read_alloc_step_field2_head_b(file_name, my_rank)
!
      module field_2nd_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_time_data_IO
      use m_2nd_field_data_IO
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
      subroutine write_step_field2_file_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary data file: ', trim(file_name)
      end if
!
      open(id_fld2_file, file = file_name, form = 'unformatted')
!
      call write_step_data_b(id_fld2_file, my_rank)
      call write_field_data_b(id_fld2_file,                             &
     &          ngrid2_sph_IO, num_phys2_fld_IO, ntot_phys2_comp_IO,    &
     &          num_phys2_comp_IO, phys2_name_IO, phys2_data_IO)
!
      close (id_fld2_file)
!
      end subroutine write_step_field2_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_step_field2_file_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_fld2_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_fld2_file)
!
      read(id_fld2_file) ngrid2_sph_IO, num_phys2_fld_IO
      read(id_fld2_file) num_phys2_comp_IO(1:num_phys2_fld_IO)
!
      call read_field_data_b(id_fld2_file,                              &
     &          ngrid2_sph_IO, num_phys2_fld_IO, ntot_phys2_comp_IO,    &
     &          phys2_name_IO, phys2_data_IO)
      close (id_fld2_file)
!
      end subroutine read_step_field2_file_b
!
!------------------------------------------------------------------
!
      subroutine read_and_alloc_step_field2_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_fld2_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_fld2_file)
!
      read(id_fld2_file) ngrid2_sph_IO, num_phys2_fld_IO
!
      call allocate_field2_name_IO
      read(id_fld2_file) num_phys2_comp_IO(1:num_phys2_fld_IO)
!
      call cal_istack_fld2_comp_IO
      call allocate_field2_data_IO
!
      call read_field_data_b(id_fld2_file,                              &
     &          ngrid2_sph_IO, num_phys2_fld_IO, ntot_phys2_comp_IO,    &
     &          phys2_name_IO, phys2_data_IO)
      close (id_fld2_file)
!
      end subroutine read_and_alloc_step_field2_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_step_field2_head_b(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary data file: ', trim(file_name)
      end if
!
      open(id_fld2_file, file = file_name, form = 'unformatted')
      call read_step_data_b(id_fld2_file)
!
      read(id_fld2_file) ngrid2_sph_IO, num_phys2_fld_IO
!
      call allocate_field2_name_IO
      read(id_fld2_file) num_phys2_comp_IO(1:num_phys2_fld_IO)
!
      close (id_fld2_file)
!
      call cal_istack_fld2_comp_IO
!
      end subroutine read_alloc_step_field2_head_b
!
!------------------------------------------------------------------
!
      end module field_2nd_IO_b
