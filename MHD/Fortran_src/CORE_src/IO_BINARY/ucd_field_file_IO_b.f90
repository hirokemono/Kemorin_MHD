!>@file  ucd_field_file_IO_b.f90
!!       module ucd_field_file_IO_b
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief binary format data IO
!!
!!@verbatim
!!      subroutine write_ucd_2_fld_file_b(my_rank, istep, ucd)
!!
!!      subroutine read_ucd_2_fld_file_b(my_rank, istep, ucd)
!!      subroutine read_alloc_ucd_2_fld_file_b(my_rank, istep, ucd)
!!
!!      subroutine read_alloc_ucd_2_fld_header_b(my_rank, istep, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param my_rank  process ID
!!@param istep    step number for output
!!@param ucd      Structure for FEM field data IO
!
      module ucd_field_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_time_data_IO
      use m_field_file_format
!
      use t_ucd_data
      use set_ucd_file_names
      use field_data_IO_b
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_fld_file_b(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: nnod4
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write binary data file: ', trim(file_name)
!
      call open_write_binary_file(file_name)
!
      nnod4 = int(ucd%nnod)
      call write_step_data_b(my_rank)
      call write_field_data_b(nnod4, ucd%num_field,                     &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd)
      call close_binary_file
!
      end subroutine write_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_fld_file_b(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: nnod4
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank)
      call read_step_data_b(istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      nnod4 = int(istack_merged(1))
!
      call read_mul_integer_b(ucd%num_field, ucd%num_comp)
      call read_field_data_b(nnod4, ucd%num_field, ucd%ntot_comp,       &
     &    ucd%phys_name, ucd%d_ucd)
      call close_binary_file
!
      end subroutine read_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_file_b(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: nnod4
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank)
      call read_step_data_b(istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
      nnod4 = int(istack_merged(1))
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b(ucd%num_field, ucd%num_comp)
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_field_data_b(nnod4, ucd%num_field, ucd%ntot_comp,       &
     &    ucd%phys_name, ucd%d_ucd)
      call close_binary_file
!
      end subroutine read_alloc_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_header_b(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      call set_parallel_ucd_file_name(ucd%file_prefix, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      call open_read_binary_file(file_name, my_rank)
      call read_step_data_b(istack_merged, ucd%num_field)
      ucd%nnod = istack_merged(1)
!
      call allocate_ucd_phys_name(ucd)
!
      call read_mul_integer_b(ucd%num_field, ucd%num_comp)
      call close_binary_file
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      end subroutine read_alloc_ucd_2_fld_header_b
!
!------------------------------------------------------------------
!
      end module ucd_field_file_IO_b
