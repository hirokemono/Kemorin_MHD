!>@file  ucd_field_file_IO_b.f90
!!       module ucd_field_file_IO_b
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief binary format data IO
!!
!!@verbatim
!!      subroutine write_ucd_2_fld_file_b(my_rank, istep)
!!
!!      subroutine read_ucd_2_fld_file_b(my_rank, istep)
!!      subroutine read_alloc_ucd_2_fld_file_b(my_rank, istep)
!!
!!      subroutine read_alloc_ucd_2_fld_header_b(my_rank, istep)
!!@endverbatim
!!
!!@param my_rank  process ID
!!@param istep    step number for output
!
      module ucd_field_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_time_data_IO
      use m_ucd_data
      use m_field_file_format
      use field_data_IO
      use skip_gz_comment
      use set_ucd_file_names
!
      implicit none
!
!>      file ID for binary field data IO
      integer(kind = kint), parameter, private :: id_binary_fld = 16
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_fld_file_b(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write binary data file: ', trim(file_name)
!
      open(id_binary_fld, file = file_name, form = 'unformatted')
!
      call write_step_data_b(id_binary_fld, my_rank)
      call write_field_data_b(id_binary_fld,                            &
     &          nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      close (id_binary_fld)
!
      end subroutine write_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_fld_file_b(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      open(id_binary_fld, file = file_name, form = 'unformatted')
!
      call read_step_data_b(id_binary_fld)
!
      read(id_binary_fld) nnod_ucd, num_field_ucd
      read(id_binary_fld) num_comp_ucd(1:num_field_ucd)
!
      call read_field_data_b(id_binary_fld,                             &
     &          nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          phys_name_ucd, d_nod_ucd)
!
      close (id_binary_fld)
!
      end subroutine read_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_file_b(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      open(id_binary_fld, file = file_name, form = 'unformatted')
!
      call read_step_data_b(id_binary_fld)
      read(id_binary_fld) nnod_ucd, num_field_ucd
!
      call allocate_ucd_phys_name
      read(id_binary_fld) num_comp_ucd(1:num_field_ucd)
!
      call cal_istack_ucd_component
      call allocate_ucd_phys_data
!
      call read_field_data_b(id_binary_fld,                             &
     &          nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          phys_name_ucd, d_nod_ucd)
!
      close (id_binary_fld)
!
      end subroutine read_alloc_ucd_2_fld_file_b
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_header_b(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_bin,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read binary data file: ', trim(file_name)
!
      open(id_binary_fld, file = file_name, form = 'unformatted')
!
      call read_step_data_b(id_binary_fld)
      read(id_binary_fld) nnod_ucd, num_field_ucd
!
      call allocate_ucd_phys_name
      read(id_binary_fld) num_comp_ucd(1:num_field_ucd)
!
      close (id_binary_fld)
!
      call cal_istack_ucd_component
      call allocate_ucd_phys_data
!
      end subroutine read_alloc_ucd_2_fld_header_b
!
!------------------------------------------------------------------
!
      end module ucd_field_file_IO_b
