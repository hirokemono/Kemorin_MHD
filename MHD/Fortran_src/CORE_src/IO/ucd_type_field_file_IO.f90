!>@file  ucd_type_field_file_IO.f90
!!       module ucd_type_field_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief ascii format data IO
!!
!!@verbatim
!!      subroutine write_ucd_type_fld_file(my_rank, istep, ucd)
!!
!!      subroutine read_ucd_type_fld_file(my_rank, istep, ucd)
!!      subroutine read_alloc_ucd_type_fld_file(my_rank, istep, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param my_rank  process ID
!!@param istep    step number for output
!!@param ucd      Structure for FEM field data IO
!
      module ucd_type_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_time_data_IO
      use m_field_file_format
!
      use t_ucd_data
!
      use field_data_IO
      use skip_gz_comment
      use set_ucd_file_names
!
      implicit none
!
!>      file ID for field file IO
      integer(kind = kint), parameter, private :: id_fld_file = 16
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_type_fld_file(my_rank, istep, ucd)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd%header_name, iflag_fld,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write ascii step data file: ', trim(file_name)
!
      open(id_fld_file, file = file_name, form = 'formatted')
!
      call write_step_data(id_fld_file, my_rank)
      call write_field_data(id_fld_file,                                &
     &          ucd%nnod, ucd%num_field, ucd%ntot_comp,                 &
     &          ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      close (id_fld_file)
!
      end subroutine write_ucd_type_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_type_fld_file(my_rank, istep, ucd)
!
      use skip_comment_f
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
      character(len=255) :: character_4_read
!
!
      call set_parallel_ucd_file_name(ucd%header_name, iflag_fld,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read ascii data file: ', trim(file_name)
!
      open(id_fld_file, file = file_name, form = 'formatted')
!
      call read_step_data(id_fld_file)
!
      call skip_comment(character_4_read, id_fld_file)
      read(character_4_read,*) ucd%nnod, ucd%num_field
      read(id_fld_file,*) ucd%num_comp(1:ucd%num_field)
!
      call read_field_data(id_fld_file,                                 &
     &          ucd%nnod, ucd%num_field, ucd%ntot_comp,                 &
     &          ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      close (id_fld_file)
!
      end subroutine read_ucd_type_fld_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_type_fld_file(my_rank, istep, ucd)
!
      use skip_comment_f
!
      integer(kind=kint), intent(in) :: my_rank, istep
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name
      character(len=255) :: character_4_read
!
!
      call set_parallel_ucd_file_name(ucd%header_name, iflag_fld,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read ascii data file: ', trim(file_name)
!
      open(id_fld_file, file = file_name, form = 'formatted')
!
      call read_step_data(id_fld_file)
!
      call skip_comment(character_4_read, id_fld_file)
      read(character_4_read,*) ucd%nnod, ucd%num_field
!
      call alloc_ucd_phys_name_t(ucd)
      read(id_fld_file,*) ucd%num_comp(1:ucd%num_field)
!
      call cal_istack_ucd_comp_type(ucd)
      call alloc_ucd_phys_data_t(ucd)
!
      call read_field_data(id_fld_file,                                 &
     &          ucd%nnod, ucd%num_field, ucd%ntot_comp,                 &
     &          ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      close (id_fld_file)
!
      end subroutine read_alloc_ucd_type_fld_file
!
!------------------------------------------------------------------
!
      end module ucd_type_field_file_IO
