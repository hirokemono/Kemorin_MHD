      program iso_bin_test
!
      use m_precision
      use m_constants
      use m_field_file_format
!
      use t_file_IO_parameter
      use t_ucd_data
      use ucd_IO_select
!
      implicit none
!
      type(field_IO_params), parameter :: ucd_param2                    &
     &   = field_IO_params(iflag_IO = 0,                                &
     &                     file_prefix = 'iso/iso_temp2',               &
                           iflag_format = iflag_ucd_bin)
      type(field_IO_params), parameter :: ucd_param3                    &
     &   = field_IO_params(iflag_IO = 0,                                &
     &                     file_prefix = 'iso/iso_temp3',               &
                           iflag_format = iflag_ucd_bin_gz)
      type(field_IO_params), parameter :: vtk_out                       &
     &   = field_IO_params(iflag_IO = 0,                                &
     &                     file_prefix = 'iso_temp_x',                  &
                           iflag_format = iflag_vtk)
!
      integer(kind = kint), parameter :: istep = 800001
      integer :: np_ucd
      type(time_data), save :: t_IO
!
      type(ucd_data), save :: ucd_b
      type(ucd_data), save :: ucd_z
!
      write(*,*) 'sel_read_alloc_ucd_file ucd_b'
      call sel_read_alloc_ucd_file(0, np_ucd, istep,                    &
     &                             ucd_param2, t_IO, ucd_b)
!
      write(*,*) 'check_read_ucd_data ucd_b'
      call check_read_ucd_data(ucd_b)
!
      write(*,*) 'sel_read_alloc_ucd_file ucd_z'
      call sel_read_alloc_ucd_file(0, np_ucd, istep,                    &
     &                             ucd_param3, t_IO, ucd_z)
!
      write(*,*) 'check_read_ucd_data ucd_z'
      call check_read_ucd_data(ucd_z)
      write(*,*) 'compare_read_ucd_data ucd_z'
      call compare_read_ucd_data(ucd_b, ucd_z)
      call deallocate_ucd_mesh(ucd_b)
!
      call sel_write_ucd_file(0, istep, vtk_out, t_IO, ucd_z)
      call deallocate_ucd_mesh(ucd_z)
!
      end program iso_bin_test
