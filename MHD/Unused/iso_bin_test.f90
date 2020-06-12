      program iso_bin_test
!
      use m_precision
      use m_constants
!
      use t_ucd_data
      use read_psf_binary_file
      use gz_read_psf_binary_file
      use vtk_file_IO
!
      implicit none
!
      character(len = kchara) :: bin_name = 'iso_temp2.800001.inb'
      character(len = kchara) :: gzip_name = 'iso_temp3.800001.inb.gz'
      character(len = kchara) :: out_name = 'iso_temp_x.800001.vtk'
      integer(kind = kint), parameter :: id_vtk_file = 16
!
      type(ucd_data), save :: ucd_b
      type(ucd_data), save :: ucd_z
!
!
      call read_alloc_iso_bindary_file(bin_name, ucd_b)
!
!      write(*,*) 'nprocs', nprocs
      call check_read_ucd_data(ucd_b)
!
      call gz_read_alloc_iso_bin_file(gzip_name, ucd_z)
!
!      write(*,*) 'nprocs_gz', nprocs_gz
      call check_read_ucd_data(ucd_z)
      call compare_read_ucd_data(ucd_b, ucd_z)
      call deallocate_ucd_mesh(ucd_b)
!
      call write_vtk_file(out_name, id_vtk_file, ucd_z)
      call deallocate_ucd_mesh(ucd_z)
!
      end program iso_bin_test
