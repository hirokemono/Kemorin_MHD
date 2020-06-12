      program iso_bin_test
!
      use m_precision
      use m_constants
!
      use t_ucd_data
      use read_psf_binary_file
      use gz_read_psf_binary_file
!
      implicit none
!
      character(len = kchara) :: bin_name = 'iso_temp2.800001.inb'
      character(len = kchara) :: gzip_name = 'iso_temp3.800001.inb.gz'
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
      call read_alloc_iso_binary_gz_file(gzip_name, ucd_z)
!
!      write(*,*) 'nprocs_gz', nprocs_gz
      call check_read_ucd_data(ucd_z)
      call compare_read_ucd_data(ucd_b, ucd_z)
!
      end program iso_bin_test
