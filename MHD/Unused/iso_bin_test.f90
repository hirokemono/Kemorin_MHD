      program iso_bin_test
!
      use m_precision
      use m_constants
      use binary_IO
      use gz_binary_IO
      use gzip_file_access
      use transfer_to_long_integers
      use read_psf_binary_data
      use gz_read_psf_binary_data
!
      use t_binary_IO_buffer
      use t_buffer_4_gzip
      use t_ucd_data
!
      implicit none
!
      character(len = kchara) :: bin_name = 'iso_temp2.800001.inb'
      character(len = kchara) :: gzip_name = 'iso_temp3.800001.inb.gz'
!
      integer :: nprocs, nprocs_gz
      integer(kind = kint_gl), allocatable :: itmp1_mp(:), itmp1_mp_gz(:)
!
      integer :: nprocs2, nprocs2_gz
      real(kind = kreal), allocatable :: d_nod(:,:), d_nod_gz(:,:)
!
      integer(kind = kint_gl) :: inod, num64, int_gl_dat(1000)
      integer(kind = kint) :: nd
!
      type(ucd_data), save :: ucd_b
      type(binary_IO_buffer) :: bbuf
!
      type(ucd_data), save :: ucd_z
      type(buffer_4_gzip) :: zbuf
!
      call open_read_binary_file(bin_name, izero, bbuf)
      call read_one_integer_b(bbuf, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_psf_node_num_bin(nprocs, ucd_b%nnod, itmp1_mp, bbuf)
!
      call allocate_ucd_node(ucd_b)
      call read_psf_phys_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ithree, ucd_b%xx, itmp1_mp, bbuf)
!
      do inod = 1, ucd_b%nnod
        ucd_b%inod_global(inod) = inod
      end do
!
      call read_psf_ele_num_bin                                         &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele, itmp1_mp, bbuf)
!
      call allocate_ucd_ele(ucd_b)
      call read_psf_ele_connect_bin                                     &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele, ucd_b%ie,               &
     &    itmp1_mp, bbuf)
!
      do inod = 1, ucd_b%nele
        ucd_b%iele_global(inod) = inod
      end do
!
      write(*,*) 'nprocs', nprocs
      write(*,*) 'ucd_b%nnod', ucd_b%nnod
      write(*,*) 'xx_1', ucd_b%xx(1,1:3)
      write(*,*) 'xx_2', ucd_b%xx(2,1:3)
      write(*,*) 'xx_3', ucd_b%xx(3,1:3)
      write(*,*) 'xx_1', ucd_b%xx(ucd_b%nnod-2,1:3)
      write(*,*) 'xx_2', ucd_b%xx(ucd_b%nnod-1,1:3)
      write(*,*) 'xx_3', ucd_b%xx(ucd_b%nnod,  1:3)
!
      write(*,*) 'ucd_b%nele', ucd_b%nele
      write(*,*) 'ie_1', ucd_b%ie(1,1:3)
      write(*,*) 'ie_2', ucd_b%ie(2,1:3)
      write(*,*) 'ie_3', ucd_b%ie(3,1:3)
      write(*,*) 'ie_1', ucd_b%ie(ucd_b%nele-2,1:3)
      write(*,*) 'ie_2', ucd_b%ie(ucd_b%nele-1,1:3)
      write(*,*) 'ie_3', ucd_b%ie(ucd_b%nele,  1:3)
!
!
      call read_one_integer_b(bbuf, nprocs2)
!
      call read_psf_phys_num_bin                                        &
     &   (nprocs, ucd_b%nnod, ucd_b%num_field, itmp1_mp, bbuf)
!
      call allocate_ucd_phys_name(ucd_b)
      call read_psf_phys_name_bin                                       &
     &   (ucd_b%num_field, ucd_b%ntot_comp, ucd_b%num_comp,             &
     &    ucd_b%phys_name, bbuf)

      allocate(d_nod(ucd_b%nnod, ucd_b%ntot_comp))
      call read_psf_phys_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%ntot_comp, d_nod, itmp1_mp, bbuf)
      call close_binary_file(bbuf)
!
      write(*,*) 'nprocs2', nprocs2
      write(*,*) 'ucd_b%num_field', ucd_b%num_field, ucd_b%ntot_comp
      write(*,*) 'ucd_b%num_comp', ucd_b%num_comp
      write(*,*) 'ucd_b%phys_name: ', ucd_b%phys_name
!
      write(*,*) 'itmp1_mp', itmp1_mp
!
!
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf)
      call gz_read_one_integer_b(zbuf, nprocs_gz)
      allocate(itmp1_mp_gz(nprocs_gz))
!
      call read_psf_node_num_bin_gz                                     &
     &   (nprocs_gz, ucd_z%nnod, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_node(ucd_z)
      call read_psf_phys_data_bin_gz                                    &
     &   (nprocs_gz, ucd_z%nnod, ithree, ucd_z%xx, itmp1_mp_gz, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (nprocs_gz, ucd_z%nele, ucd_z%nnod_4_ele, itmp1_mp_gz, zbuf)
!
      do inod = 1, ucd_z%nnod
        ucd_z%inod_global(inod) = inod
      end do
!
      call allocate_ucd_ele(ucd_z)
      call read_psf_ele_connect_bin_gz                                  &
     &   (nprocs_gz, ucd_z%nele, ucd_z%nnod_4_ele, ucd_z%ie,            &
     &    itmp1_mp_gz, zbuf)
!
      do inod = 1, ucd_z%nele
        ucd_z%iele_global(inod) = inod
      end do
!
      write(*,*) 'nprocs_gz', nprocs_gz
      write(*,*) 'ucd_z%nnod', ucd_z%nnod
      write(*,*) 'xx_gz_1', ucd_z%xx(1,1:3)
      write(*,*) 'xx_gz_2', ucd_z%xx(2,1:3)
      write(*,*) 'xx_gz_3', ucd_z%xx(3,1:3)
      write(*,*) 'xx_gz_1', ucd_z%xx(ucd_z%nnod-2,1:3)
      write(*,*) 'xx_gz_2', ucd_z%xx(ucd_z%nnod-1,1:3)
      write(*,*) 'xx_gz_3', ucd_z%xx(ucd_z%nnod,  1:3)
!
      write(*,*) 'ucd_z%nele', ucd_z%nele
      write(*,*) 'ie_gz_1', ucd_z%ie(1,1:3)
      write(*,*) 'ie_gz_2', ucd_z%ie(2,1:3)
      write(*,*) 'ie_gz_3', ucd_z%ie(3,1:3)
      write(*,*) 'ie_gz_1', ucd_z%ie(ucd_z%nele-2,1:3)
      write(*,*) 'ie_gz_2', ucd_z%ie(ucd_z%nele-1,1:3)
      write(*,*) 'ie_gz_3', ucd_z%ie(ucd_z%nele,  1:3)
!
!
      call gz_read_one_integer_b(zbuf, nprocs2_gz)
!
      call read_psf_phys_num_bin_gz                                     &
     &   (nprocs2_gz, ucd_z%nnod, ucd_z%num_field, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_phys_name(ucd_z)
      call read_psf_phys_name_bin_gz                                    &
     &   (ucd_z%num_field, ucd_z%ntot_comp, ucd_z%num_comp,             &
     &    ucd_z%phys_name, zbuf)
      allocate(d_nod_gz(ucd_z%nnod, ucd_z%ntot_comp))
!
      call read_psf_phys_data_bin_gz                                    &
     &   (nprocs2_gz, ucd_z%nnod, ucd_z%ntot_comp, d_nod_gz,            &
     &    itmp1_mp_gz, zbuf)
      call close_gzfile_b
!
      write(*,*) 'nprocs2_gz', nprocs2_gz
      write(*,*) 'ucd_z%num_field', ucd_z%num_field, ucd_z%ntot_comp
      write(*,*) 'ucd_z%num_comp', ucd_z%num_comp
      write(*,*) 'ucd_z%phys_name: ', ucd_z%phys_name
!
      write(*,*) 'itmp1_mp_gz', itmp1_mp_gz
!
      write(*,*) 'Check ie'
      do nd = 1, ucd_z%nnod_4_ele
        do inod = 1, ucd_z%nele
          if(ucd_b%ie(inod,nd) .ne. ucd_z%ie(inod,nd)) then
            write(*,*) 'Baka ie at ', nd, inod,                         &
     &                ucd_z%ie(inod,nd), ucd_b%ie(inod,nd)
          end if
        end do
      end do
!
      write(*,*) 'Check xx'
      do nd = 1, 3
        do inod = 1, ucd_z%nnod
          if(ucd_b%xx(inod,nd) .ne. ucd_z%xx(inod,nd)) then
            write(*,*) 'Baka xx at ', nd, inod,                         &
     &                ucd_z%xx(inod,nd), ucd_b%xx(inod,nd)
          end if
        end do
      end do
!
      write(*,*) 'Check d_nod'
      do nd = 1, ucd_z%ntot_comp
        do inod = 1, ucd_z%nnod
          if(d_nod_gz(inod,nd) .ne. d_nod(inod,nd)) then
            write(*,*) 'Baka at ', nd, inod,                            &
     &                d_nod_gz(inod,nd), d_nod(inod,nd)
          end if
        end do
      end do
!
      write(*,*) 'i_UNIX', i_UNIX
      write(*,*) 'i_XINU', i_XINU
!
      end program iso_bin_test
!
