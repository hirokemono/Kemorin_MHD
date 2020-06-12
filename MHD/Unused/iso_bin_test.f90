      program iso_bin_test
!
      use m_precision
      use m_constants
      use t_binary_IO_buffer
      use t_buffer_4_gzip
      use binary_IO
      use gz_binary_IO
      use gzip_file_access
      use transfer_to_long_integers
!
      use t_ucd_data
!
      implicit none
!
      character(len = kchara) :: bin_name = 'iso_temp2.800001.inb'
      character(len = kchara) :: gzip_name = 'iso_temp3.800001.inb.gz'
      type(binary_IO_buffer) :: bbuf
      type(buffer_4_gzip) :: zbuf
!
      integer :: nprocs, nprocs_gz
      integer(kind = kint_gl), allocatable :: itmp1_mp(:), itmp1_mp_gz(:)
      integer(kind = kint_gl) :: nnod_gl, nnod_gl_gz
      integer(kind = kint_gl) :: nele_gl, nele_gl_gz
      integer(kind = kint_gl) :: eletype(1)
      integer(kind = kint_gl), allocatable :: ie(:,:), ie_gz(:,:)
      real(kind = kreal), allocatable :: xx(:,:), xx_gz(:,:)
!
      integer :: nprocs2, nprocs2_gz
      integer(kind = kint) :: num_field, num_field_gz
      integer(kind = kint) :: ntot_comp, ntot_comp_gz
      integer(kind = kint), allocatable :: num_comp(:), num_comp_gz(:)
      character(len = kchara), allocatable :: name(:), name_gz(:)
      integer(kind = kint_gl), allocatable :: n_inter2(:), n_inter2_gz(:)
      real(kind = kreal), allocatable :: d_nod(:,:), d_nod_gz(:,:)
      integer(kind = kint_gl) :: nnod_gl2, nnod_gl2_gz
!
      integer(kind = kint_gl) :: num64, int_gl_dat(1000)
      integer(kind = kint) :: nd, inod
!
      integer(kind = kint) :: nnod_ele, nnod_ele_gz
!
      type(ucd_data), save :: ucd_b
      type(merged_ucd_data), save :: m_ucd_b
!
      type(ucd_data), save :: ucd_z
      type(merged_ucd_data), save :: m_ucd_z
!
      call open_read_binary_file(bin_name, izero, bbuf)
      call read_one_integer_b(bbuf, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_psf_node_num_bin(nprocs, nnod_gl, itmp1_mp, bbuf)
!
      allocate(xx(nnod_gl,3))
      call read_psf_phys_data_bin                                       &
     &   (nprocs, nnod_gl, ithree, xx, itmp1_mp, bbuf)
!
      call read_psf_ele_num_bin                                         &
     &   (nprocs, nele_gl, nnod_ele, itmp1_mp, bbuf)
!
      allocate(ie(nele_gl,nnod_ele))
      call read_psf_ele_connect_bin                                     &
     &   (nprocs, nele_gl, nnod_ele, ie, itmp1_mp, bbuf)
!
      write(*,*) 'nprocs', nprocs
      write(*,*) 'nnod_gl', nnod_gl
      write(*,*) 'xx_1', xx(1,1:3)
      write(*,*) 'xx_2', xx(2,1:3)
      write(*,*) 'xx_3', xx(3,1:3)
      write(*,*) 'xx_1', xx(nnod_gl-2,1:3)
      write(*,*) 'xx_2', xx(nnod_gl-1,1:3)
      write(*,*) 'xx_3', xx(nnod_gl,  1:3)
!
      write(*,*) 'nele_gl', nele_gl
      write(*,*) 'ie_1', ie(1,1:3)
      write(*,*) 'ie_2', ie(2,1:3)
      write(*,*) 'ie_3', ie(3,1:3)
      write(*,*) 'ie_1', ie(nele_gl-2,1:3)
      write(*,*) 'ie_2', ie(nele_gl-1,1:3)
      write(*,*) 'ie_3', ie(nele_gl,  1:3)
!
!
      call read_one_integer_b(bbuf, nprocs2)
!
      call read_psf_phys_num_bin                                  &
     &   (nprocs, nnod_gl2, num_field, itmp1_mp, bbuf)
      allocate(num_comp(num_field))
      allocate(name(num_field))
!
      call read_psf_phys_name_bin                                 &
     &   (num_field, ntot_comp, num_comp, name, bbuf)

      allocate(d_nod(nnod_gl2, ntot_comp))
      call read_psf_phys_data_bin                                 &
     &   (nprocs, nnod_gl2, ntot_comp, d_nod, itmp1_mp, bbuf)
      call close_binary_file(bbuf)
!
      write(*,*) 'nprocs2', nprocs2
      write(*,*) 'nnod_gl2', nnod_gl2
      write(*,*) 'num_field', num_field, ntot_comp
      write(*,*) 'num_comp', num_comp
      write(*,*) 'name: ', name
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
     &   (nprocs_gz, nnod_gl_gz, itmp1_mp_gz, zbuf)
!
      allocate(xx_gz(nnod_gl_gz,3))
      call read_psf_phys_data_bin_gz                                    &
     &   (nprocs_gz, nnod_gl_gz, ithree, xx_gz, itmp1_mp_gz, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (nprocs_gz, nele_gl_gz, nnod_ele_gz, itmp1_mp_gz, zbuf)
!
      allocate(ie_gz(nele_gl_gz,nnod_ele_gz))
      call read_psf_ele_connect_bin_gz                                  &
     &   (nprocs_gz, nele_gl_gz, nnod_ele_gz, ie_gz, itmp1_mp_gz, zbuf)
!
      write(*,*) 'nprocs_gz', nprocs_gz
      write(*,*) 'nnod_gl_gz', nnod_gl_gz
      write(*,*) 'xx_gz_1', xx_gz(1,1:3)
      write(*,*) 'xx_gz_2', xx_gz(2,1:3)
      write(*,*) 'xx_gz_3', xx_gz(3,1:3)
      write(*,*) 'xx_gz_1', xx_gz(nnod_gl_gz-2,1:3)
      write(*,*) 'xx_gz_2', xx_gz(nnod_gl_gz-1,1:3)
      write(*,*) 'xx_gz_3', xx_gz(nnod_gl_gz,  1:3)
!
      write(*,*) 'nele_gl_gz', nele_gl_gz
      write(*,*) 'ie_gz_1', ie_gz(1,1:3)
      write(*,*) 'ie_gz_2', ie_gz(2,1:3)
      write(*,*) 'ie_gz_3', ie_gz(3,1:3)
      write(*,*) 'ie_gz_1', ie_gz(nele_gl_gz-2,1:3)
      write(*,*) 'ie_gz_2', ie_gz(nele_gl_gz-1,1:3)
      write(*,*) 'ie_gz_3', ie_gz(nele_gl_gz,  1:3)
!
!
      call gz_read_one_integer_b(zbuf, nprocs2_gz)
!
      call read_psf_phys_num_bin_gz                                     &
     &   (nprocs2_gz, nnod_gl2_gz, num_field_gz, itmp1_mp_gz, zbuf)
!
      allocate(num_comp_gz(num_field_gz))
      allocate(name_gz(num_field_gz))
!
      call read_psf_phys_name_bin_gz                                    &
     &   (num_field_gz, ntot_comp_gz, num_comp_gz, name_gz, zbuf)
      allocate(d_nod_gz(nnod_gl2_gz, ntot_comp_gz))
!
      call read_psf_phys_data_bin_gz                                    &
     &   (nprocs2_gz, nnod_gl2_gz, ntot_comp_gz, d_nod_gz,              &
     &    itmp1_mp_gz, zbuf)
      call close_gzfile_b
!
      write(*,*) 'nprocs2_gz', nprocs2_gz
      write(*,*) 'n_inter2_gz', n_inter2_gz
      write(*,*) 'nnod_gl2_gz', nnod_gl2_gz
      write(*,*) 'num_field_gz', num_field_gz, ntot_comp_gz
      write(*,*) 'num_comp_gz', num_comp_gz
      write(*,*) 'name_gz: ', name_gz
!
      write(*,*) 'itmp1_mp_gz', itmp1_mp_gz
!
      write(*,*) 'Check ie'
      do nd = 1, nnod_ele_gz
        do inod = 1, nele_gl_gz
          if(ie(inod,nd) .ne. ie_gz(inod,nd)) then
            write(*,*) 'Baka ie at ', nd, inod,                         &
     &                ie_gz(inod,nd), ie(inod,nd)
          end if
        end do
      end do
!
      write(*,*) 'Check xx'
      do nd = 1, 3
        do inod = 1, nnod_gl2_gz
          if(xx(inod,nd) .ne. xx_gz(inod,nd)) then
            write(*,*) 'Baka xx at ', nd, inod,                         &
     &                xx_gz(inod,nd), xx(inod,nd)
          end if
        end do
      end do
!
      write(*,*) 'Check d_nod'
      do nd = 1, ntot_comp_gz
        do inod = 1, nnod_gl2_gz
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
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_num_bin                                  &
     &         (nprocs, nnod, itmp1_mp, bbuf)
!
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind=kint_gl), intent(inout) :: nnod
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs_gz)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: n_inter(:)
!
!
      allocate(n_inter(nprocs))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), n_inter)
      nnod = sum(n_inter)
!
!      write(*,*) 'n_inter', n_inter
      deallocate(n_inter)
!
      end subroutine read_psf_node_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_num_bin                                  &
     &         (nprocs, nnod, num_field, itmp1_mp, bbuf)
!
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind=kint_gl), intent(inout) :: nnod
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: n_inter(:)
!
!
      allocate(n_inter(nprocs))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), n_inter)
      nnod = sum(n_inter)
!
      call read_one_integer_b(bbuf, num_field)
!
!      write(*,*) 'n_inter', n_inter
      deallocate(n_inter)
!
      end subroutine read_psf_phys_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_name_bin                                 &
     &         (num_field, ntot_comp, ncomp_field, field_name, bbuf)
!
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint), intent(in) :: num_field
!
      integer(kind=kint), intent(inout) :: ntot_comp
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_mul_integer_b(bbuf, cast_long(num_field), ncomp_field)
      call read_mul_character_b(bbuf, num_field, field_name)
      ntot_comp = sum(ncomp_field)
!
      end subroutine read_psf_phys_name_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_data_bin                                 &
     &         (nprocs, nnod, ntot_comp, d_nod, itmp1_mp, bbuf)
!
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs_gz)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, ntot_comp
        call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
        call read_1d_vector_b(bbuf, nnod, d_nod(1,nd))
      end do
!
      end subroutine read_psf_phys_data_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_num_bin                               &
     &         (nprocs, nele, nnod_ele, itmp1_mp, bbuf)
!
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs_gz)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl), allocatable :: nele_lc(:)
      integer(kind = kint_gl) :: eletype(1), nnod_ele_b(1)
!
!
      call read_mul_int8_b(bbuf, cast_long(ione), nnod_ele_b)
      call read_mul_int8_b(bbuf, cast_long(ione), eletype)
!
      allocate(nele_lc(nprocs))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), nele_lc)
!
      nnod_ele = int(nnod_ele_b(1),KIND(nnod_ele))
      nele = sum(nele_lc)
!
!      write(*,*) 'nele_lc', nele_lc
!      write(*,*) 'nnod_ele_b', nnod_ele_b
!      write(*,*) 'eletype', eletype
!
      deallocate(nele_lc)
!
      end subroutine read_psf_ele_num_bin
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_connect_bin                               &
     &         (nprocs, nele, nnod_ele, ie_psf, itmp1_mp, bbuf)
!
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint_gl), intent(inout) :: ie_psf(nele,nnod_ele)
      integer(kind = kint_gl), intent(inout) :: itmp1_mp(nprocs_gz)
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, nnod_ele
        call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
        call read_mul_int8_b(bbuf, nele, ie_psf(1,nd))
      end do
!
      end subroutine read_psf_ele_connect_bin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_psf_node_num_bin_gz                               &
     &         (nprocs, nnod, itmp1_mp_gz, zbuf)
!
      use t_buffer_4_gzip
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind=kint_gl), intent(inout) :: nnod
      integer(kind = kint_gl), intent(inout) :: itmp1_mp_gz(nprocs)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl), allocatable :: n_inter(:)
!
!
      allocate(n_inter(nprocs))
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs), itmp1_mp_gz)
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs), n_inter)
      nnod = sum(n_inter)
!
!      write(*,*) 'n_inter', n_inter
      deallocate(n_inter)
!
      end subroutine read_psf_node_num_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_num_bin_gz                               &
     &         (nprocs, nnod, num_field, itmp1_mp_gz, zbuf)
!
      use t_buffer_4_gzip
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind=kint_gl), intent(inout) :: nnod
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: itmp1_mp_gz(nprocs)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl), allocatable :: n_inter(:)
!
!
      allocate(n_inter(nprocs))
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs), itmp1_mp_gz)
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs), n_inter)
      nnod = sum(n_inter)
!
      call gz_read_one_integer_b(zbuf, num_field)
!
!      write(*,*) 'n_inter', n_inter
      deallocate(n_inter)
!
      end subroutine read_psf_phys_num_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_name_bin_gz                              &
     &         (num_field, ntot_comp, ncomp_field, field_name, zbuf)
!
      use t_buffer_4_gzip
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer(kind=kint), intent(in) :: num_field
!
      integer(kind=kint), intent(inout) :: ntot_comp
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
!
      call gz_read_mul_integer_b                                        &
     &   (zbuf, cast_long(num_field), ncomp_field)
      call gz_read_mul_character_b                                      &
     &   (zbuf, cast_long(num_field), field_name)
      ntot_comp = sum(ncomp_field)
!
      end subroutine read_psf_phys_name_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_phys_data_bin_gz                              &
     &         (nprocs, nnod, ntot_comp, d_nod, itmp1_mp_gz, zbuf)
!
      use t_buffer_4_gzip
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
      integer(kind = kint_gl), intent(inout) :: itmp1_mp_gz(nprocs)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, ntot_comp
        call gz_read_mul_int8_b(zbuf, cast_long(nprocs), itmp1_mp_gz)
        call gz_read_1d_vector_b(zbuf, nnod, d_nod(1,nd))
      end do
!
      end subroutine read_psf_phys_data_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_num_bin_gz                            &
     &         (nprocs, nele, nnod_ele, itmp1_mp_gz, zbuf)
!
      use t_buffer_4_gzip
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
!
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
      integer(kind = kint_gl), intent(inout) :: itmp1_mp_gz(nprocs)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint_gl), allocatable :: nele_lc(:)
      integer(kind = kint_gl) :: eletype(1), nnod_ele_b(1)
!
!
      call gz_read_mul_int8_b(zbuf, cast_long(ione), nnod_ele_b)
      call gz_read_mul_int8_b(zbuf, cast_long(ione), eletype)
!
      allocate(nele_lc(nprocs))
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs), itmp1_mp_gz)
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs), nele_lc)
!
      nnod_ele = int(nnod_ele_b(1),KIND(nnod_ele))
      nele = sum(nele_lc)
!
!      write(*,*) 'nnod_ele_b', nnod_ele_b
!      write(*,*) 'nele_lc', nele_lc
!      write(*,*) 'eletype', eletype
!
      deallocate(nele_lc)
!
      end subroutine read_psf_ele_num_bin_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_psf_ele_connect_bin_gz                            &
     &         (nprocs, nele, nnod_ele, ie_psf, itmp1_mp_gz, zbuf)
!
      use t_buffer_4_gzip
      use gz_binary_IO
      use transfer_to_long_integers
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint_gl), intent(inout) :: ie_psf(nele,nnod_ele)
      integer(kind = kint_gl), intent(inout) :: itmp1_mp_gz(nprocs)
      type(buffer_4_gzip), intent(inout)  :: zbuf
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, nnod_ele
        call gz_read_mul_int8_b(zbuf, cast_long(nprocs), itmp1_mp_gz)
        call gz_read_mul_int8_b(zbuf, nele, ie_psf(1,nd))
      end do
!
      end subroutine read_psf_ele_connect_bin_gz
!
! -----------------------------------------------------------------------
