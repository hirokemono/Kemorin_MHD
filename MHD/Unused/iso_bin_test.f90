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
      implicit none
!
      character(len = kchara) :: bin_name = 'iso_temp2.800001.inb'
      character(len = kchara) :: gzip_name = 'iso_temp3.800001.inb.gz'
      type(binary_IO_buffer) :: bbuf
      type(buffer_4_gzip) :: zbuf
!
      integer(kind = kint) :: nprocs, nprocs_gz
      integer(kind = kint_gl), allocatable :: itmp1_mp(:), itmp1_mp_gz(:)
      integer(kind = kint_gl), allocatable :: n_inter(:), n_inter_gz(:)
      integer(kind = kint_gl), allocatable :: nele(:), nele_gz(:)
      integer(kind = kint_gl) :: nnod_gl, nnod_gl_gz
      integer(kind = kint_gl) :: nele_gl, nele_gl_gz
      integer(kind = kint_gl) :: nnod_ele(1), nnod_ele_gz(1)
      integer(kind = kint_gl) :: eletype(1), eletype_gz(1)
      integer(kind = kint_gl), allocatable :: ie(:,:), ie_gz(:,:)
      real(kind = kreal), allocatable :: xx(:,:), xx_gz(:,:)
!
      integer(kind = kint) :: nprocs2, nprocs2_gz
      integer(kind = kint) :: num_field, num_field_gz
      integer(kind = kint) :: ntot_comp, ntot_comp_gz
      integer(kind = kint_gl), allocatable :: num_comp(:), num_comp_gz(:)
      character(len = kchara), allocatable :: name(:), name_gz(:)
      integer(kind = kint_gl), allocatable :: n_inter2(:), n_inter2_gz(:)
      real(kind = kreal), allocatable :: d_nod(:,:), d_nod_gz(:,:)
      integer(kind = kint_gl) :: nnod_gl2, nnod_gl2_gz
!
      integer(kind = kint_gl) :: num64, int_gl_dat(1000)
      integer(kind = kint) :: nd, inod
!
!
      call open_read_binary_file(bin_name, izero, bbuf)
      call read_one_integer_b(bbuf, nprocs)
      allocate(itmp1_mp(nprocs))
      allocate(n_inter(nprocs))
      allocate(nele(nprocs))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), n_inter)
!
      nnod_gl = sum(n_inter)
      allocate(xx(nnod_gl,3))
!
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_1d_vector_b(bbuf, nnod_gl, xx(1,1))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_1d_vector_b(bbuf, nnod_gl, xx(1,2))
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_1d_vector_b(bbuf, nnod_gl, xx(1,3))

      call read_mul_int8_b(bbuf, cast_long(ione), nnod_ele)
      call read_mul_int8_b(bbuf, cast_long(ione), eletype)
!
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), nele)
!
      nele_gl = sum(nele)
      allocate(ie(nele_gl,nnod_ele(1)))
!
      do nd = 1, nnod_ele(1)
        call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
        call read_mul_int8_b(bbuf, nele_gl, ie(1,nd))
      end do
!
      write(*,*) 'nprocs', nprocs
      write(*,*) 'n_inter', n_inter
      write(*,*) 'nnod_gl', nnod_gl
      write(*,*) 'xx_1', xx(1,1:3)
      write(*,*) 'xx_2', xx(2,1:3)
      write(*,*) 'xx_3', xx(3,1:3)
      write(*,*) 'xx_1', xx(nnod_gl-2,1:3)
      write(*,*) 'xx_2', xx(nnod_gl-1,1:3)
      write(*,*) 'xx_3', xx(nnod_gl,  1:3)
!
      write(*,*) 'nnod_ele', nnod_ele
      write(*,*) 'eletype', eletype
      write(*,*) 'nele', nele
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
      allocate(n_inter2(nprocs2))
!
      call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
      call read_mul_int8_b(bbuf, cast_long(nprocs), n_inter2)
!
      call read_one_integer_b(bbuf, num_field)
      allocate(num_comp(num_field))
      allocate(name(num_field))
!
      call read_mul_int8_b(bbuf, cast_long(num_field), num_comp)
      call read_mul_character_b(bbuf, num_field, name)

      nnod_gl2 = sum(n_inter2)
      ntot_comp = sum(num_comp)
      allocate(d_nod(nnod_gl2, ntot_comp))
!
      do nd = 1, ntot_comp
        call read_mul_int8_b(bbuf, cast_long(nprocs), itmp1_mp)
        call read_1d_vector_b(bbuf, nnod_gl2, d_nod(1,nd))
      end do
!
      call close_binary_file(bbuf)
      write(*,*) 'nprocs2', nprocs2
      write(*,*) 'n_inter2', n_inter2
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
      allocate(n_inter_gz(nprocs_gz))
      allocate(nele_gz(nprocs))
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), n_inter_gz)
!
      nnod_gl_gz = sum(n_inter_gz)
      allocate(xx_gz(nnod_gl_gz,3))
!
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
      call gz_read_1d_vector_b(zbuf, nnod_gl, xx_gz(1,1))
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
      call gz_read_1d_vector_b(zbuf, nnod_gl, xx_gz(1,2))
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
      call gz_read_1d_vector_b(zbuf, nnod_gl, xx_gz(1,3))

      call gz_read_mul_int8_b(zbuf, cast_long(ione), nnod_ele_gz)
      call gz_read_mul_int8_b(zbuf, cast_long(ione), eletype_gz)
!
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), nele_gz)
!
      nele_gl_gz = sum(nele_gz)
      allocate(ie_gz(nele_gl_gz,nnod_ele_gz(1)))
!
      do nd = 1, nnod_ele_gz(1)
        call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
        call gz_read_mul_int8_b(zbuf, nele_gl_gz, ie_gz(1,nd))
      end do
!
      write(*,*) 'nprocs_gz', nprocs_gz
      write(*,*) 'n_inter_gz', n_inter_gz
      write(*,*) 'nnod_gl_gz', nnod_gl_gz
      write(*,*) 'xx_gz_1', xx_gz(1,1:3)
      write(*,*) 'xx_gz_2', xx_gz(2,1:3)
      write(*,*) 'xx_gz_3', xx_gz(3,1:3)
      write(*,*) 'xx_gz_1', xx_gz(nnod_gl_gz-2,1:3)
      write(*,*) 'xx_gz_2', xx_gz(nnod_gl_gz-1,1:3)
      write(*,*) 'xx_gz_3', xx_gz(nnod_gl_gz,  1:3)
!
      write(*,*) 'nnod_ele_gz', nnod_ele_gz
      write(*,*) 'eletype_gz', eletype_gz
      write(*,*) 'nele_gz', nele_gz
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
      allocate(n_inter2_gz(nprocs2_gz))
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
      call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), n_inter2_gz)
!
      call gz_read_one_integer_b(zbuf, num_field_gz)
!
      allocate(num_comp_gz(num_field_gz))
      allocate(name_gz(num_field_gz))
      call gz_read_mul_int8_b                                           &
     &   (zbuf, cast_long(num_field_gz), num_comp_gz)
      call gz_read_mul_character_b                                      &
     &   (zbuf, cast_long(num_field_gz), name_gz)
      nnod_gl2_gz =  sum(n_inter2_gz)
      ntot_comp_gz = sum(num_comp_gz)
      allocate(d_nod_gz(nnod_gl2_gz, ntot_comp_gz))
!
      do nd = 1, ntot_comp_gz
        call gz_read_mul_int8_b(zbuf, cast_long(nprocs_gz), itmp1_mp_gz)
        call gz_read_1d_vector_b(zbuf, nnod_gl2_gz, d_nod_gz(1,nd))
      end do
!
      call close_gzfile_b
      write(*,*) 'nprocs2_gz', nprocs2_gz
      write(*,*) 'n_inter2_gz', n_inter2_gz
      write(*,*) 'nnod_gl2_gz', nnod_gl2_gz
      write(*,*) 'num_field_gz', num_field_gz, ntot_comp_gz
      write(*,*) 'num_comp_gz', num_comp_gz
      write(*,*) 'name_gz: ', name_gz
!
      write(*,*) 'itmp1_mp_gz', itmp1_mp_gz
!
      do nd = 1, nnod_ele_gz(1)
        do inod = 1, nele_gz(1)
          if(ie(inod,nd) .ne. ie_gz(inod,nd)) then
            write(*,*) 'Baka ie at ', nd, inod,                         &
     &                ie_gz(inod,nd), ie(inod,nd)
          end if
        end do
      end do
!
      do nd = 1, 3
        do inod = 1, nnod_gl2_gz
          if(xx(inod,nd) .ne. xx_gz(inod,nd)) then
            write(*,*) 'Baka xx at ', nd, inod,                         &
     &                xx_gz(inod,nd), xx(inod,nd)
          end if
        end do
      end do
!
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
