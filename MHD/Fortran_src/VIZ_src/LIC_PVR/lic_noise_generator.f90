!lic_noise_generator.f90
!
!      module lic_noise_generator
!
!      Written by Yangguang Liao 2018
!
!!      subroutine import_noise_nd_ary                                  &
!!     &         (filename, n_node_data, n_data_size, ierr)
!!      subroutine import_noise_ary(filename, n_raw_data, n_data_size)
!!      subroutine import_noise_grad_ary                                &
!!     &         (filename, n_grad_data, n_data_size, ierr)
!!
!!      subroutine noise_nd_sampling(noise_size, f_noise, n_node,       &
!!     &          xx_org, xyz_min, xyz_max, noise_value)
!!      subroutine noise_and_grad_sampling(noise_t,                     &
!!     &          n_cube, f_noise, noise_data, noise_grad,              &
!!     &          xx_org, xyz_min, xyz_max, noise_value, grad_value)
!
      module lic_noise_generator
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_binary_IO_buffer
      use binary_IO
!
      implicit  none
!
!>      Integer flag of endian swap
      type(binary_IO_buffer), private :: bbuf_nze
!
!
      private :: noise_sampling, noise_grad_sampling
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine import_noise_nd_ary                                    &
     &         (filename, n_node_data, n_data_size, ierr)

      use t_noise_node_data
      use binary_file_access
      use set_parallel_file_name

      character(len = kchara), intent(in) :: filename
      integer(kind = kint), intent(inout) :: n_data_size(3)
      type(noise_node), intent(inout), pointer :: n_node_data(:)
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint) :: d_size, i
      character(len=kchara) :: file_name
      character(len=1) :: noise_char(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl), parameter :: ithree64 = 3
      integer(kind = kint_4b) :: isize3d_tmp(3)
!
!
      file_name = add_null_character(filename)
      call open_rd_rawfile_f(file_name, bbuf_nze)
      if(bbuf_nze%ierr_bin .ne. 0) go to 99
! first line read 3 integer size data, byte 4
      call read_mul_int_from_32bit(bbuf_nze, ithree64, isize3d_tmp)
      n_data_size(1:3) = int(isize3d_tmp(1:3), KIND(n_data_size(1)))
!
      if(bbuf_nze%ierr_bin .ne. 0) go to 99
      d_size = n_data_size(1)*n_data_size(2)*n_data_size(3)
!      write(*,*) d_size
      allocate(n_node_data(d_size))
      do i=1, d_size
!  change 0 to any level to initial complex noise node tree
        call alloc_noise_node(n_node_data(i), itwo, izero)
        call read_mul_one_character_b(bbuf_nze, ione64, noise_char)
        if(bbuf_nze%ierr_bin .gt. 0) go to 99
!
        n_node_data(i)%n_value = ichar(noise_char(1)) / 255.0
!        write(*,*) n_node_data(i)%n_value
      end do
!
  99  continue
      call close_rawfile_f()
      ierr = bbuf_nze%ierr_bin
!
      end subroutine import_noise_nd_ary
!
!  ---------------------------------------------------------------------
!
      subroutine import_noise_ary                                       &
     &         (filename, n_raw_data, n_data_size, ierr)

      use set_parallel_file_name
      use binary_file_access

! parameter for read noise data
      character(len = kchara), intent(in) :: filename
      integer(kind = kint), intent(inout) :: n_data_size(3)
      character(len=1), allocatable, intent(inout) :: n_raw_data(:)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: d_size, i
      character(len=kchara) :: file_name
      character(len=1) :: one_chara(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl), parameter :: ithree64 = 3
      integer(kind = kint_4b) :: isize3d_tmp(3)
      real(kind = kreal) :: ave_noise
!
!
      if(my_rank .eq. 0) then
        bbuf_nze%iflag_swap = iendian_KEEP
        file_name = add_null_character(filename)
        call open_rd_rawfile_f(file_name, bbuf_nze)
        if(bbuf_nze%ierr_bin .ne. 0) go to 99
! first line read 3 integer size data, byte 4
        bbuf_nze%iflag_swap = iendian_KEEP
        call read_mul_int_from_32bit(bbuf_nze, ithree64, isize3d_tmp)
        n_data_size(1:3) = int(isize3d_tmp(1:3), KIND(n_data_size(1)))
        if(bbuf_nze%ierr_bin .ne. 0) go to 99
!
        d_size = n_data_size(1)*n_data_size(2)*n_data_size(3)
        if(iflag_debug .gt. 0) write(*,*) 'd_size',                     &
     &                           d_size, n_data_size(1:3)
!
        bbuf_nze%iflag_swap = iendian_KEEP
        call seek_forward_binary_file(d_size-1, bbuf_nze)
        call read_mul_one_character_b(bbuf_nze, ione64, one_chara)
        if(bbuf_nze%ierr_bin .gt. 0) bbuf_nze%iflag_swap = iendian_FLIP
!          write(*,*) 'iflag_swap a', bbuf_nze%iflag_swap
        call read_mul_one_character_b(bbuf_nze, ione64, one_chara)
        if(bbuf_nze%ierr_bin .eq. 0) bbuf_nze%iflag_swap = iendian_FLIP
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'iflag_swap', bbuf_nze%iflag_swap
  99    continue
        call close_rawfile_f()
        ierr = bbuf_nze%ierr_bin
!
        call open_rd_rawfile_f(file_name, bbuf_nze)
        if(bbuf_nze%ierr_bin .ne. 0) go to 98
! first line read 3 integer size data, byte 4
        call read_mul_int_from_32bit(bbuf_nze, ithree64, isize3d_tmp)
        n_data_size(1:3) = int(isize3d_tmp(1:3), KIND(n_data_size(1)))
!
        d_size = n_data_size(1)*n_data_size(2)*n_data_size(3)
        if(iflag_debug .gt. 0) write(*,*) 'd_size again',               &
     &                                     d_size, n_data_size(1:3)
!
        allocate( n_raw_data(d_size))  ! allocate space for noise data
        call read_mul_one_character_b(bbuf_nze, d_size, n_raw_data)
!
  98    continue
        call close_rawfile_f()
        ierr = bbuf_nze%ierr_bin
!
        if(iflag_debug .gt. 0) then
          open(111, file='noise_text.dat')
          do i = 1, d_size
            write(111,'(2i6)') iachar(n_raw_data(i))
          end do
          close(111)
        end if
!
        ave_noise = 0.0d0
        do i = 1, d_size
          ave_noise = ave_noise + ichar(n_raw_data(i)) / 256.0
        end do
        ave_noise = ave_noise / dble(d_size)
        write(*,*) 'Average of noise:', ave_noise
!
      end if
!
      call MPI_BCAST(ierr, 1,                                           &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(bbuf_nze%ierr_bin, 1,                              &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(bbuf_nze%iflag_swap, 1,                            &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(n_data_size, 3,                                    &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(d_size, 1,                                         &
     &    CALYPSO_GLOBAL_INT, 0, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) allocate( n_raw_data(d_size))
      call calypso_mpi_bcast_character(n_raw_data(1), d_size, 0)
!
      end subroutine import_noise_ary
!
!  ---------------------------------------------------------------------
!
      subroutine import_noise_grad_ary                                  &
     &         (filename, n_grad_data, n_data_size, ierr)
!
      use set_parallel_file_name
      use binary_file_access
      use delete_data_files
!
! parameter for read noise data
      character(len = kchara), intent(in) :: filename
      integer(kind = kint), intent(in) :: n_data_size(3)
      character(len=1), allocatable, intent(inout) :: n_grad_data(:)
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint_gl) :: d_size
      character(len=kchara) :: file_name
!
!
      if(my_rank .eq. 0) then
        file_name = add_null_character(filename)
        d_size = n_data_size(1)*n_data_size(2)*n_data_size(3)*3
        if(check_file_exist(file_name)) then
          call open_rd_rawfile_f(file_name, bbuf_nze)
          if(bbuf_nze%ierr_bin .ne. 0) go to 99
!
          allocate( n_grad_data(d_size)) 
          call read_mul_one_character_b(bbuf_nze, d_size, n_grad_data)
!
  99      continue
          call close_rawfile_f()
          ierr = bbuf_nze%ierr_bin
        else
          allocate(n_grad_data(d_size))
          n_grad_data(1:d_size) = char(0)
        end if
      end if
!
      call MPI_BCAST(ierr, 1,                                           &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(d_size, 1,                                         &
     &    CALYPSO_GLOBAL_INT, 0, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) allocate( n_grad_data(d_size))
      call calypso_mpi_bcast_character(n_grad_data(1), d_size, 0)
!
      end subroutine import_noise_grad_ary
!
!  ---------------------------------------------------------------------
!
      subroutine cal_pos_idx_volume                                     &
     &         (noise_size, xx_org, xyz_min, xyz_max, idx)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: noise_size
      real(kind = kreal), intent(in) :: xx_org(3), xyz_min(3), xyz_max(3)
      integer(kind = kint), intent(inout) :: idx
      integer(kind = kint) :: xyz_i(3)
!
      integer(kind = kint) :: dim
      real(kind = kreal) :: xyz_norm(3)
!
!
      xyz_norm = (xx_org - xyz_min) / (xyz_max - xyz_min)
      dim = int(noise_size**(1./3.),KIND(dim))
      xyz_i(1:3) = int(xyz_norm(1:3) * dim, KIND(xyz_i(1)))
      xyz_i(1:3) = mod((xyz_i(1:3) * 2), dim)

      idx = xyz_i(1) + (xyz_i(2)-1) * dim + (xyz_i(3)-1) * dim * dim

      end subroutine cal_pos_idx_volume
!
!  ---------------------------------------------------------------------
!

      function get_noise_value(noise_size, noise_data, idx)

      use set_parallel_file_name

      !
      integer(kind = kint), intent(in) :: noise_size
      character(len=1), intent(in) :: noise_data(noise_size)
      integer(kind = kint), intent(in) :: idx
      real(kind = kreal) :: get_noise_value

      !
      if(idx > noise_size) then
        get_noise_value = 0.0
        return
      end if
      get_noise_value = ichar(noise_data(idx)) / 255.0
      !if(get_noise_value > 50.0) then
      !  get_noise_value = 1.0
      !else
      !  get_noise_value = 0.0
      !end if

      end function get_noise_value
!
!  ---------------------------------------------------------------------
!
      function get_noise_nd_value                                       &
     &       (noise_size, noise_node_data, idx, level)

      use set_parallel_file_name
      use t_noise_node_data
      !
      integer(kind = kint), intent(in) :: noise_size
      type(noise_node), intent(in) :: noise_node_data(noise_size)
      integer(kind = kint), intent(in) :: idx, level
      real(kind = kreal) :: get_noise_nd_value
      ! depends one sampleing level, we choose noise value
      !
      if(idx > noise_size) then
        get_noise_nd_value = 0.0
        return
      end if
      get_noise_nd_value = noise_node_data(idx)%n_value

      end function get_noise_nd_value
!
!  ---------------------------------------------------------------------
!

      function get_noise_grad_value(noise_size, grad_data, idx)

      use set_parallel_file_name

      !
      integer(kind = kint), intent(in) :: noise_size
      character(len=1), intent(in) :: grad_data(noise_size*3)
      integer(kind = kint), intent(in) :: idx
      real(kind = kreal) :: get_noise_grad_value(3)
      integer(kind = kint) :: i, i_idx

      !
      if(idx > noise_size) then
        get_noise_grad_value(1:3) = 0.0
        return
      end if
      i_idx = idx - 1
      do i = 1, 3
        get_noise_grad_value(i) = ichar(grad_data(i_idx*3 + i)) / 255.0
      end do
      !if(get_noise_value > 50.0) then
      !  get_noise_value = 1.0
      !else
      !  get_noise_value = 0.0
      !end if

      end function get_noise_grad_value
!
!  ---------------------------------------------------------------------
!
      function get_offset_vol(x, y, z, noise_dim)
      integer(kind=kint) :: get_offset_vol

      integer(kind = kint) :: x, y, z, noise_dim

      if(x .gt. noise_dim) x = noise_dim
      if(x .lt. 1) x = 1
      if(y .gt. noise_dim) y = noise_dim
      if(y .lt. 1) y = 1
      if(z .gt. noise_dim) z = noise_dim
      if(z .lt. 1) z = 1
      get_offset_vol = x + (y-1)*noise_dim + (z-1)*noise_dim*noise_dim

      end function get_offset_vol
!
!  ---------------------------------------------------------------------
!
      subroutine noise_sampling(noise_size, f_noise, noise_data,        &
     &          xx_org, xyz_min, xyz_max, noise_value)

      use set_parallel_file_name
      !
      integer(kind = kint), intent(in) :: noise_size
      real(kind = kreal), intent(in) :: f_noise
      character(len=1), intent(in) :: noise_data(noise_size)
      real(kind = kreal), intent(in) :: xx_org(3), xyz_min(3), xyz_max(3)
      real(kind = kreal), intent(inout) :: noise_value
      integer(kind = kint) :: idx000, idx001, idx010, idx011
      integer(kind = kint) :: idx100, idx101, idx110, idx111
      real(kind = kreal) :: xyz(3), xyz_d(3), c00, c01, c10, c11, c0, c1
      integer(kind = kint) :: xyz_i(3)
      !
      integer(kind = kint) :: dim
      real(kind = kreal) :: xyz_norm(3)

      noise_value = 0.0
      xyz_norm = (xx_org - xyz_min) / (xyz_max - xyz_min)
      xyz_norm = xyz_norm * f_noise
      xyz_norm = xyz_norm - int(xyz_norm)
      dim = int(noise_size**(1./3.))
      xyz = xyz_norm * dim
      xyz_i = int(xyz_norm * dim + 0.5)
      xyz_d = xyz + 0.5 - xyz_i

      idx000 = get_offset_vol(xyz_i(1), xyz_i(2), xyz_i(3), dim)
      idx001 = get_offset_vol(xyz_i(1), xyz_i(2), xyz_i(3)+1, dim)
      idx010 = get_offset_vol(xyz_i(1), xyz_i(2)+1, xyz_i(3), dim)
      idx011 = get_offset_vol(xyz_i(1), xyz_i(2)+1, xyz_i(3)+1, dim)
      idx100 = get_offset_vol(xyz_i(1)+1, xyz_i(2), xyz_i(3), dim)
      idx101 = get_offset_vol(xyz_i(1)+1, xyz_i(2), xyz_i(3)+1, dim)
      idx110 = get_offset_vol(xyz_i(1)+1, xyz_i(2)+1, xyz_i(3), dim)
      idx111 = get_offset_vol(xyz_i(1)+1, xyz_i(2)+1, xyz_i(3)+1, dim)
      c00 = get_noise_value(noise_size, noise_data, idx000) * (1 - xyz_d(1))  &
      &   + get_noise_value(noise_size, noise_data, idx100) * xyz_d(1)
      c01 = get_noise_value(noise_size, noise_data, idx001) * (1 - xyz_d(1))  &
      &   + get_noise_value(noise_size, noise_data, idx101) * xyz_d(1)
      c10 = get_noise_value(noise_size, noise_data, idx010) * (1 - xyz_d(1))  &
      &   + get_noise_value(noise_size, noise_data, idx110) * xyz_d(1)
      c11 = get_noise_value(noise_size, noise_data, idx011) * (1 - xyz_d(1))  &
      &   + get_noise_value(noise_size, noise_data, idx111) * xyz_d(1)

      c0 = c00 * (1 - xyz_d(2)) + c10 * xyz_d(2)
      c1 = c01 * (1 - xyz_d(2)) + c11 * xyz_d(2)
      noise_value = c0 * (1 - xyz_d(3)) + c1 * xyz_d(3)

      !xyz_i = mod((xyz_i * 2), dim)

      end subroutine noise_sampling
!
!  ---------------------------------------------------------------------
!
      subroutine noise_grad_sampling(n_cube, f_noise,               &
     &          noise_grad, xx_org, xyz_min, xyz_max, grad_value)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: n_cube
      real(kind = kreal), intent(in) :: f_noise
      character(len=1), intent(in) :: noise_grad(n_cube*3)
      real(kind = kreal), intent(in) :: xx_org(3), xyz_min(3), xyz_max(3)
      real(kind = kreal), intent(inout) :: grad_value(3)
      integer(kind = kint) :: idx000,idx001,idx010,idx011,idx100,idx101,idx110,idx111
      real(kind = kreal) :: xyz(3), xyz_d(3), c00(3), c01(3), c10(3), c11(3), c0(3), c1(3)
      integer(kind = kint) :: xyz_i(3)
!
      integer(kind = kint) :: dim
      real(kind = kreal) :: xyz_norm(3)

      grad_value(1:3) = 0.0
      xyz_norm = (xx_org - xyz_min) / (xyz_max - xyz_min)
      xyz_norm = xyz_norm * f_noise
      xyz_norm = xyz_norm - int(xyz_norm)
      dim = int(n_cube**(1./3.))
      xyz = xyz_norm * dim
      xyz_i = int(xyz_norm * dim + 0.5)
      xyz_d = xyz + 0.5 - xyz_i
      idx000 = get_offset_vol(xyz_i(1), xyz_i(2), xyz_i(3), dim)
      idx001 = get_offset_vol(xyz_i(1), xyz_i(2), xyz_i(3)+1, dim)
      idx010 = get_offset_vol(xyz_i(1), xyz_i(2)+1, xyz_i(3), dim)
      idx011 = get_offset_vol(xyz_i(1), xyz_i(2)+1, xyz_i(3)+1, dim)
      idx100 = get_offset_vol(xyz_i(1)+1, xyz_i(2), xyz_i(3), dim)
      idx101 = get_offset_vol(xyz_i(1)+1, xyz_i(2), xyz_i(3)+1, dim)
      idx110 = get_offset_vol(xyz_i(1)+1, xyz_i(2)+1, xyz_i(3), dim)
      idx111 = get_offset_vol(xyz_i(1)+1, xyz_i(2)+1, xyz_i(3)+1, dim)
      c00 = get_noise_grad_value(n_cube, noise_grad, idx000) * (1 - xyz_d(1))  &
      &   + get_noise_grad_value(n_cube, noise_grad, idx100) * xyz_d(1)
      c01 = get_noise_grad_value(n_cube, noise_grad, idx001) * (1 - xyz_d(1))  &
      &   + get_noise_grad_value(n_cube, noise_grad, idx101) * xyz_d(1)
      c10 = get_noise_grad_value(n_cube, noise_grad, idx010) * (1 - xyz_d(1))  &
      &   + get_noise_grad_value(n_cube, noise_grad, idx110) * xyz_d(1)
      c11 = get_noise_grad_value(n_cube, noise_grad, idx011) * (1 - xyz_d(1))  &
      &   + get_noise_grad_value(n_cube, noise_grad, idx111) * xyz_d(1)

      c0 = c00 * (1 - xyz_d(2)) + c10 * xyz_d(2)
      c1 = c01 * (1 - xyz_d(2)) + c11 * xyz_d(2)
      grad_value = c0 * (1 - xyz_d(3)) + c1 * xyz_d(3)

      !xyz_i = mod((xyz_i * 2), dim)

      end subroutine noise_grad_sampling
!
!  ---------------------------------------------------------------------
!
      subroutine noise_nd_sampling(noise_size, f_noise, n_node,         &
     &          xx_org, xyz_min, xyz_max, noise_value)

      use t_noise_node_data
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: noise_size
      real(kind = kreal), intent(in) :: f_noise
      type(noise_node), intent(in) :: n_node(noise_size)
      real(kind = kreal), intent(in) :: xx_org(3), xyz_min(3), xyz_max(3)
      real(kind = kreal), intent(inout) :: noise_value
      integer(kind = kint) :: idx
      real(kind = kreal) :: xyz(3)
      integer(kind = kint) :: xyz_i(3)
!
      integer(kind = kint) :: dim
      real(kind = kreal) :: xyz_norm(3)

      noise_value = 0.0
      xyz_norm = (xx_org - xyz_min) / (xyz_max - xyz_min)
      xyz_norm = xyz_norm * f_noise
      xyz_norm = xyz_norm - int(xyz_norm)
      dim = int(noise_size**(1./3.))
      xyz = xyz_norm * dim
      xyz_i = int(xyz_norm * (dim-1))+1
      idx = get_offset_vol(xyz_i(1), xyz_i(2), xyz_i(3), dim)
      noise_value = get_noise_nd_value(noise_size, n_node, idx, izero)

      end subroutine noise_nd_sampling
!
!  ---------------------------------------------------------------------
!
      subroutine noise_and_grad_sampling(noise_t,                       &
     &          n_cube, f_noise, noise_data, noise_grad, xx_org,        &
     &          xyz_min, xyz_max, noise_value, grad_value)
!
      use t_3d_noise
      use cal_noise_value
!
      integer(kind = kint), intent(in) :: n_cube
      real(kind = kreal), intent(in) :: f_noise
      character(len=1), intent(in) :: noise_data(n_cube)
      character(len=1), intent(in) :: noise_grad(n_cube*3)
      real(kind = kreal), intent(in) :: xx_org(3)
      real(kind = kreal), intent(in) :: xyz_min(3), xyz_max(3)
      type(noise_cube), intent(in) :: noise_t
!
      real(kind = kreal), intent(inout) :: noise_value
      real(kind = kreal), intent(inout) :: grad_value(3)
!
      real(kind = kreal) :: neo_noise, neo_grad(3)
!
      integer(kind = kint_gl) :: i
!

      call interpolate_noise_at_node                                    &
     &   (xx_org, noise_t, noise_value, grad_value)
      return
!
      call noise_sampling(n_cube, f_noise, noise_data,                  &
     &    xx_org, xyz_min, xyz_max, noise_value)
      call noise_grad_sampling(n_cube, f_noise, noise_grad,             &
     &    xx_org, xyz_min, xyz_max, grad_value)
      if(neo_noise .gt. 0) write(*,*) 'noise_value', neo_noise, noise_value
!
      end subroutine noise_and_grad_sampling
!
!  ---------------------------------------------------------------------
!
      end module lic_noise_generator
