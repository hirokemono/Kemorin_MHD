!t_lic_noise_generator.f90
!
!      module t_lic_noise_generator
!
!      Written by Yangguang Liao 2018
!
!!      subroutine import_noise_nd_ary                                  &
!!     &         (filename, n_node_data, n_data_size, ierr)
!!      subroutine import_noise_ary(filename, n_raw_data, n_data_size)
!!      subroutine import_noise_grad_ary                                &
!!     &         (filename, n_grad_data, n_data_size, ierr)
!!
!!      subroutine noise_and_grad_sampling                              &
!!     &         (noise_t, xx_org, noise_value, grad_value)
!
      module t_lic_noise_generator
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
!
      type lic_old_noise
!>        file name of kernel function data
        character(len = kchara) :: noise_file_name
!>        file name of reflection file
        character(len = kchara) :: reflection_file_name
!>        1-D grid size of LIC noise, resolution = size * frequency
        integer(kind = kint) :: noise_resolution
!>        normalization factor for LIC value
        real(kind = kreal) :: freq_noise = one
!>        input noise texture size
        integer(kind = kint) :: noise_dim(3)
        integer(kind = kint) :: noise_size
!>        noise texture, 1 BYTE for each noise value
        character, allocatable:: noise_data(:)
!>        Precomputed noise gradient for lighting
        character, allocatable:: noise_grad_data(:)
!>        reflection parameter
        real(kind = kreal) :: reflection_parameter
      end type lic_old_noise
!
!>      Integer flag of endian swap
      type(binary_IO_buffer), private :: bbuf_nze
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_noise_data(noise_p)
!
      use t_control_data_LIC
!
      integer(kind = kint) :: read_err
      type(lic_old_noise), intent(inout) :: noise_p
!
      if(my_rank .eq. 0) write(*,*) 'loading noise texture from: ',     &
     &                             trim(noise_p%noise_file_name)
      call import_noise_ary(noise_p%noise_file_name,                    &
     &    noise_p%noise_data, noise_p%noise_dim, read_err)
      if(read_err .eq. 0) then
        if(iflag_debug .gt. 0) then
          write(*,*) 'loading noise successfuly, ',                     &
     &         'loading gradient from: ', noise_p%reflection_file_name
        end if
        call import_noise_grad_ary(noise_p%reflection_file_name,        &
     &      noise_p%noise_grad_data, noise_p%noise_dim, read_err)
      end if
!
      noise_p%noise_size = noise_p%noise_dim(1) * noise_p%noise_dim(2)  &
     &                  * noise_p%noise_dim(3)
      noise_p%freq_noise = noise_p%noise_resolution                     &
     &                  / noise_p%noise_dim(1)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'load noise texture from: ', noise_p%noise_file_name
        write(*,*) 'noise size: ', noise_p%noise_size
      end if
      end subroutine load_noise_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_noise_data(noise_p)
!
      type(lic_old_noise), intent(inout) :: noise_p
!
      if(noise_p%noise_size .gt. 0) then
        deallocate(noise_p%noise_data)
        deallocate(noise_p%noise_grad_data)
      end if
      end subroutine dealloc_lic_noise_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine import_noise_ary                                       &
     &         (filename, n_raw_data, n_data_size, ierr)

      use calypso_mpi_int
      use calypso_mpi_int8
      use calypso_mpi_char
      use transfer_to_long_integers
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
      call calypso_mpi_bcast_one_int(ierr, 0)
      call calypso_mpi_bcast_one_int(bbuf_nze%ierr_bin, 0)
      call calypso_mpi_bcast_one_int(bbuf_nze%iflag_swap, 0)
      call calypso_mpi_bcast_int(n_data_size, cast_long(3), 0)
      call calypso_mpi_bcast_one_int8(d_size, 0)
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
      use calypso_mpi_int
      use calypso_mpi_int8
      use calypso_mpi_char
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
      call calypso_mpi_bcast_one_int(ierr, 0)
      call calypso_mpi_bcast_one_int8(d_size, 0)
!
      if(my_rank .ne. 0) allocate( n_grad_data(d_size))
      call calypso_mpi_bcast_character(n_grad_data(1), d_size, 0)
!
      end subroutine import_noise_grad_ary
!
!  ---------------------------------------------------------------------
!
      end module t_lic_noise_generator
