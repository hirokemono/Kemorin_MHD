!>@file  defleat_4_merged_arrays.f90
!!       module defleat_4_merged_arrays
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief gzip compression and expansion for MPI-IO routines
!!
!!@verbatim
!!      subroutine defleat_int8_vector_mul(nloop, i8_array, zbuf)
!!      subroutine defleat_int_vector_mul(nloop, i_array, zbuf)
!!      subroutine defleat_int2d_vector_mul(nloop, iv_array, zbuf)
!!      subroutine defleat_1d_vector_mul(nloop, r_array, zbuf)
!!      subroutine defleat_2d_vector_mul(nloop, v_array, zbuf)
!!        type(int8array_IO), intent(in) ::  i8_array(nloop)
!!        type(intarray_IO), intent(in) ::  i_array(nloop)
!!        type(ivecarray_IO), intent(in) ::  iv_array(nloop)
!!        type(realarray_IO), intent(in) ::  r_array(nloop)
!!        type(vectarray_IO), intent(in) ::  v_array(nloop)
!!        type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!!
!!      subroutine infleat_int8_vector_mul                              &
!!     &         (iflag_bin_swap, nloop, i8_array, zbuf)
!!      subroutine infleat_int_vector_mul                               &
!!     &         (iflag_bin_swap, nloop, i_array, zbuf)
!!      subroutine infleat_int2d_vector_mul                             &
!!     &        (iflag_bin_swap, nloop, iv_array, zbuf)
!!      subroutine infleat_1d_vector_mul                                &
!!     &         (iflag_bin_swap, nloop, r_array, zbuf)
!!      subroutine infleat_2d_vector_mul                                &
!!     &         (iflag_bin_swap, nloop, v_array, zbuf)
!!        type(int8array_IO), intent(inout) ::  i8_array(nloop)
!!        type(intarray_IO), intent(inout) ::   i_array(nloop)
!!        type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
!!        type(realarray_IO), intent(inout) ::  r_array(nloop)
!!        type(vectarray_IO), intent(inout) ::  v_array(nloop)
!!        type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!!@endverbatim
!
      module defleat_4_merged_arrays
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gzip_array_mul(id_file, nprocs_in, nloop,   &
     &          ioff_gl, istack_merged, zbuf)
!
      use m_calypso_mpi_IO
!
      integer, intent(in) :: id_file
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        if(zbuf(iloop)%ilen_gzipped .gt. 0) then
          ioffset = ioff_gl + istack_merged(id_rank)
          call calypso_mpi_seek_write_gz(id_file, ioffset, zbuf(iloop))
        end if
        call dealloc_zip_buffer(zbuf(iloop))
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs)
!
      end subroutine mpi_write_gzip_array_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gzip_array_mul(id_file, nprocs_in, nloop,    &
     &          ioff_gl, istack_merged, zbuf)
!
      use m_calypso_mpi_IO
!
      integer, intent(in) :: id_file
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
!
        ioffset = ioff_gl + istack_merged(id_rank)
        zbuf(iloop)%ilen_gz = istack_merged(id_rank+1)                  &
     &                       - istack_merged(id_rank)
!
        call alloc_zip_buffer(zbuf(iloop))
        call calypso_mpi_seek_read_gz(id_file, ioffset, zbuf(iloop))
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs_in)
!
      end subroutine mpi_read_gzip_array_mul
!
! -----------------------------------------------------------------------
!
      subroutine set_istack_by_gzip_length                              &
     &         (nprocs_in, nloop, zbuf, istack_merged)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      type(buffer_4_gzip), intent(inout) ::  zbuf(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num_local(nloop)
!
!
      num_local(1:nloop) = zbuf(1:nloop)%ilen_gzipped
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_gzip_length
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine defleat_int8_vector_mul(nloop, i8_array, zbuf)
!
      integer(kind = kint), intent(in) :: nloop
      type(int8array_IO), intent(in) ::  i8_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
!
!
      do iloop = 1, nloop
        call defleate_int8_vector_b                                     &
     &     (i8_array(iloop)%num, i8_array(iloop)%i8_IO, zbuf(iloop))
      end do
!
      end subroutine defleat_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_int_vector_mul(nloop, i_array, zbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: nloop
      type(intarray_IO), intent(in) ::  i_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
      type(tmp_i8_array)  :: tmp64
!
!
      do iloop = 1, nloop
        call dup_from_short_array                                       &
     &     (i_array(iloop)%num, i_array(iloop)%i_IO, tmp64)
        call defleate_int8_vector_b(tmp64%n1, tmp64%id_a, zbuf(iloop))
        call dealloc_1d_i8array(tmp64)
      end do
!
      end subroutine defleat_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_int2d_vector_mul(nloop, iv_array, zbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: nloop
      type(ivecarray_IO), intent(in) ::  iv_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
      integer(kind = kint_gl) :: num
      type(tmp_i8_array)  :: tmp64
!
!
      do iloop = 1, nloop
        num =  iv_array(iloop)%n1 * iv_array(iloop)%n2
        call dup_from_short_array                                       &
     &     (num, iv_array(iloop)%iv_IO(1,1), tmp64)
        call defleate_int8_vector_b(tmp64%n1, tmp64%id_a, zbuf(iloop))
        call dealloc_1d_i8array(tmp64)
      end do
!
      end subroutine defleat_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_1d_vector_mul(nloop, r_array, zbuf)
!
      integer(kind = kint), intent(in) :: nloop
      type(realarray_IO), intent(in) ::  r_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
!
!
      do iloop = 1, nloop
        call defleate_1d_vector_b                                       &
     &     (r_array(iloop)%num, r_array(iloop)%r_IO(1), zbuf(iloop))
      end do
!
      end subroutine defleat_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_2d_vector_mul(nloop, v_array, zbuf)
!
      integer(kind = kint), intent(in) :: nloop
      type(vectarray_IO), intent(in) ::  v_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
      integer(kind = kint_gl) :: num
!
!
      do iloop = 1, nloop
        num =  v_array(iloop)%n1 * v_array(iloop)%n2
        call defleate_1d_vector_b                                       &
     &     (num, v_array(iloop)%v_IO(1,1), zbuf(iloop))
      end do
!
      end subroutine defleat_2d_vector_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleat_int8_vector_mul                                &
     &         (iflag_bin_swap, nloop, i8_array, zbuf)
!
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(int8array_IO), intent(inout) ::  i8_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        call infleate_int8_vector_b                                     &
     &     (i8_array(iloop)%num, i8_array(iloop)%i8_IO, zbuf(iloop))
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = i8_array(iloop)%num * kint_gl
          call byte_swap_64bit_f(l8_byte, i8_array(iloop)%i8_IO(1))
        end if
      end do
!
      end subroutine infleat_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_int_vector_mul                                 &
     &         (iflag_bin_swap, nloop, i_array, zbuf)
!
      use transfer_to_long_integers
!
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(intarray_IO), intent(inout) ::  i_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      type(tmp_i8_array)  :: tmp64
      integer(kind = kint) :: iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        call alloc_1d_i8array(i_array(iloop)%num, tmp64)
        call infleate_int8_vector_b(tmp64%n1, tmp64%id_a, zbuf(iloop))
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = tmp64%n1 * kint_gl
          call byte_swap_64bit_f(l8_byte, tmp64%id_a)
        end if
!
        call dup_to_short_array(tmp64, i_array(iloop)%i_IO)
      end do
!
      end subroutine infleat_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_int2d_vector_mul                               &
     &        (iflag_bin_swap, nloop, iv_array, zbuf)
!
      use transfer_to_long_integers
!
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      type(tmp_i8_array)  :: tmp64
      integer(kind = kint) :: iloop
      integer(kind = kint_gl) :: num
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        num =  iv_array(iloop)%n1 * iv_array(iloop)%n2
        call alloc_1d_i8array(num, tmp64)
        call infleate_int8_vector_b(tmp64%n1, tmp64%id_a, zbuf(iloop))
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = tmp64%n1 * kint_gl
          call byte_swap_64bit_f(l8_byte, tmp64%id_a)
        end if
!
        call dup_to_short_array(tmp64, iv_array(iloop)%iv_IO)
      end do
!
      end subroutine infleat_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_1d_vector_mul                                  &
     &         (iflag_bin_swap, nloop, r_array, zbuf)
!
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(realarray_IO), intent(inout) ::  r_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        call infleate_1d_vector_b                                       &
     &     (r_array(iloop)%num, r_array(iloop)%r_IO, zbuf(iloop))
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = r_array(iloop)%num * kreal
          call byte_swap_64bit_f(l8_byte, r_array(iloop)%r_IO(1))
        end if
      end do
!
      end subroutine infleat_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_2d_vector_mul                                  &
     &         (iflag_bin_swap, nloop, v_array, zbuf)
!
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(vectarray_IO), intent(inout) ::  v_array(nloop)
      type(buffer_4_gzip), intent(inout) :: zbuf(nloop)
!
      integer(kind = kint) :: iloop
      integer(kind = kint_gl) :: num
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        num =  v_array(iloop)%n1 * v_array(iloop)%n2
        call infleate_1d_vector_b                                       &
     &     (num, v_array(iloop)%v_IO, zbuf(iloop))
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = num * kreal
          call byte_swap_64bit_f(l8_byte, v_array(iloop)%v_IO(1,1))
        end if
      end do
!
      end subroutine infleat_2d_vector_mul
!
! -----------------------------------------------------------------------
!
      end module defleat_4_merged_arrays
