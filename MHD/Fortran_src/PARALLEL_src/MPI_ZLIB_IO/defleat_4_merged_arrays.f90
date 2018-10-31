!>@file  defleat_4_merged_arrays.f90
!!       module defleat_4_merged_arrays
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief gzip compression and expansion for MPI-IO routines
!!
!!@verbatim
!!      subroutine defleat_int8_vector_mul(nloop, i8_array, c_array)
!!      subroutine defleat_int_vector_mul(nloop, i_array, c_array)
!!      subroutine defleat_int2d_vector_mul(nloop, iv_array, c_array)
!!      subroutine defleat_1d_vector_mul(nloop, r_array, c_array)
!!      subroutine defleat_2d_vector_mul(nloop, v_array, c_array)
!!        type(int8array_IO), intent(in) ::  i8_array(nloop)
!!        type(intarray_IO), intent(in) ::  i_array(nloop)
!!        type(ivecarray_IO), intent(in) ::  iv_array(nloop)
!!        type(realarray_IO), intent(in) ::  r_array(nloop)
!!        type(vectarray_IO), intent(in) ::  v_array(nloop)
!!        type(charaarray_IO), intent(inout) :: c_array(nloop)
!!
!!      subroutine infleat_int8_vector_mul                              &
!!     &         (iflag_bin_swap, nloop, c_array, i8_array)
!!      subroutine infleat_int_vector_mul                               &
!!     &         (iflag_bin_swap, nloop, c_array, i_array)
!!      subroutine infleat_int2d_vector_mul                             &
!!     &        (iflag_bin_swap, nloop, c_array, iv_array)
!!      subroutine infleat_1d_vector_mul                                &
!!     &         (iflag_bin_swap, nloop, c_array, r_array)
!!      subroutine infleat_2d_vector_mul                                &
!!     &         (iflag_bin_swap, nloop, c_array, v_array)
!!        type(charaarray_IO), intent(inout) :: c_array(nloop)
!!        type(int8array_IO), intent(inout) ::  i8_array(nloop)
!!        type(intarray_IO), intent(inout) ::   i_array(nloop)
!!        type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
!!        type(realarray_IO), intent(inout) ::  r_array(nloop)
!!        type(vectarray_IO), intent(inout) ::  v_array(nloop)
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
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine defleat_int8_vector_mul(nloop, i8_array, c_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(int8array_IO), intent(in) ::  i8_array(nloop)
      type(charaarray_IO), intent(inout) :: c_array(nloop)
!
      integer(kind = kint) :: ilen_gz, ilength, iloop
!
!
      do iloop = 1, nloop
        ilength =  i8_array(iloop)%num * kint_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(c_array(iloop)%c_IO(ilen_gz))
        call gzip_defleat_once(ilength, i8_array(iloop)%i8_IO(1),       &
     &      ilen_gz, c_array(iloop)%num, c_array(iloop)%c_IO(1))
      end do
!
      end subroutine defleat_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_int_vector_mul(nloop, i_array, c_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(intarray_IO), intent(in) ::  i_array(nloop)
      type(charaarray_IO), intent(inout) :: c_array(nloop)
!
      integer(kind = kint) :: ilen_gz, ilength, iloop
!
!
      do iloop = 1, nloop
        ilength =  i_array(iloop)%num * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(c_array(iloop)%c_IO(ilen_gz))
        call gzip_defleat_once(ilength, i_array(iloop)%i_IO(1),         &
     &      ilen_gz, c_array(iloop)%num, c_array(iloop)%c_IO(1))
      end do
!
      end subroutine defleat_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_int2d_vector_mul(nloop, iv_array, c_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(ivecarray_IO), intent(in) ::  iv_array(nloop)
      type(charaarray_IO), intent(inout) :: c_array(nloop)
!
      integer(kind = kint) :: ilen_gz, ilength, iloop
!
!
      do iloop = 1, nloop
        ilength =  iv_array(iloop)%n1 * iv_array(iloop)%n2 * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(c_array(iloop)%c_IO(ilen_gz))
        call gzip_defleat_once(ilength, iv_array(iloop)%iv_IO(1,1),     &
     &      ilen_gz, c_array(iloop)%num, c_array(iloop)%c_IO(1))
      end do
!
      end subroutine defleat_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_1d_vector_mul(nloop, r_array, c_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(realarray_IO), intent(in) ::  r_array(nloop)
      type(charaarray_IO), intent(inout) :: c_array(nloop)
!
      integer(kind = kint) :: ilen_gz, ilength, iloop
!
!
      do iloop = 1, nloop
        ilength =  r_array(iloop)%num * kreal
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(c_array(iloop)%c_IO(ilen_gz))
        call gzip_defleat_once(ilength, r_array(iloop)%r_IO(1),         &
     &      ilen_gz, c_array(iloop)%num, c_array(iloop)%c_IO(1))
      end do
!
      end subroutine defleat_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine defleat_2d_vector_mul(nloop, v_array, c_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(vectarray_IO), intent(in) ::  v_array(nloop)
      type(charaarray_IO), intent(inout) :: c_array(nloop)
!
      integer(kind = kint) :: ilen_gz, ilength, iloop
!
!
      do iloop = 1, nloop
        ilength =  v_array(iloop)%n1 * v_array(iloop)%n2 * kreal
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(c_array(iloop)%c_IO(ilen_gz))
        call gzip_defleat_once(ilength, v_array(iloop)%v_IO(1,1),       &
     &      ilen_gz, c_array(iloop)%num, c_array(iloop)%c_IO(1))
      end do
!
      end subroutine defleat_2d_vector_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleat_int8_vector_mul                                &
     &         (iflag_bin_swap, nloop, c_array, i8_array)
!
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) :: c_array(nloop)
      type(int8array_IO), intent(inout) ::  i8_array(nloop)
!
      integer(kind = kint) :: ilen_gzipped, ilength,iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        ilength = i8_array(iloop)%num * kint_gl
!
        call gzip_infleat_once                                          &
     &     (c_array(iloop)%num, c_array(iloop)%c_IO(1),                 &
     &      ilength, i8_array(iloop)%i8_IO(1), ilen_gzipped)
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, i8_array(iloop)%i8_IO(1))
        end if
        deallocate(c_array(iloop)%c_IO)
      end do
!
      end subroutine infleat_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_int_vector_mul                                 &
     &         (iflag_bin_swap, nloop, c_array, i_array)
!
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) :: c_array(nloop)
      type(intarray_IO), intent(inout) ::  i_array(nloop)
!
      integer(kind = kint) :: ilen_gzipped, ilength,iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        ilength = i_array(iloop)%num * kint
!
        call gzip_infleat_once                                          &
     &     (c_array(iloop)%num, c_array(iloop)%c_IO(1),                 &
     &      ilength, i_array(iloop)%i_IO(1), ilen_gzipped)
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, i_array(iloop)%i_IO(1))
        end if
        deallocate(c_array(iloop)%c_IO)
      end do
!
      end subroutine infleat_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_int2d_vector_mul                               &
     &        (iflag_bin_swap, nloop, c_array, iv_array)
!
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) :: c_array(nloop)
      type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
!
      integer(kind = kint) :: ilen_gzipped, ilength,iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        ilength = iv_array(iloop)%n1*iv_array(iloop)%n2 * kint
!
        call gzip_infleat_once                                          &
     &     (c_array(iloop)%num, c_array(iloop)%c_IO(1),                 &
     &      ilength, iv_array(iloop)%iv_IO(1,1), ilen_gzipped)
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, iv_array(iloop)%iv_IO(1,1))
        end if
        deallocate(c_array(iloop)%c_IO)
      end do
!
      end subroutine infleat_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_1d_vector_mul                                  &
     &         (iflag_bin_swap, nloop, c_array, r_array)
!
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) :: c_array(nloop)
      type(realarray_IO), intent(inout) ::  r_array(nloop)
!
      integer(kind = kint) :: ilen_gzipped, ilength,iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        ilength = r_array(iloop)%num * kreal
!
        call gzip_infleat_once                                          &
     &     (c_array(iloop)%num, c_array(iloop)%c_IO(1),                 &
     &      ilength, r_array(iloop)%r_IO(1), ilen_gzipped)
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, r_array(iloop)%r_IO(1))
        end if
        deallocate(c_array(iloop)%c_IO)
      end do
!
      end subroutine infleat_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine infleat_2d_vector_mul                                  &
     &         (iflag_bin_swap, nloop, c_array, v_array)
!
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) :: c_array(nloop)
      type(vectarray_IO), intent(inout) ::  v_array(nloop)
!
      integer(kind = kint) :: ilen_gzipped, ilength,iloop
!
      integer(kind = kint_gl) :: l8_byte
!
!
      do iloop = 1, nloop
        ilength = v_array(iloop)%n1*v_array(iloop)%n2 * kreal
!
        call gzip_infleat_once                                          &
     &     (c_array(iloop)%num, c_array(iloop)%c_IO(1),                 &
     &      ilength, v_array(iloop)%v_IO(1,1), ilen_gzipped)
!
        if(iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, v_array(iloop)%v_IO(1,1))
        end if
        deallocate(c_array(iloop)%c_IO)
      end do
!
      end subroutine infleat_2d_vector_mul
!
! -----------------------------------------------------------------------
!
      end module defleat_4_merged_arrays
