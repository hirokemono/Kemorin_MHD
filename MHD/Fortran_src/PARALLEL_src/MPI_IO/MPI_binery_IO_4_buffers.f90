!>@file  MPI_binery_IO_4_buffers.f90
!!       module MPI_binery_IO_4_buffers
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief gzip compression and expansion for MPI-IO routines
!!
!!@verbatim
!!      subroutine mpi_write_chara_array_mul(id_file, nprocs_in, nloop, &
!!     &          ioff_gl, istack_merged, c_array)
!!      subroutine mpi_write_i8_vect_mul_b(id_file, nprocs_in, nloop,   &
!!     &          ioff_gl, istack_merged, i8_array)
!!      subroutine mpi_write_intvect_mul_b(id_file, nprocs_in, nloop,   &
!!     &          ioff_gl, istack_merged, i_array)
!!      subroutine mpi_write_i2dvect_mul_b(id_file, nprocs_in, nloop,   &
!!     &          ioff_gl, istack_merged, iv_array)
!!      subroutine mpi_write_realvect_mul_b(id_file, nprocs_in, nloop,  &
!!     &          ioff_gl, istack_merged, r_array)
!!      subroutine mpi_write_r2dvect_mul_b(id_file, nprocs_in, nloop,   &
!!     &          ioff_gl, istack_merged, v_array)
!!        type(charaarray_IO), intent(in) :: c_array(nloop)
!!        type(int8array_IO), intent(in) ::  i8_array(nloop)
!!        type(intarray_IO), intent(in) ::  i_array(nloop)
!!        type(ivecarray_IO), intent(in) ::  iv_array(nloop)
!!        type(realarray_IO), intent(in) ::  r_array(nloop)
!!        type(vectarray_IO), intent(in) ::  v_array(nloop)
!!
!!      subroutine mpi_read_chara_array_mul(id_file, nprocs_in, nloop,  &
!!     &          ioff_gl, istack_merged, c_array)
!!      subroutine mpi_read_i8_vect_mul_b(id_file, iflag_bin_swap,      &
!!     &          nprocs_in, nloop, ioff_gl, istack_merged, i8_array)
!!      subroutine mpi_read_intvect_mul_b(id_file, iflag_bin_swap,      &
!!     &          nprocs_in, nloop, ioff_gl, istack_merged, i_array)
!!      subroutine mpi_read_i2dvect_mul_b(id_file, iflag_bin_swap,      &
!!     &          nprocs_in, nloop, ioff_gl, istack_merged, iv_array)
!!      subroutine mpi_read_realvect_mul_b(id_file, iflag_bin_swap,     &
!!     &          nprocs_in, nloop, ioff_gl, istack_merged, r_array)
!!      subroutine mpi_read_r2dvect_mul_b(id_file, iflag_bin_swap,      &
!!     &          nprocs_in, nloop, ioff_gl, istack_merged, v_array)
!!        type(charaarray_IO), intent(inout) :: c_array(nloop)
!!        type(int8array_IO), intent(inout) ::  i8_array(nloop)
!!        type(intarray_IO), intent(inout) ::   i_array(nloop)
!!        type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
!!        type(realarray_IO), intent(inout) ::  r_array(nloop)
!!        type(vectarray_IO), intent(inout) ::  v_array(nloop)
!!@endverbatim
!
      module MPI_binery_IO_4_buffers
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_calypso_mpi_IO_param
      use m_calypso_mpi_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_chara_array_mul(id_file, nprocs_in, nloop,   &
     &          ioff_gl, istack_merged, c_array)
!
      use m_calypso_mpi_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      type(charaarray_IO), intent(in) ::  c_array(nloop)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
      integer(kind = kint) :: num32
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        if(c_array(iloop)%num .gt. 0) then
          ioffset = ioff_gl + istack_merged(id_rank)
          num32 = c_array(iloop)%num
          call calypso_mpi_seek_write_chara(id_file, ioffset,           &
     &        num32, c_array(iloop)%c_IO(1))
        end if
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs)
!
      end subroutine mpi_write_chara_array_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_i8_vect_mul_b(id_file, nprocs_in, nloop,     &
     &          ioff_gl, istack_merged, i8_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      type(int8array_IO), intent(in) ::  i8_array(nloop)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
      integer(kind = kint) :: num32
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + istack_merged(id_rank)
        num32 = i8_array(iloop)%num
        call calypso_mpi_seek_write_int8(id_file, ioffset,              &
     &      num32, i8_array(iloop)%i8_IO)
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs_in)
!
      end subroutine mpi_write_i8_vect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_intvect_mul_b(id_file, nprocs_in, nloop,     &
     &          ioff_gl, istack_merged, i_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      type(intarray_IO), intent(in) ::  i_array(nloop)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
      integer(kind = kint) :: num32
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + istack_merged(id_rank)
        num32 = i_array(iloop)%num
        call calypso_mpi_seek_write_int                                 &
     &    (id_file, ioffset, num32, i_array(iloop)%i_IO)
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs_in)
!
      end subroutine mpi_write_intvect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_i2dvect_mul_b(id_file, nprocs_in, nloop,     &
     &          ioff_gl, istack_merged, iv_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      type(ivecarray_IO), intent(in) ::  iv_array(nloop)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank, n_2d
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + istack_merged(id_rank)
        n_2d = iv_array(iloop)%n1 * iv_array(iloop)%n2
        call calypso_mpi_seek_write_int                                 &
     &    (id_file, ioffset, n_2d, iv_array(iloop)%iv_IO)
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs_in)
!
      end subroutine mpi_write_i2dvect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_realvect_mul_b(id_file, nprocs_in, nloop,    &
     &          ioff_gl, istack_merged, r_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      type(realarray_IO), intent(in) ::  r_array(nloop)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
      integer(kind = kint) :: num32
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + istack_merged(id_rank)
        num32 = r_array(iloop)%num
        call calypso_mpi_seek_write_real                                &
     &    (id_file, ioffset, num32, r_array(iloop)%r_IO)
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs_in)
!
      end subroutine mpi_write_realvect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_r2dvect_mul_b(id_file, nprocs_in, nloop,     &
     &          ioff_gl, istack_merged, v_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      type(vectarray_IO), intent(in) ::  v_array(nloop)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank, n_2d
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + istack_merged(id_rank)
        n_2d = v_array(iloop)%n1 * v_array(iloop)%n2
        call calypso_mpi_seek_write_real                                &
     &    (id_file, ioffset, n_2d, v_array(iloop)%v_IO)
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs_in)
!
      end subroutine mpi_write_r2dvect_mul_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_chara_array_mul(id_file, nprocs_in, nloop,    &
     &          ioff_gl, istack_merged, c_array)
!
      use m_calypso_mpi_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(charaarray_IO), intent(inout) ::  c_array(nloop)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
!
        ioffset = ioff_gl + istack_merged(id_rank)
        c_array(iloop)%num = int(istack_merged(id_rank+1)               &
     &                         - istack_merged(id_rank))
!
        allocate(c_array(iloop)%c_IO(c_array(iloop)%num))
        call calypso_mpi_seek_read_mul_chara(id_file, ioffset,          &
     &      ione, c_array(iloop)%num, c_array(iloop)%c_IO(1))
      end do
      ioff_gl = ioff_gl + istack_merged(nprocs_in)
!
      end subroutine mpi_read_chara_array_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_i8_vect_mul_b(id_file, iflag_bin_swap,        &
     &          nprocs_in, nloop, ioff_gl, istack_merged, i8_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(int8array_IO), intent(inout) ::  i8_array(nloop)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
      integer(kind = kint) :: num32
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + kint_gl * istack_merged(id_rank)
        num32 = i8_array(iloop)%num
        call calypso_mpi_seek_read_int8(id_file, iflag_bin_swap,        &
     &      ioffset, num32, i8_array(iloop)%i8_IO)
      end do
      ioff_gl = ioff_gl + kreal * istack_merged(nprocs_in)
!
      end subroutine mpi_read_i8_vect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_intvect_mul_b(id_file, iflag_bin_swap,        &
     &          nprocs_in, nloop, ioff_gl, istack_merged, i_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(intarray_IO), intent(inout) ::  i_array(nloop)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
      integer(kind = kint) :: num32
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + kint * istack_merged(id_rank)
        num32 = i_array(iloop)%num
        call calypso_mpi_seek_read_int(id_file, iflag_bin_swap,         &
     &      ioffset, num32, i_array(iloop)%i_IO)
      end do
      ioff_gl = ioff_gl + kreal * istack_merged(nprocs_in)
!
      end subroutine mpi_read_intvect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_i2dvect_mul_b(id_file, iflag_bin_swap,        &
     &          nprocs_in, nloop, ioff_gl, istack_merged, iv_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank, n_2d
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + kint * istack_merged(id_rank)
        n_2d = iv_array(iloop)%n1 * iv_array(iloop)%n2
        call calypso_mpi_seek_read_int(id_file, iflag_bin_swap,         &
     &      ioffset, n_2d, iv_array(iloop)%iv_IO)
      end do
      ioff_gl = ioff_gl + kreal * istack_merged(nprocs_in)
!
      end subroutine mpi_read_i2dvect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_realvect_mul_b(id_file, iflag_bin_swap,       &
     &          nprocs_in, nloop, ioff_gl, istack_merged, r_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(realarray_IO), intent(inout) ::  r_array(nloop)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank
      integer(kind = kint) :: num32
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + kreal * istack_merged(id_rank)
        num32 = r_array(iloop)%num
        call calypso_mpi_seek_read_real(id_file, iflag_bin_swap,        &
     &      ioffset, num32, r_array(iloop)%r_IO)
      end do
      ioff_gl = ioff_gl + kreal * istack_merged(nprocs_in)
!
      end subroutine mpi_read_realvect_mul_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_r2dvect_mul_b(id_file, iflag_bin_swap,        &
     &          nprocs_in, nloop, ioff_gl, istack_merged, v_array)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: nloop, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(vectarray_IO), intent(inout) ::  v_array(nloop)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iloop, id_rank, n_2d
!
!
      do iloop = 1, nloop
        id_rank = rank_in_multi_domain(iloop)
        ioffset = ioff_gl + kreal * istack_merged(id_rank)
        n_2d = v_array(iloop)%n1 * v_array(iloop)%n2
        call calypso_mpi_seek_read_real(id_file, iflag_bin_swap,        &
     &      ioffset, n_2d, v_array(iloop)%v_IO)
      end do
      ioff_gl = ioff_gl + kreal * istack_merged(nprocs_in)
!
      end subroutine mpi_read_r2dvect_mul_b
!
! -----------------------------------------------------------------------
!
      end module MPI_binery_IO_4_buffers
