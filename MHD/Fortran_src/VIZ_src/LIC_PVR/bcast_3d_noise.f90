!>@file   bcast_3d_noise.f90
!!@brief  module bcast_3d_noise
!!
!!@author Y. Liao and H. Matsui
!!@date Programmed by Y. Liao in Apr. 2018
!!      Modified by H. Matsui in Apr. 2020
!
!> @brief Broadcast 3D noise data for LIC
!!
!!@verbatim
!!      subroutine bcast_3d_cube_noise(nze)
!!        type(noise_cube), intent(inout) :: nze
!!@endverbatim
!
      module bcast_3d_noise
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_3d_noise
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_3d_cube_noise(nze)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_int8
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(noise_cube), intent(inout) :: nze
!
      integer(kind = kint_gl) :: n3_cube
!
!
      call calypso_mpi_bcast_one_int(nze%iflag_noise_type, 0)
      call calypso_mpi_bcast_character                                  &
     &   (nze%noise_file_name, cast_long(kchara), 0)
!
      call calypso_mpi_bcast_real(nze%size_cube, cast_long(ithree), 0)
      call calypso_mpi_bcast_real(nze%asize_cube, cast_long(ithree), 0)
!
      call calypso_mpi_bcast_int(nze%nidx_xyz, cast_long(ithree), 0)
      call calypso_mpi_bcast_one_int(nze%i_stepsize, 0)
      call calypso_mpi_bcast_one_int8(nze%n_cube, 0)
!
      if(my_rank .ne. 0) call alloc_3d_cube_noise(nze)
      n3_cube = 3 * nze%n_cube
      call calypso_mpi_bcast_real(nze%rnoise(1), nze%n_cube, 0)
      call calypso_mpi_bcast_real(nze%rnoise_grad(1,1), n3_cube, 0)
!
      end subroutine bcast_3d_cube_noise
!
!  ---------------------------------------------------------------------
!
      end module bcast_3d_noise
