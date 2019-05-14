!>@file   MPI_sph_gauss_coefs_IO.f90
!!@brief  module MPI_sph_gauss_coefs_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_picked_spectrum_file(file_name, picked)
!!        type(picked_spectrum_data), intent(in) :: picked
!!@endverbatim
!!
      module MPI_sph_gauss_coefs_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
      use t_calypso_mpi_IO_param
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_address
      use t_phys_data
      use t_time_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_sph_gauss_coefs_file                            &
     &         (time_d, sph_params, sph_rj, ipol, rj_fld, gauss)
!
      use set_parallel_file_name
      use picked_sph_spectr_data_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
!
      type(calypso_MPI_IO_params) :: IO_param
      character(len = kchara) :: file_name
!
!
      if(gauss%num_sph_mode .le. 0) return
!
      file_name = add_dat_extension(gauss%file_prefix)
!
      call calypso_mpi_barrier
      call calypso_mpi_append_file_open                                 &
     &   (file_name, nprocs, IO_param%id_rank, IO_param%ioff_gl)
      call calypso_mpi_barrier
!
      if(IO_param%ioff_gl .eq. 0) then
        call write_sph_gauss_header_mpi(IO_param, gauss)
      end if
!
      call write_sph_gauss_coefes_mpi                                   &
     &   (IO_param, time_d, sph_params, sph_rj, ipol, rj_fld, gauss)
!
      call calypso_close_mpi_file(IO_param%id_rank)
!
      end subroutine append_sph_gauss_coefs_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_sph_gauss_header_mpi(IO_param, picked)
!
      use MPI_ascii_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint_gl) :: inum
      integer(kind = kint_gl) :: num
      integer, parameter :: ilen_n = 14
      integer, parameter :: ilen_h = 28+1+16+25+1+19
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = 28+1+16+25+1+19) :: timebuf(1)
      character(len = ilen_n), allocatable :: pickedbuf(:)
!
!
      if(my_rank .eq. 0) then
        write(timebuf(1),'(a28,a1, i16,1pe25.15e3,a1, a19)')            &
     &        'num_spectr, reference_radius', char(10),                 &
     &        picked%num_sph_mode, picked%radius_gl(1), char(10),       &
     &        't_step    time    '
!
        num = 1
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_rank, IO_param%ioff_gl, ilen_h, num, timebuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_h
!
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &         + ilen_n * picked%istack_picked_spec_lc(my_rank)
!
        allocate(pickedbuf(picked%num_sph_mode_lc))
        do inum = 1, num
          write(pickedbuf(inum),'(a14)')   '              '
          write(pickedbuf(inum),'(a)')                                  &
     &                trim(picked%gauss_mode_name_lc(inum))
        end do
!
        call calypso_mpi_seek_wrt_mul_chara(IO_param%id_rank, ioffset,  &
     &      ilen_n, num, pickedbuf)
        deallocate(pickedbuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                + ilen_n * picked%istack_picked_spec_lc(nprocs)
!
      if(my_rank .eq. 0) then
        write(timebuf(1),'(a1)') char(10)
        num = 1
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_rank, IO_param%ioff_gl, ione, num, timebuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ione
!
!
      end subroutine write_sph_gauss_header_mpi
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_sph_gauss_coefes_mpi(IO_param, time_d,           &
     &          sph_params, sph_rj, ipol, rj_fld,  picked)
!
      use MPI_ascii_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
!
      integer(kind = kint) :: inum
!
      integer(kind = kint_gl) :: num
      integer :: ilength
      integer, parameter :: ilen_n = 25
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = 16+25) :: timebuf(1)
      character(len = 25), allocatable :: pickedbuf(:)
      real(kind=kreal), allocatable :: d_rj_out(:)
!
!
!
      ilength = len(picked_gauss_head(time_d%i_time_step, time_d%time))
      if(my_rank .eq. 0) then
        timebuf(1) = picked_gauss_head(time_d%i_time_step, time_d%time)
        num = 1
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_rank, IO_param%ioff_gl, ilength, num, timebuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ilength
!
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &         + ilen_n * picked%istack_picked_spec_lc(my_rank)
!
        allocate(d_rj_out(picked%num_sph_mode_lc))
        allocate(pickedbuf(picked%num_sph_mode_lc))
!
        call gauss_coefficients_4_write                                 &
     &     (sph_params, sph_rj, ipol, rj_fld, picked, d_rj_out)
        do inum = 1, picked%num_sph_mode_lc
          write(pickedbuf(inum),'(1pE25.14e3)') d_rj_out(inum)
        end do
!
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_rank, ioffset, ilen_n, num, pickedbuf)
        deallocate(d_rj_out, pickedbuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                + ilen_n * picked%istack_picked_spec_lc(nprocs)
!
      if(my_rank .eq. 0) then
        write(timebuf(1),'(a1)') char(10)
        num = 1
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_rank, IO_param%ioff_gl, ione, num, timebuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ione
!
!
      end subroutine write_sph_gauss_coefes_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      character(len = 16+25) function picked_gauss_head(i_step, time)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
!
      write(picked_gauss_head,'(i16,1pe25.14e3)') i_step, time
!
      end function  picked_gauss_head
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gauss_coefficients_4_write                             &
     &         (sph_params, sph_rj, ipol, rj_fld, picked, d_rj_out)
!
      use calypso_mpi
      use t_pickup_sph_spectr_data
!
      type(phys_address), intent(in) :: ipol
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      real(kind=kreal), intent(inout)                                   &
     &                 :: d_rj_out(picked%num_sph_mode_lc)
!
      integer(kind = kint) :: inum, l, i
      real(kind = kreal) :: r_ICB, r_CMB, rcmb_to_Re, ricb_to_Rref
      real(kind = kreal) :: r_4_gauss_coefs, a2r_4_gauss
!
!
      r_4_gauss_coefs = picked%radius_gl(1)
      r_ICB = sph_rj%radius_1d_rj_r(sph_params%nlayer_ICB)
      r_CMB = sph_rj%radius_1d_rj_r(sph_params%nlayer_CMB)
      if(r_4_gauss_coefs .ge. r_CMB) then
        a2r_4_gauss = one / (r_4_gauss_coefs**2)
        rcmb_to_Re = r_CMB / r_4_gauss_coefs
!$omp parallel do private(l,i)
        do inum = 1, picked%num_sph_mode_lc
          l = picked%idx_out(inum,1)
          i = picked%idx_out(inum,4)                                    &
     &          + (sph_params%nlayer_CMB-1) * sph_rj%nidx_rj(2)
          d_rj_out(inum) = rj_fld%d_fld(i,ipol%i_magne)                 &
     &                    * dble(l) * rcmb_to_Re**l *a2r_4_gauss
        end do
!$omp end parallel do
!
      else if(r_4_gauss_coefs .le. r_ICB) then
        a2r_4_gauss = one / (r_ICB**2)
        ricb_to_Rref = r_4_gauss_coefs / r_ICB
!$omp parallel do private(l,i)
        do inum = 1, picked%num_sph_mode_lc
          l = picked%idx_out(inum,1)
          i = picked%idx_out(inum,4)                                    &
     &          + (sph_params%nlayer_ICB-1) * sph_rj%nidx_rj(2)
          d_rj_out(inum) = - rj_fld%d_fld(i,ipol%i_magne)               &
     &                  * dble(l+1) * a2r_4_gauss * ricb_to_Rref**(l-1)
        end do
!$omp end parallel do
      end if
!
      end subroutine gauss_coefficients_4_write
!
! -----------------------------------------------------------------------
!
      end module MPI_sph_gauss_coefs_IO
