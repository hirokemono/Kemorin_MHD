!>@file   m_part_sum_sph_ene_spectr.f90
!!        module m_part_sum_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief TAke sum of a part of spectrum data
!!
!!@verbatim
!!      subroutine sph_part_pwr_spectr_sum                              &
!!     &         (fname_org, start_time, end_time, lst, led, sph_IN)
!!@endverbatim
!
      module m_part_sum_sph_ene_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms =      34
      integer(kind = kint), parameter :: id_file_rms_l =    44
!
      type(read_sph_spectr_data), save :: sph_OUT1
!
      private :: sph_OUT1
      private :: id_file_rms, id_file_rms_l
      private :: copy_read_ene_params_4_sum, part_sum_ene_spectr
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_part_pwr_spectr_sum                                &
     &         (input_header, start_time, end_time, lst, led, sph_IN)
!
      use sph_mean_square_IO_select
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: lst, led
      character(len = kchara), intent(in) :: input_header
      real(kind = kreal), intent(in) :: start_time, end_time
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!
      sph_IN%iflag_spectr = 1
      file_name = add_dat_extension(input_header)
      open(id_file_rms_l, file=file_name)
      call select_input_sph_pwr_head(id_file_rms_l, sph_IN)
!
      sph_OUT1%iflag_spectr =  0
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      write(fname_tmp, '(a5,a)') 'part_', trim(input_header)
      file_name = add_int_suffix(izero, fname_tmp)
      fname_tmp = add_int_suffix(izero, file_name)
      file_name = add_dat_extension(fname_tmp)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head(id_file_rms, sph_OUT1)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        ierr = select_input_sph_pwr_data(id_file_rms_l, sph_IN)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          call copy_read_ene_step_data(sph_IN, sph_OUT1)
          call part_sum_ene_spectr(lst, led,                            &
     &        sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,     &
     &        sph_IN%spectr_IO, sph_OUT1%spectr_IO)
          icou = icou + 1
!
          call select_output_sph_pwr_data(id_file_rms, sph_OUT1)
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms_l)
      close(id_file_rms)
!
!
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
!
      end subroutine sph_part_pwr_spectr_sum
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine part_sum_ene_spectr(lst, led, nri_sph, ltr_sph, ncomp, &
     &          spectr_l, sum_spec)
!
      integer(kind = kint), intent(in) :: lst, led
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: sum_spec(ncomp,0:0,nri_sph)
!
      integer(kind = kint) :: kr, lth
!
!
!$omp parallel workshare
        sum_spec(1:ncomp,0:0,1:nri_sph) = 0.0d0
!$omp end parallel workshare
!
      do kr = 1, nri_sph
        do lth = lst, led
!$omp parallel workshare
            sum_spec(1:ncomp,0,kr) = sum_spec(1:ncomp,0,kr)             &
     &                              + spectr_l(1:ncomp,lth,kr)
!$omp end parallel workshare
        end do
      end do
!
      end subroutine part_sum_ene_spectr
!
!   --------------------------------------------------------------------
!
      end module m_part_sum_sph_ene_spectr
