!>@file   m_sph_uli_lengh_scale.f90
!!        module m_sph_uli_lengh_scale
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Obtain lengh scale from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sph_uli_lengh_scale_by_spectr                        &
!!     &         (input_header, iflag_vol_ave, spec_evo_p, sph_IN)
!!        type(sph_spectr_file_param), intent(in) :: spec_evo_p
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module m_sph_uli_lengh_scale
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms_l =    35
      integer(kind = kint), parameter :: id_file_lscale =   44
!
      type(read_sph_spectr_data), save :: sph_OUT1
!
      real(kind = kreal), allocatable :: total_msq(:,:)
      real(kind = kreal), allocatable :: spec_times_l(:,:)
!
!
      private :: sph_OUT1, total_msq, spec_times_l
      private :: id_file_rms_l, id_file_lscale
      private :: cal_uli_length_scale_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_uli_lengh_scale_by_spectr                          &
     &         (input_header, iflag_vol_ave, spec_evo_p, sph_IN)
!
      use t_ctl_param_sph_series_util
      use sph_mean_square_IO_select
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: input_header
        integer(kind = kint), intent(in) :: iflag_vol_ave
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!
      file_name = add_dat_extension(input_header)
      open(id_file_rms_l, file=file_name)
      call select_input_sph_pwr_head(id_file_rms_l,                     &
     &    spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      write(fname_tmp, '(a7,a)') 'lscale_', trim(input_header)
      file_name = add_dat_extension(fname_tmp)
      open(id_file_lscale, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (id_file_lscale, iflag_vol_ave, sph_OUT1)
!
      call allocate_lscale_espec_data(sph_IN)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        call select_input_sph_pwr_data(id_file_rms_l,                   &
     &      spec_evo_p%iflag_old_fmt, iflag_vol_ave, sph_IN, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          call copy_read_ene_step_data(sph_IN, sph_OUT1)
          call cal_uli_length_scale_sph                                 &
     &       (sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,     &
     &        sph_IN%spectr_IO, total_msq, spec_times_l,                &
     &        sph_OUT1%spectr_IO)
          icou = icou + 1
!
          call select_output_sph_series_data                            &
     &       (id_file_lscale, iflag_vol_ave, sph_OUT1)
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. spec_evo_p%end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms_l)
      close(id_file_lscale)
!
!
      call deallocate_lscale_espec_data
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
!
      end subroutine sph_uli_lengh_scale_by_spectr
!
!   --------------------------------------------------------------------
!
      subroutine cal_uli_length_scale_sph(nri_sph, ltr_sph, ncomp,      &
     &          spectr_l, total_msq, spec_times_l, scale_uli)
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: total_msq(ncomp,nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                    :: spec_times_l(ncomp,nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                    :: scale_uli(ncomp,0:0,nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp
          total_msq(nd,kr) =    spectr_l(nd,0,kr)
          spec_times_l(nd,kr) = 0.0d0
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      do lth = 1, ltr_sph
!$omp parallel private(kr,nd)
        do kr = 1, nri_sph
!$omp do
          do nd = 1, ncomp
            total_msq(nd,kr) = total_msq(nd,kr)                         &
     &                           + spectr_l(nd,lth,kr)
            spec_times_l(nd,kr) = spec_times_l(nd,kr)                   &
     &                           + spectr_l(nd,lth,kr) * dble(lth)
          end do
!$omp end do nowait
        end do
!$omp end parallel
      end do
!
!$omp parallel private(kr,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp
          if(total_msq(nd,kr) .le. 0.0d0) then
            scale_uli(nd,0,kr) = 0.0d0
          else
            scale_uli(nd,0,kr) = spec_times_l(nd,kr) / total_msq(nd,kr)
          end if
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_uli_length_scale_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_lscale_espec_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      allocate( total_msq(sph_IN%ntot_sph_spec,sph_IN%nri_sph) )
      allocate( spec_times_l(sph_IN%ntot_sph_spec,sph_IN%nri_sph) )
      total_msq = 0.0d0
      spec_times_l = 0.0d0
!
      end subroutine allocate_lscale_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_lscale_espec_data
!
      deallocate(total_msq, spec_times_l)
!
      end subroutine deallocate_lscale_espec_data
!
!   --------------------------------------------------------------------
!
      end module m_sph_uli_lengh_scale
