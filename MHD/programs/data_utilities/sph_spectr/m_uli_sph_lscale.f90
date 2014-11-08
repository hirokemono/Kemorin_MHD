!>@file   m_uli_sph_lscale.f90
!!        module m_uli_sph_lscale
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine allocate_lscale_espec_data
!!      subroutine deallocate_lscale_espec_data
!!
!!      subroutine open_uli_sph_lscale
!!      subroutine close_uli_sph_lscale
!!      subroutine output_uli_sph_lscale(istep)
!!
!!      subroutine cal_uli_length_scale_sph
!!@endverbatim
!
      module m_uli_sph_lscale
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      real(kind = kreal), allocatable :: total_msq(:,:)
      real(kind = kreal), allocatable :: scale_uli(:,:)
      real(kind = kreal), allocatable :: spec_times_l(:,:)
!
!     data file ID
!
      integer(kind = kint), parameter :: id_lscale =    131
!
      private :: write_lscale_sph_head
      private :: write_volume_sph_lscale, write_layer_sph_lscale
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_lscale_espec_data
!
      use m_sph_ene_spectra
!
      allocate( total_msq(ncomp_sph_spec,nri_sph) )
      allocate( scale_uli(ncomp_sph_spec,nri_sph) )
      allocate( spec_times_l(ncomp_sph_spec,nri_sph) )
!
      total_msq =  0.0d0
      scale_uli =  0.0d0
      spec_times_l =  0.0d0
!
      end subroutine allocate_lscale_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_lscale_espec_data
!
      deallocate(total_msq, scale_uli, spec_times_l)
!
      end subroutine deallocate_lscale_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine open_uli_sph_lscale
!
      use m_sph_ene_spectra
!
      character(len = kchara) :: fname_max_rms_l
!
!
      write(fname_max_rms_l, '(a8,a)')                                  &
     &        'l_scale_', trim(fname_org_rms_l)
!
      open(id_lscale, file=fname_max_rms_l)
!
      call write_lscale_sph_head
!
      end subroutine open_uli_sph_lscale
!
!   --------------------------------------------------------------------
!
      subroutine close_uli_sph_lscale
!
!
      close(id_lscale)
!
      end subroutine close_uli_sph_lscale
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_uli_sph_lscale(istep)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: istep
!
!
      if(iflag_sph_ene_file .eq. 1) then
        call write_volume_sph_lscale(nri_sph, ncomp_sph_spec,           &
     &      scale_uli, istep)
      else
        call write_layer_sph_lscale(nri_sph, ncomp_sph_spec,            &
     &      scale_uli, istep)
      end if
!
      end subroutine output_uli_sph_lscale
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_lscale_sph_head
!
      use m_sph_ene_spectra
      use write_field_labels
!
      integer(kind = kint) :: num
!
!
      num = ncomp_sph_spec + num_time_labels
      write(ene_sph_spec_name(num_time_labels),'(a)') 'degree'
      call write_multi_labels(id_lscale, num, ene_sph_spec_name)
!
      write(id_lscale,* )
!
      end subroutine write_lscale_sph_head
!
!   --------------------------------------------------------------------
!
      subroutine write_volume_sph_lscale(nri, ncomp, spec_l, istep)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: nri, ncomp, istep
      real(kind = kreal), intent(inout) :: spec_l(ncomp,nri)
!
!
      write(id_lscale,'(i15,1pE25.15e3,i15,1p255E25.15e3)')             &
     &         istep, time_sph, izero, spec_l(1:ncomp,1)
!
      end subroutine write_volume_sph_lscale
!
!   --------------------------------------------------------------------
!
      subroutine write_layer_sph_lscale(nri, ncomp, spec_l, istep)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: nri, ncomp, istep
      real(kind = kreal), intent(inout) :: spec_l(ncomp,nri)
!
      integer(kind = kint) :: kr
!
!
      do kr = 1, nri
        write(id_lscale,'(i15,1pE25.15e3,2i10,1p255E25.15e3)')          &
     &     istep, time_sph, kr_sph(kr), izero, spec_l(1:ncomp,kr)
      end do
!
      end subroutine write_layer_sph_lscale
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine cal_uli_length_scale_sph
!
      use m_sph_ene_spectra
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
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
          do nd = 1, ncomp_sph_spec
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
        do nd = 1, ncomp_sph_spec
          scale_uli(nd,kr) = spec_times_l(nd,kr) / total_msq(nd,kr)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_uli_length_scale_sph
!
!   --------------------------------------------------------------------
!
      end module m_uli_sph_lscale
