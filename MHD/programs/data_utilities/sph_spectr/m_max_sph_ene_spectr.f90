!>@file   m_max_sph_ene_spectr.f90
!!        module m_max_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine allocate_max_sph_espec_data
!!      subroutine deallocate_max_sph_espec_data
!!
!!      subroutine open_maxmode_spec_data
!!      subroutine close_maxmode_spec_data
!!      subroutine output_dominant_scale_sph(istep)
!!
!!      subroutine find_dominant_scale_sph
!!@endverbatim
!
      module m_max_sph_ene_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      real(kind = kreal), allocatable :: max_spec_l(:,:)
      real(kind = kreal), allocatable :: max_spec_m(:,:)
      real(kind = kreal), allocatable :: max_spec_lm(:,:)
!
      real(kind = kreal), allocatable :: max_degree(:,:)
      real(kind = kreal), allocatable :: max_order(:,:)
      real(kind = kreal), allocatable :: max_diff_lm(:,:)
!
      real(kind = kreal), allocatable :: max_count_l(:,:,:)
      real(kind = kreal), allocatable :: max_count_m(:,:,:)
      real(kind = kreal), allocatable :: max_count_lm(:,:,:)
!
!     data file ID
!
      integer(kind = kint), parameter :: id_max_rms_l =    131
      integer(kind = kint), parameter :: id_max_rms_m =    132
      integer(kind = kint), parameter :: id_max_rms_lm =   133
!
      private :: write_maxmode_ene_sph_head
      private :: write_max_vol_sph_data, write_max_layer_sph_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_max_sph_espec_data
!
      use m_sph_ene_spectra
!
      allocate( max_spec_l(ncomp_sph_spec,nri_sph) )
      allocate( max_spec_m(ncomp_sph_spec,nri_sph) )
      allocate( max_spec_lm(ncomp_sph_spec,nri_sph) )
!
      allocate( max_degree(ncomp_sph_spec,nri_sph) )
      allocate( max_order(ncomp_sph_spec,nri_sph) )
      allocate( max_diff_lm(ncomp_sph_spec,nri_sph) )
!
      allocate( max_count_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( max_count_m(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( max_count_lm(ncomp_sph_spec,0:ltr_sph,nri_sph) )
!
      max_spec_l =  0.0d0
      max_spec_m =  0.0d0
      max_spec_lm = 0.0d0
!
      max_degree =  0.0d0
      max_order =   0.0d0
      max_diff_lm = 0.0d0
!
      max_count_l =  0.0d0
      max_count_m =  0.0d0
      max_count_lm = 0.0d0
!
      end subroutine allocate_max_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_max_sph_espec_data
!
      deallocate(max_spec_l, max_spec_m, max_spec_lm)
      deallocate(max_degree, max_order)
      deallocate(max_diff_lm)
!
      end subroutine deallocate_max_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine open_maxmode_spec_data
!
      use m_sph_ene_spectra
!
      character(len = kchara) :: fname_max_rms_l
      character(len = kchara) :: fname_max_rms_m
      character(len = kchara) :: fname_max_rms_lm
!
!
      write(fname_max_rms_l, '(a8,a)')                                  &
     &        'maxmode_', trim(fname_org_rms_l)
      write(fname_max_rms_m, '(a8,a)')                                  &
     &        'maxmode_', trim(fname_org_rms_m)
      write(fname_max_rms_lm,'(a8,a)')                                  &
     &        'maxmode_', trim(fname_org_rms_lm)
!
      open(id_max_rms_l, file=fname_max_rms_l)
      open(id_max_rms_m, file=fname_max_rms_m)
      open(id_max_rms_lm,file=fname_max_rms_lm)
!
      call write_maxmode_ene_sph_head
!
      end subroutine open_maxmode_spec_data
!
!   --------------------------------------------------------------------
!
      subroutine close_maxmode_spec_data
!
!
      close(id_max_rms_l)
      close(id_max_rms_m)
      close(id_max_rms_lm)
!
      end subroutine close_maxmode_spec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_dominant_scale_sph(istep)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: istep
!
!
      if(iflag_sph_ene_file .eq. 1) then
        call write_max_vol_sph_data(nri_sph, ncomp_sph_spec,            &
     &      max_degree, max_order, max_diff_lm, istep)
      else
        call write_max_layer_sph_data(nri_sph,                          &
     &      ncomp_sph_spec, max_degree, max_order, max_diff_lm, istep)
      end if
!
      end subroutine output_dominant_scale_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_maxmode_ene_sph_head
!
      use m_sph_ene_spectra
      use write_field_labels
!
      integer(kind = kint) :: num
!
!
      num = ncomp_sph_spec + num_time_labels
      write(ene_sph_spec_name(num_time_labels),'(a)') 'degree'
      call write_multi_labels(id_max_rms_l, num, ene_sph_spec_name)
!
      write(ene_sph_spec_name(num_time_labels),'(a)') 'order'
      call write_multi_labels(id_max_rms_m, num, ene_sph_spec_name)
!
      write(ene_sph_spec_name(num_time_labels),'(a)')                   &
     &                                           'diff_deg_order'
      call write_multi_labels(id_max_rms_lm, num, ene_sph_spec_name)
!
      write(id_max_rms_l,* )
      write(id_max_rms_m,* )
      write(id_max_rms_lm,*)
!
      end subroutine write_maxmode_ene_sph_head
!
!   --------------------------------------------------------------------
!
      subroutine write_max_vol_sph_data(nri, ncomp,                     &
     &          spec_l, spec_m, spec_lm, istep)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: nri, ncomp, istep
      real(kind = kreal), intent(inout) :: spec_l(ncomp,nri)
      real(kind = kreal), intent(inout) :: spec_m(ncomp,nri)
      real(kind = kreal), intent(inout) :: spec_lm(ncomp,nri)
!
!
      write(id_max_rms_l,'(i16,1pE25.15e3,i16,1p255E25.15e3)')          &
     &         istep, time_sph, izero, spec_l(1:ncomp,1)
      write(id_max_rms_m,'(i16,1pE25.15e3,i16,1p255E25.15e3)')          &
     &         istep, time_sph, izero, spec_m(1:ncomp,1)
      write(id_max_rms_lm,'(i16,1pE25.15e3,i16,1p255E25.15e3)')         &
     &         istep, time_sph, izero, spec_lm(1:ncomp,1)
!
      end subroutine write_max_vol_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_max_layer_sph_data(nri, ncomp,                   &
     &          spec_l, spec_m, spec_lm, istep)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: nri, ncomp, istep
      real(kind = kreal), intent(inout) :: spec_l(ncomp,nri)
      real(kind = kreal), intent(inout) :: spec_m(ncomp,nri)
      real(kind = kreal), intent(inout) :: spec_lm(ncomp,nri)
!
      integer(kind = kint) :: kr
!
!
      do kr = 1, nri
        write(id_max_rms_l,'(i16,1pE25.15e3,2i16,1p255E25.15e3)')       &
     &     istep, time_sph, kr_sph(kr), izero, spec_l(1:ncomp,kr)
        write(id_max_rms_m,'(i16,1pE25.15e3,2i16,1p255E25.15e3)')       &
     &     istep, time_sph, kr_sph(kr), izero, spec_m(1:ncomp,kr)
        write(id_max_rms_lm,'(i16,1pE25.15e3,2i16,1p255E25.15e3)')      &
     &     istep, time_sph, kr_sph(kr), izero, spec_lm(1:ncomp,kr)
      end do
!
      end subroutine write_max_layer_sph_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine find_dominant_scale_sph
!
      use m_sph_ene_spectra
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
          max_spec_l(nd,kr) = spectr_l(nd,0,kr)
          max_degree(nd,kr) = 0
          max_spec_m(nd,kr) = spectr_m(nd,0,kr)
          max_order(nd,kr) = 0
          max_spec_lm(nd,kr) = spectr_lm(nd,0,kr)
          max_diff_lm(nd,kr) = 0
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
            if(spectr_l(nd,lth,kr) .gt. max_spec_l(nd,kr)) then
              max_spec_l(nd,kr) = spectr_l(nd,lth,kr)
              max_degree(nd,kr) = lth
            end if
            if(spectr_m(nd,lth,kr) .gt. max_spec_m(nd,kr)) then
              max_spec_m(nd,kr) = spectr_m(nd,lth,kr)
              max_order(nd,kr) = lth
            end if
            if(spectr_lm(nd,lth,kr) .gt. max_spec_lm(nd,kr)) then
              max_spec_lm(nd,kr) = spectr_lm(nd,lth,kr)
              max_diff_lm(nd,kr) = lth
            end if
          end do
!$omp end do nowait
        end do
!$omp end parallel
      end do
!
      end subroutine find_dominant_scale_sph
!
!   --------------------------------------------------------------------
!
      end module m_max_sph_ene_spectr
