!>@file   m_tave_sph_ene_spectr.f90
!!        module m_tave_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine allocate_tave_sph_data
!!      subroutine allocate_tave_sph_espec_data
!!      subroutine deallocate_tave_sph_data
!!      subroutine deallocate_tave_sph_espec_data
!!
!!      subroutine reset_tave_sph_espec_data
!!      subroutine reset_tsigma_sph_espec_data
!!
!!      subroutine open_tave_ene_spec_data
!!      subroutine open_tsigma_ene_spec_data
!!
!!      subroutine write_tave_vol_sph_data                              &
!!     &         (file_name, spectr_name, i_step, time, ncomp, spec)
!!      subroutine write_tave_vol_spectr_data(file_name, spectr_name,   &
!!     &          i_step, time, ltr, ncomp, spec_l)
!!
!!      subroutine write_tave_layer_sph_data(file_name, spectr_name,    &
!!     &          i_step, time, nri, ncomp, spec)
!!      subroutine write_tave_layer_spectr_data(file_name, spectr_name, &
!!     &          i_step, time, nri, ltr, ncomp, spec_l)
!!@endverbatim
!
      module m_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      real(kind = kreal), allocatable :: ave_spec_t(:,:)
      real(kind = kreal), allocatable :: ave_spec_l(:,:,:)
!
      real(kind = kreal), allocatable :: sigma_spec_t(:,:)
      real(kind = kreal), allocatable :: sigma_spec_l(:,:,:)
!
      integer(kind = kint), parameter :: id_tave_rms =      34
      private :: id_tave_rms
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_tave_sph_data
!
      use m_sph_ene_spectra
!
      allocate( ave_spec_t(ncomp_sph_spec,nri_sph) )
      allocate( sigma_spec_t(ncomp_sph_spec,nri_sph) )
!
      ave_spec_t =  0.0d0
      sigma_spec_t =  0.0d0
!
      end subroutine allocate_tave_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine allocate_tave_sph_espec_data
!
      use m_sph_ene_spectra
!
      allocate( ave_spec_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( sigma_spec_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
!
      ave_spec_l =  0.0d0
      sigma_spec_l =  0.0d0
!
      end subroutine allocate_tave_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_tave_sph_data
!
      deallocate(ave_spec_t, sigma_spec_t)
!
      end subroutine deallocate_tave_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_tave_sph_espec_data
!
      deallocate(ave_spec_l, sigma_spec_l)
!
      end subroutine deallocate_tave_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_tave_vol_sph_data                                &
     &         (file_name, spectr_name, i_step, time, ncomp, spec)
!
      use m_sph_ene_spectra
      use write_field_labels
!
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in)                               &
     &       :: spectr_name(ncomp_sph_spec+num_time_labels)
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: spec(ncomp)
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: num
!
!
      num = ncomp + num_time_labels
!
      open(id_tave_rms,   file=file_name)
      call write_multi_labels(id_tave_rms, num, spectr_name)
      write(id_tave_rms,*)
      write(id_tave_rms,1000) i_step, time, izero, spec(1:ncomp)
      close(id_tave_rms)
!
 1000 format(i16,1pE25.15e3,i16,1p255E25.15e3)
!
      end subroutine write_tave_vol_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_tave_vol_spectr_data(file_name, spectr_name,     &
     &          i_step, time, ltr, ncomp, spec_l)
!
      use m_sph_ene_spectra
      use write_field_labels
!
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in)                               &
     &       :: spectr_name(ncomp_sph_spec+num_time_labels)
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(in) :: ltr, ncomp
      real(kind = kreal), intent(in) :: spec_l(ncomp,0:ltr)
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: lth, num
!
!
      num = ncomp + num_time_labels
!
      open(id_tave_rms,file=file_name)
      call write_multi_labels(id_tave_rms, num, spectr_name)
      write(id_tave_rms,*)
      do lth = 0, ltr
        write(id_tave_rms,1000)                                         &
     &         i_step, time, lth, spec_l(1:ncomp,lth)
      end do
      close(id_tave_rms)
!
 1000 format(i16,1pE25.15e3,i16,1p255E25.15e3)
!
      end subroutine write_tave_vol_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_tave_layer_sph_data(file_name, spectr_name,      &
     &          i_step, time, nri, ncomp, spec)
!
      use m_sph_ene_spectra
      use write_field_labels
!
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in)                               &
     &       :: spectr_name(ncomp_sph_spec+num_time_labels)
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(in) :: nri, ncomp
      real(kind = kreal), intent(in) :: spec(ncomp,nri)
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: kr, num
!
!
      num = ncomp + num_time_labels
!
      open(id_tave_rms,file=file_name)
      call write_multi_labels(id_tave_rms, num, spectr_name)
      write(id_tave_rms,*)
      do kr = 1, nri
        write(id_tave_rms,1000) i_step, time,                           &
     &         kr_sph(kr), r_sph(kr), izero, spec(1:ncomp,kr)
      end do
      close(id_tave_rms)
!
 1000 format(i16,1pE25.15e3,i16,1pE25.15e3,i16,1p255E25.15e3)
!
      end subroutine write_tave_layer_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_tave_layer_spectr_data(file_name, spectr_name,   &
     &          i_step, time, nri, ltr, ncomp, spec_l)
!
      use m_sph_ene_spectra
      use write_field_labels
!
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in)                               &
     &       :: spectr_name(ncomp_sph_spec+num_time_labels)
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(in) :: nri, ltr, ncomp
      real(kind = kreal), intent(in) :: spec_l(ncomp,0:ltr,nri)
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: kr, lth, num
!
!
      num = ncomp + num_time_labels
!
      open(id_tave_rms,file=file_name)
      call write_multi_labels(id_tave_rms, num, spectr_name)
      write(id_tave_rms,*)
      do kr = 1, nri
        do lth = 0, ltr
          write(id_tave_rms,1000) i_step, time,                         &
     &         kr_sph(kr), r_sph(kr), lth, spec_l(1:ncomp,lth,kr)
        end do
      end do
      close(id_tave_rms)
!
 1000 format(i16,1pE25.15e3,i16,1pE25.15e3,i16,1p255E25.15e3)
!
      end subroutine write_tave_layer_spectr_data
!
!   --------------------------------------------------------------------
!
      end module m_tave_sph_ene_spectr
