!>@file   compare_sph_mean_square.f90
!!        program compare_sph_mean_square
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!>@brief Compare volume mean square data from reference
!!@n
!!@n      Reference data: reference/sph_pwr_volumne.dat
!!@n      Compared data:          ./sph_pwr_volumne.dat
!!
!
      program compare_sph_mean_square
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: ierr, i, icomp, itmp, istep
      integer(kind = kint) :: nfld, ltr_sph
      character(len=255) :: tmpchara
!
      character(len = kchara) :: fname_rms_vol = 'sph_pwr_volume.dat'
      character(len = kchara) :: fname_rms_ref                          &
     &                               = 'reference/sph_pwr_volume.dat'
      integer(kind = kint), parameter :: id_file_rms =      34
!
      integer(kind = kint) :: ncomp_sph_spec
      character(len = kchara), allocatable :: ene_sph_spec_name(:)
      real(kind = kreal), allocatable :: spectr_t(:)
      real(kind = kreal), allocatable :: spectr_ref(:)
      real(kind = kreal), allocatable :: diff(:)
      real(kind = kreal) :: time, time_ref
!
      open (id_file_rms,file=fname_rms_vol)
!
      read(id_file_rms,*)
      read(id_file_rms,*) itmp, ltr_sph
      read(id_file_rms,*)
      read(id_file_rms,*)
      read(id_file_rms,*)
      read(id_file_rms,*) nfld, ncomp_sph_spec
!
      allocate( ene_sph_spec_name(ncomp_sph_spec) )
      allocate( spectr_t(ncomp_sph_spec) )
      allocate( spectr_ref(ncomp_sph_spec) )
      allocate( diff(ncomp_sph_spec) )
!
!    Evaluate time average
!
      istep = 0
      read(id_file_rms,*) tmpchara, tmpchara,                           &
     &                    ene_sph_spec_name(1:ncomp_sph_spec)
      write(*,'(26a1,a6,i12,a8)',advance="NO") (char(8),i=1,26),        &
     &       'step= ', istep,   ' is read'
      do
        read(id_file_rms,*,err=99,end=99) istep, time,                  &
     &         spectr_t(1:ncomp_sph_spec)
        write(*,'(26a1,a6,i12,a8)',advance="NO") (char(8),i=1,26),      &
     &       'step= ', istep,   ' is read'
      end do
!
  99  continue
      close(id_file_rms)
!
      open (id_file_rms,file=fname_rms_ref)
!
      read(id_file_rms,*)
      read(id_file_rms,*) itmp, ltr_sph
      read(id_file_rms,*)
      read(id_file_rms,*)
      read(id_file_rms,*)
      read(id_file_rms,*) nfld, ncomp_sph_spec
!
!    Evaluate time average
!
      istep = 0
      read(id_file_rms,*) tmpchara, tmpchara,                           &
     &                    ene_sph_spec_name(1:ncomp_sph_spec)
      write(*,'(26a1,a6,i12,a8)',advance="NO") (char(8),i=1,26),        &
     &       'step= ', istep,   ' is read'
      do
        read(id_file_rms,*,err=98,end=98) istep, time_ref,              &
     &         spectr_ref(1:ncomp_sph_spec)
        write(*,'(26a1,a6,i12,a8)',advance="NO") (char(8),i=1,26),      &
     &       'step= ', istep,   ' is read'
      end do
      write(*,*) char(10)
!
  98  continue
      close(id_file_rms)
!
!
      do icomp = 1, ncomp_sph_spec
        diff(icomp) = abs(spectr_t(icomp) - spectr_ref(icomp))          &
     &               / spectr_ref(icomp)
      end do
!
      ierr = 0
      do icomp = 1, ncomp_sph_spec
        if(diff(icomp) .gt. 1.d-9) then
          write(*,*) 'Large error in ', trim(ene_sph_spec_name(icomp)), &
     &               ': ', diff(icomp)
          ierr = 1
          exit
        end if
      end do

      if(ierr .gt. 0) stop '!!!!!!  Chack Failed!!'
      stop 'Mean square data are successfully checked'
!
      end program compare_sph_mean_square
