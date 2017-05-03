!>@file   m_sph_ene_spectra.f90
!!        module m_sph_ene_spectra
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine allocate_sph_espec_name
!!      subroutine allocate_sph_sq_data
!!
!!      subroutine deallocate_sph_espec_name
!!      subroutine deallocate_sph_espec_data
!!      subroutine deallocate_sph_spectr_data
!!      subroutine deallocate_sph_sq_data
!!
!!      subroutine select_sph_ene_spec_data_file
!!      subroutine set_org_ene_spec_file_name
!!
!!      subroutine open_org_ene_spec_data
!!
!!      subroutine read_org_layer_ene_data(istep, ierr)
!!      subroutine read_org_volume_ene_data(istep, ierr)
!!@endverbatim
!
      module m_sph_ene_spectra
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      integer(kind = kint) :: ltr_sph, nri_sph
      integer(kind = kint) :: ncomp_sph_spec, num_time_labels
      character(len = kchara), allocatable :: ene_sph_spec_name(:)
!
      integer(kind = kint) :: istep_st, istep_ed, istep_read
      integer(kind = kint) :: ist_true, ied_true
      real(kind = kreal) :: time_sph, time_ini, pre_time
!
      integer(kind = kint), allocatable :: kr_sph(:)
      real(kind = kreal), allocatable :: r_sph(:)
      real(kind = kreal), allocatable :: spectr_t(:,:)
      real(kind = kreal), allocatable :: spectr_l(:,:,:)
      real(kind = kreal), allocatable :: spectr_m(:,:,:)
      real(kind = kreal), allocatable :: spectr_lm(:,:,:)
!
      real(kind = kreal), allocatable :: spectr_pre_t(:,:)
      real(kind = kreal), allocatable :: spectr_pre_l(:,:,:)
!
      integer(kind = kint) :: iflag_sph_ene_file
      integer(kind = kint) :: ilayer_sph_ene
!
!     data file ID
!
      integer(kind = kint), parameter :: id_file_rms_l =    31
      integer(kind = kint), parameter :: id_file_rms_m =    32
      integer(kind = kint), parameter :: id_file_rms_lm =   33
      integer(kind = kint), parameter :: id_file_rms =      34
!
!      data file name
!
      character(len = kchara) :: fhead_rms_vol =    'sph_pwr_volume'
      character(len = kchara) :: fhead_rms_layer =  'sph_pwr_layer'
!
      character(len = kchara) :: fname_org_rms_l
      character(len = kchara) :: fname_org_rms_m
      character(len = kchara) :: fname_org_rms_lm
      character(len = kchara) :: fname_org_rms
!
      private :: fhead_rms_vol, fhead_rms_layer
      private :: ilayer_sph_ene
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_sph_espec_name
!
!
      allocate( ene_sph_spec_name(ncomp_sph_spec+num_time_labels) )
!
      end subroutine allocate_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine allocate_sph_espec_data
!
!
      allocate( kr_sph(nri_sph) )
      allocate( r_sph(nri_sph) )
!
      allocate( spectr_t(ncomp_sph_spec,nri_sph) )
      allocate( spectr_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( spectr_m(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( spectr_lm(ncomp_sph_spec,0:ltr_sph,nri_sph) )
!
      allocate( spectr_pre_t(ncomp_sph_spec,nri_sph) )
      allocate( spectr_pre_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
!
      r_sph = zero
!
      spectr_t =  zero
      spectr_l =  zero
      spectr_m =  zero
      spectr_lm = zero
!
      spectr_pre_t =  zero
      spectr_pre_l =  zero
!
      end subroutine allocate_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine allocate_sph_spectr_data
!
!
      allocate( kr_sph(nri_sph) )
      allocate( r_sph(nri_sph) )
!
      allocate( spectr_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( spectr_pre_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
!
      r_sph = zero
      spectr_l =  zero
      spectr_pre_l =  zero
!
      end subroutine allocate_sph_spectr_data
!
!   --------------------------------------------------------------------
!
      subroutine allocate_sph_sq_data
!
!
      allocate( kr_sph(nri_sph) )
      allocate( r_sph(nri_sph) )
!
      allocate( spectr_t(ncomp_sph_spec,nri_sph) )
      allocate( spectr_pre_t(ncomp_sph_spec,nri_sph) )
!
      r_sph = zero
!
      spectr_t =  zero
      spectr_pre_t =  zero
!
      end subroutine allocate_sph_sq_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_espec_name
!
      deallocate(ene_sph_spec_name)
!
      end subroutine deallocate_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_espec_data
!
      deallocate(ene_sph_spec_name, kr_sph, r_sph)
      deallocate(spectr_t, spectr_l, spectr_m, spectr_lm)
      deallocate(spectr_pre_t, spectr_pre_l)
!
      end subroutine deallocate_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_spectr_data
!
      deallocate(kr_sph, r_sph)
      deallocate(spectr_l, spectr_pre_l)
!
      end subroutine deallocate_sph_spectr_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_sq_data
!
      deallocate(kr_sph, r_sph)
      deallocate(spectr_t, spectr_pre_t)
!
      end subroutine deallocate_sph_sq_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_sph_ene_spec_data_file
!
!
      write(*,*) ' Choose data type to take average'
      write(*,*)  ' 1: volume average spectrum '
      write(*,*)  ' 2: spectrum on each layer  '
      write(*,*)  ' 3: spectrum on specific layer  '
      read(*,*) iflag_sph_ene_file
!
      write(*,*) 'enter file header for averaging'
      if (iflag_sph_ene_file .eq. 1) then
        read(*,*) fhead_rms_vol
      else
        read(*,*) fhead_rms_layer
      end if
!
      if (iflag_sph_ene_file .eq. 3) then
        write(*,*) ' Choose layer number'
        read(*,*) ilayer_sph_ene
      end if
!
      end subroutine select_sph_ene_spec_data_file
!
!   --------------------------------------------------------------------
!
      subroutine set_org_ene_spec_file_name
!
      use set_parallel_file_name
!
      character(len = kchara) :: fname_tmp
!
      if (iflag_sph_ene_file .eq. 1) then
        write(fname_org_rms_l, '(a,a6)')                                &
     &                        trim(fhead_rms_vol), '_l.dat'
        write(fname_org_rms_m, '(a,a6)')                                &
     &                        trim(fhead_rms_vol), '_m.dat'
        write(fname_org_rms_lm,'(a,a7)')                                &
     &                        trim(fhead_rms_vol), '_lm.dat'
        call add_dat_extension(fhead_rms_vol, fname_org_rms)
      else if (iflag_sph_ene_file .eq. 2) then
        write(fname_org_rms_l, '(a,a6)')                                &
     &                        trim(fhead_rms_layer), '_l.dat'
        write(fname_org_rms_m, '(a,a6)')                                &
     &                        trim(fhead_rms_layer), '_m.dat'
        write(fname_org_rms_lm,'(a,a7)')                                &
     &                        trim(fhead_rms_layer), '_lm.dat'
        call add_dat_extension(fhead_rms_layer, fname_org_rms)
      else if (iflag_sph_ene_file .eq. 3) then
        write(fname_org_rms_l, '(a,a2)') fhead_rms_layer, '_l'
        call add_int_suffix(ilayer_sph_ene,                             &
     &      fname_org_rms_l, fname_tmp)
        call add_dat_extension(fname_tmp, fname_org_rms_l)
!
        write(fname_org_rms_m, '(a,a2)') fhead_rms_layer, '_m'
        call add_int_suffix(ilayer_sph_ene,                             &
     &      fname_org_rms_m, fname_tmp)
        call add_dat_extension(fname_tmp, fname_org_rms_m)
!
        write(fname_org_rms_lm, '(a,a3)') fhead_rms_layer, '_lm'
        call add_int_suffix(ilayer_sph_ene,                             &
     &      fname_org_rms_lm, fname_tmp)
        call add_dat_extension(fname_tmp, fname_org_rms_lm)
!
        write(fname_org_rms, '(a,a3)') fhead_rms_layer, '_lm'
        call add_int_suffix(ilayer_sph_ene,                             &
     &      fname_org_rms, fname_tmp)
        call add_dat_extension(fname_tmp, fname_org_rms)
      end if
!
      end subroutine set_org_ene_spec_file_name
!
!   --------------------------------------------------------------------
!
      subroutine open_org_ene_spec_data
!
!
      open(id_file_rms,   file=fname_org_rms)
      open(id_file_rms_l, file=fname_org_rms_l)
      open(id_file_rms_m, file=fname_org_rms_m)
      open(id_file_rms_lm,file=fname_org_rms_lm)
!
      call read_ene_spectr_header(id_file_rms,   ione, ene_sph_spec_name)
      call read_ene_spectr_header(id_file_rms_l, izero, ene_sph_spec_name)
      call read_ene_spectr_header(id_file_rms_m, izero, ene_sph_spec_name)
      call read_ene_spectr_header(id_file_rms_lm, izero, ene_sph_spec_name)
!
      end subroutine open_org_ene_spec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ene_spectr_header                                 &
     &         (id_file, iflag_total, ene_sph_spec_name)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file, iflag_total
      character(len = kchara), intent(inout)                            &
     &             :: ene_sph_spec_name(ncomp_sph_spec+num_time_labels)
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nfld, ncomp_sph_spec
      read(id_file,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name) - iflag_total
      read(id_file,*)  ene_sph_spec_name(1:num)
!
      end subroutine read_ene_spectr_header
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function read_org_layer_ene_data(istep)
!
      integer(kind = kint), intent(inout) :: istep
!
!
      read_org_layer_ene_data                                           &
     &     = read_layer_ene_sph(id_file_rms, istep, time_sph,           &
     &      nri_sph, ncomp_sph_spec, spectr_t)
!
      read_org_layer_ene_data = read_org_layer_ene_data                 &
     &     + read_layer_ene_spectr(id_file_rms_l, istep, time_sph,      &
     &      nri_sph, ltr_sph, ncomp_sph_spec, spectr_l)
      read_org_layer_ene_data = read_org_layer_ene_data                 &
     &     + read_layer_ene_spectr(id_file_rms_m, istep, time_sph,      &
     &      nri_sph, ltr_sph, ncomp_sph_spec, spectr_m)
      read_org_layer_ene_data = read_org_layer_ene_data                 &
     &     + read_layer_ene_spectr(id_file_rms_lm, istep, time_sph,     &
     &      nri_sph, ltr_sph, ncomp_sph_spec, spectr_lm)
!
      end function read_org_layer_ene_data
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function read_org_volume_ene_data(istep)
!
      integer(kind = kint), intent(inout) :: istep
!
!
      read_org_volume_ene_data                                          &
     &     = read_vol_ene_sph(id_file_rms, istep, time_sph,             &
     &      ncomp_sph_spec, spectr_t(1,1))
!
      read_org_volume_ene_data = read_org_volume_ene_data               &
     &     + read_vol_ene_spectr(id_file_rms_l, istep, time_sph,        &
     &      ltr_sph, ncomp_sph_spec, spectr_l(1,0,1))
      read_org_volume_ene_data = read_org_volume_ene_data               &
     &     + read_vol_ene_spectr(id_file_rms_m, istep, time_sph,        &
     &      ltr_sph, ncomp_sph_spec, spectr_m(1,0,1))
      read_org_volume_ene_data = read_org_volume_ene_data               &
     &     + read_vol_ene_spectr(id_file_rms_lm, istep, time_sph,       &
     &      ltr_sph, ncomp_sph_spec, spectr_lm(1,0,1))
!
      end function read_org_volume_ene_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_layer_ene_sph(id_file,        &
     &                    istep, time, nri, ncomp, spectr_t)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri, ncomp
      integer(kind = kint), intent(inout) :: istep
      real(kind = kreal), intent(inout) :: time
      real(kind = kreal), intent(inout) :: spectr_t(ncomp,nri)
!
      integer(kind = kint) :: kr
!
!
      read_layer_ene_sph = 0
      do kr = 1, nri_sph
        read(id_file,*,err=99,end=99) istep, time,                      &
     &         kr_sph(kr), r_sph(kr), spectr_t(1:ncomp_sph_spec,kr)
      end do
      return
!
   99 continue
      read_layer_ene_sph = 1
      return
!
      end function read_layer_ene_sph
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function  read_layer_ene_spectr(id_file,     &
     &                    istep, time, nri, ltr, ncomp, spectr_l)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri, ltr, ncomp
      integer(kind = kint), intent(inout) :: istep
      real(kind = kreal), intent(inout) :: time
      real(kind = kreal), intent(inout) :: spectr_l(ncomp,0:ltr,nri)
!
      integer(kind = kint) :: itmp
      integer(kind = kint) :: kr, lth
      real(kind = kreal) :: rtmp
!
!
      do kr = 1, nri
        do lth = 0, ltr
          read(id_file,*,err=99,end=99) istep, time,                    &
     &         itmp, rtmp, itmp, spectr_l(1:ncomp_sph_spec,lth,kr)
        end do
      end do
      read_layer_ene_spectr = 0
      return
!
   99 continue
      read_layer_ene_spectr = 1
      return
!
      end function read_layer_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function read_vol_ene_sph                    &
     &                   (id_file, istep, time, ncomp, spectr_t)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(inout) :: istep
      real(kind = kreal), intent(inout) :: time
      real(kind = kreal), intent(inout) :: spectr_t(ncomp)
!
!
      read(id_file,*,err=99,end=99) istep, time, spectr_t(1:ncomp)
      read_vol_ene_sph = 0
      return
!
   99 continue
      read_vol_ene_sph = 1
      return
!
      end function read_vol_ene_sph
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function read_vol_ene_spectr                 &
     &                   (id_file, istep, time, ltr, ncomp, spectr_l)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, ncomp
      integer(kind = kint), intent(inout) :: istep
      real(kind = kreal), intent(inout) :: time
      real(kind = kreal), intent(inout) :: spectr_l(ncomp,0:ltr)
!
      integer(kind = kint) :: itmp
      integer(kind = kint) :: lth
!
!
      do lth = 0, ltr
          read(id_file,*,err=99,end=99) istep, time,                    &
     &         itmp, spectr_l(1:ncomp,lth)
      end do
      read_vol_ene_spectr = 0
      return
!
   99 continue
      read_vol_ene_spectr = 1
      return
!
      end function read_vol_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_degree_on_layer_data
!
      use skip_comment_f
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
!
      open (id_file_rms_l,file=fname_org_rms_l)
!
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nri_sph, ltr_sph
      write(*,*) 'ltr_sph', ltr_sph
      write(*,*) 'nri_sph', nri_sph
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nfld, ncomp_sph_spec
!
      num_time_labels = 5
      write(*,*) 'ncomp_sph_spec', ncomp_sph_spec
      call allocate_sph_espec_name
!
!
      read(id_file_rms_l,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name)
      write(*,*) 'num vector', num
      read(id_file_rms_l,*)  ene_sph_spec_name(1:num)
      do i = 1, NUM
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      close(id_file_rms_l)
!
      end subroutine count_degree_on_layer_data
!
!   --------------------------------------------------------------------
!
      subroutine count_degree_one_layer_data
!
      use skip_comment_f
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
!
      open (id_file_rms_l,file=fname_org_rms_l)
!
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nri_sph, ltr_sph
      nri_sph = 1
      write(*,*) 'ltr_sph', ltr_sph
      write(*,*) 'nri_sph', nri_sph
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nfld, ncomp_sph_spec
!
      num_time_labels = 5
      write(*,*) 'ncomp_sph_spec', ncomp_sph_spec
      call allocate_sph_espec_name
!
!
      read(id_file_rms_l,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name)
      write(*,*) 'num vector', num
      read(id_file_rms_l,*)  ene_sph_spec_name(1:num)
      do i = 1, NUM
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      close(id_file_rms_l)
!
      end subroutine count_degree_one_layer_data
!
!   --------------------------------------------------------------------
!
      subroutine count_degree_on_volume_data
!
      use skip_comment_f
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
      open (id_file_rms_l,file=fname_org_rms_l)
!
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) itmp, ltr_sph
      write(*,*) 'ltr_sph', ltr_sph
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nfld, ncomp_sph_spec
!
      num_time_labels = 3
      nri_sph = 1
      write(*,*) 'ncomp_sph_spec', ncomp_sph_spec
      write(*,*) 'nri_sph', nri_sph
      call allocate_sph_espec_name
!
!
      read(id_file_rms_l,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name)
      write(*,*) 'num v', num
      read(id_file_rms_l,*)  ene_sph_spec_name(1:num)
      DO I = 1, NUM
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      close(id_file_rms_l)
!
      end subroutine count_degree_on_volume_data
!
!   --------------------------------------------------------------------
!
      end module m_sph_ene_spectra
