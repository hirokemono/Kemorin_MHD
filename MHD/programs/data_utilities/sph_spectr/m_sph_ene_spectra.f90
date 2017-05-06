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
!!
!!      subroutine deallocate_sph_espec_name
!!      subroutine deallocate_sph_espec_data
!!      subroutine deallocate_sph_spectr_data
!!
!!      subroutine select_sph_ene_spec_data_file                        &
!!     &         (iflag_volume, iflag_old_fmt, input_header)
!!      subroutine set_org_ene_spec_file_name(input_header)
!!
!!      subroutine open_org_ene_spec_data
!!
!!      subroutine read_org_layer_ene_data(istep, ierr)
!!      subroutine read_org_volume_ene_data(istep, ierr)
!!
!!      subroutine count_degree_on_layer_spectr
!!      subroutine count_degree_on_volume_spectr
!!      subroutine count_degree_on_layer_sph
!!      subroutine count_degree_on_volume_sph
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
      integer(kind = kint) :: num_time_labels
      integer(kind = kint) :: nfield_sph_spec, ntot_sph_spec
      integer(kind = kint) :: num_labels
      integer(kind = kint), allocatable :: ncomp_sph_spec(:)
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
      integer(kind = kint) :: iflag_volume_average
      integer(kind = kint) :: iflag_old_file_format
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
      character(len = kchara) :: fname_org_rms_l
      character(len = kchara) :: fname_org_rms_m
      character(len = kchara) :: fname_org_rms_lm
      character(len = kchara) :: fname_org_rms
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
      num_labels = ntot_sph_spec + num_time_labels
      allocate( ene_sph_spec_name(num_labels) )
      allocate( ncomp_sph_spec(nfield_sph_spec))
      ncomp_sph_spec = 0
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
      allocate( spectr_t(ntot_sph_spec,nri_sph) )
      allocate( spectr_l(ntot_sph_spec,0:ltr_sph,nri_sph) )
      allocate( spectr_m(ntot_sph_spec,0:ltr_sph,nri_sph) )
      allocate( spectr_lm(ntot_sph_spec,0:ltr_sph,nri_sph) )
!
      r_sph = zero
!
      spectr_t =  zero
      spectr_l =  zero
      spectr_m =  zero
      spectr_lm = zero
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
      allocate( spectr_l(ntot_sph_spec,0:ltr_sph,nri_sph) )
!
      r_sph = zero
      spectr_l =  zero
!
      end subroutine allocate_sph_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_espec_name
!
      deallocate(ene_sph_spec_name, ncomp_sph_spec)
!
      end subroutine deallocate_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_espec_data
!
      deallocate(ene_sph_spec_name, kr_sph, r_sph)
      deallocate(spectr_t, spectr_l, spectr_m, spectr_lm)
!
      end subroutine deallocate_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_spectr_data
!
      deallocate(kr_sph, r_sph)
      deallocate(spectr_l)
!
      end subroutine deallocate_sph_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_sph_ene_spec_data_file                          &
     &         (iflag_volume, iflag_old_fmt, input_header)
!
      integer(kind = kint), intent(inout) :: iflag_volume
      integer(kind = kint), intent(inout) :: iflag_old_fmt
      character(len = kchara), intent(inout) :: input_header
!
!
      write(*,*) ' Choose data type to take average'
      write(*,*)  ' 0: spectrum on each layer  '
      write(*,*)  ' 1: volume average spectrum '
      read(*,*) iflag_volume
!
      write(*,*) ' Is data has old format?'
      write(*,*)  ' 1: yes '
      write(*,*)  ' 0: no  '
      read(*,*) iflag_old_fmt
!
      write(*,*) 'enter file header for averaging'
      read(*,*) input_header
!
      end subroutine select_sph_ene_spec_data_file
!
!   --------------------------------------------------------------------
!
      subroutine set_org_ene_spec_file_name(input_header)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: input_header
!
!
      write(fname_org_rms_l, '(a,a6)')                                  &
     &                        trim(input_header), '_l.dat'
      write(fname_org_rms_m, '(a,a6)')                                  &
     &                        trim(input_header), '_m.dat'
      write(fname_org_rms_lm,'(a,a7)')                                  &
     &                        trim(input_header), '_lm.dat'
      call add_dat_extension(input_header, fname_org_rms)
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
      call read_ene_spectr_header(id_file_rms)
      call read_ene_spectr_header(id_file_rms_l)
      call read_ene_spectr_header(id_file_rms_m)
      call read_ene_spectr_header(id_file_rms_lm)
!
      end subroutine open_org_ene_spec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ene_spectr_header(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len=255) :: character_4_read
      character(len = kchara) :: ctmp
      integer(kind = kint) :: itmp, i
!
!
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      read(id_file,*) (itmp,i=1,nfield_sph_spec)
!
      read(id_file,*) (ctmp,i=1,num_labels)
      write(*,*) 'aho', ctmp
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
     &      nri_sph, ntot_sph_spec, spectr_t)
!
      read_org_layer_ene_data = read_org_layer_ene_data                 &
     &     + read_layer_ene_spectr(id_file_rms_l, istep, time_sph,      &
     &      nri_sph, ltr_sph, ntot_sph_spec, spectr_l)
      read_org_layer_ene_data = read_org_layer_ene_data                 &
     &     + read_layer_ene_spectr(id_file_rms_m, istep, time_sph,      &
     &      nri_sph, ltr_sph, ntot_sph_spec, spectr_m)
      read_org_layer_ene_data = read_org_layer_ene_data                 &
     &     + read_layer_ene_spectr(id_file_rms_lm, istep, time_sph,     &
     &      nri_sph, ltr_sph, ntot_sph_spec, spectr_lm)
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
     &      ntot_sph_spec, spectr_t(1,1))
!
      read_org_volume_ene_data = read_org_volume_ene_data               &
     &     + read_vol_ene_spectr(id_file_rms_l, istep, time_sph,        &
     &      ltr_sph, ntot_sph_spec, spectr_l(1,0,1))
      read_org_volume_ene_data = read_org_volume_ene_data               &
     &     + read_vol_ene_spectr(id_file_rms_m, istep, time_sph,        &
     &      ltr_sph, ntot_sph_spec, spectr_m(1,0,1))
      read_org_volume_ene_data = read_org_volume_ene_data               &
     &     + read_vol_ene_spectr(id_file_rms_lm, istep, time_sph,       &
     &      ltr_sph, ntot_sph_spec, spectr_lm(1,0,1))
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
     &         kr_sph(kr), r_sph(kr), spectr_t(1:ntot_sph_spec,kr)
        write(*,*)
        write(*,*) 'istep', istep, time, kr, ntot_sph_spec
        write(*,*) 'kr_sph', kr_sph,  r_sph
        write(*,*) 'spectr_t',spectr_t(1:ntot_sph_spec,kr)
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
      write(*,*) 'Aho', (size(spectr_l,kr),kr = 1,3)
      do kr = 1, nri
        do lth = 0, ltr
          read(id_file,*,err=99,end=99) istep, time,                    &
     &         itmp, rtmp, itmp, spectr_l(1:ntot_sph_spec,lth,kr)
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
      subroutine count_degree_on_layer_spectr(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len=255) :: character_4_read
      integer(kind = kint) :: itmp, i
!
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nri_sph, ltr_sph
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nfield_sph_spec, ntot_sph_spec
!
      num_time_labels = 4
      num_labels = ntot_sph_spec + num_time_labels
      call allocate_sph_espec_name
!
!
      read(id_file,*) (itmp,i=1,nfield_sph_spec)
!
      write(*,*) 'num labels', num_labels
      read(id_file,*)  ene_sph_spec_name(1:num_labels)
      do i = 1, num_labels
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      end subroutine count_degree_on_layer_spectr
!
!   --------------------------------------------------------------------
!
      subroutine count_degree_on_volume_spectr(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len=255) :: character_4_read
      integer(kind = kint) :: itmp, i
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) itmp, ltr_sph
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nfield_sph_spec, ntot_sph_spec
!
      num_time_labels = 3
      num_labels = ntot_sph_spec + num_time_labels
      nri_sph = 1
      call allocate_sph_espec_name
!
!
      read(id_file,*) (itmp,i=1,nfield_sph_spec)
!
      write(*,*) 'num label', num_labels
      read(id_file,*)  ene_sph_spec_name(1:num_labels)
      DO I = 1, num_labels
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      end subroutine count_degree_on_volume_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_degree_on_layer_sph(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len=255) :: character_4_read
      integer(kind = kint) :: itmp, i
!
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nri_sph, ltr_sph
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nfield_sph_spec, ntot_sph_spec
!
      num_time_labels = 3
      num_labels = ntot_sph_spec + num_time_labels
      call allocate_sph_espec_name
!
!
      read(id_file,*) (itmp,i=1,nfield_sph_spec)
!
      write(*,*) 'num labels', num_labels
      read(id_file,*)  ene_sph_spec_name(1:num_labels)
      do i = 1, num_labels
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      end subroutine count_degree_on_layer_sph
!
!   --------------------------------------------------------------------
!
      subroutine count_degree_on_volume_sph(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len=255) :: character_4_read
      integer(kind = kint) :: itmp, i
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) itmp, ltr_sph
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nfield_sph_spec, ntot_sph_spec
!
      num_time_labels = 2
      num_labels = ntot_sph_spec + num_time_labels
      nri_sph = 1
      call allocate_sph_espec_name
!
!
      read(id_file,*) (itmp,i=1,nfield_sph_spec)
!
      read(id_file,*)  ene_sph_spec_name(1:num_labels)
      DO I = 1, num_labels
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      end subroutine count_degree_on_volume_sph
!
!   --------------------------------------------------------------------
!
      end module m_sph_ene_spectra
