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
!!      subroutine allocate_tave_sph_espec_name
!!      subroutine allocate_tave_sph_espec_data
!!      subroutine deallocate_tave_sph_espec_data
!!
!!      subroutine select_sph_ene_spec_data_file
!!      subroutine set_org_ene_spec_file_name
!!
!!      subroutine open_org_ene_spec_data
!!      subroutine open_tave_ene_spec_data
!!      subroutine open_tsigma_ene_spec_data
!!
!!      subroutine close_ene_spec_data
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
      integer(kind = kint) :: ltr_sph, nri_sph
      integer(kind = kint) :: ncomp_sph_tave, num_time_labels
      character(len = kchara), allocatable :: ene_sph_spec_name(:)
!
      integer(kind = kint) :: istep_st, istep_ed
      integer(kind = kint) :: ist_true, ied_true
      real(kind = kreal) :: time_sph
!
      integer(kind = kint), allocatable :: kr_sph(:)
      real(kind = kreal), allocatable :: spectr_t(:,:)
      real(kind = kreal), allocatable :: spectr_l(:,:,:)
      real(kind = kreal), allocatable :: spectr_m(:,:,:)
      real(kind = kreal), allocatable :: spectr_lm(:,:,:)
!
      real(kind = kreal), allocatable :: ave_spec_t(:,:)
      real(kind = kreal), allocatable :: ave_spec_l(:,:,:)
      real(kind = kreal), allocatable :: ave_spec_m(:,:,:)
      real(kind = kreal), allocatable :: ave_spec_lm(:,:,:)
!
      real(kind = kreal), allocatable :: sigma_spec_t(:,:)
      real(kind = kreal), allocatable :: sigma_spec_l(:,:,:)
      real(kind = kreal), allocatable :: sigma_spec_m(:,:,:)
      real(kind = kreal), allocatable :: sigma_spec_lm(:,:,:)
!
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
      private :: fname_org_rms_m, fname_org_rms_lm, fname_org_rms
      private :: ilayer_sph_ene
      private :: read_ene_spectr_header, write_average_ene_sph_head
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_tave_sph_espec_name
!
!
      allocate( ene_sph_spec_name(ncomp_sph_tave+num_time_labels) )
!
      end subroutine allocate_tave_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine allocate_tave_sph_espec_data
!
!
      allocate( kr_sph(nri_sph) )
!
      allocate( spectr_t(ncomp_sph_tave,nri_sph) )
      allocate( spectr_l(ncomp_sph_tave,0:ltr_sph,nri_sph) )
      allocate( spectr_m(ncomp_sph_tave,0:ltr_sph,nri_sph) )
      allocate( spectr_lm(ncomp_sph_tave,0:ltr_sph,nri_sph) )
!
      allocate( ave_spec_t(ncomp_sph_tave,nri_sph) )
      allocate( ave_spec_l(ncomp_sph_tave,0:ltr_sph,nri_sph) )
      allocate( ave_spec_m(ncomp_sph_tave,0:ltr_sph,nri_sph) )
      allocate( ave_spec_lm(ncomp_sph_tave,0:ltr_sph,nri_sph) )
!
      allocate( sigma_spec_t(ncomp_sph_tave,nri_sph) )
      allocate( sigma_spec_l(ncomp_sph_tave,0:ltr_sph,nri_sph) )
      allocate( sigma_spec_m(ncomp_sph_tave,0:ltr_sph,nri_sph) )
      allocate( sigma_spec_lm(ncomp_sph_tave,0:ltr_sph,nri_sph) )
!
      kr_sph = 0
      spectr_t =    0.0d0
      spectr_l =    0.0d0
      spectr_m =    0.0d0
      spectr_lm =   0.0d0
!
      ave_spec_t =  0.0d0
      ave_spec_l =  0.0d0
      ave_spec_m =  0.0d0
      ave_spec_lm = 0.0d0
!
      sigma_spec_t =  0.0d0
      sigma_spec_l =  0.0d0
      sigma_spec_m =  0.0d0
      sigma_spec_lm = 0.0d0
!
      end subroutine allocate_tave_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_tave_sph_espec_data
!
      deallocate(ene_sph_spec_name, kr_sph)
      deallocate(spectr_t, spectr_l, spectr_m, spectr_lm)
      deallocate(ave_spec_t, ave_spec_l, ave_spec_m, ave_spec_lm)
      deallocate(sigma_spec_l, sigma_spec_m)
      deallocate(sigma_spec_t, sigma_spec_lm)
!
      end subroutine deallocate_tave_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_sph_ene_spec_data_file
!
!
      write(*,*) ' Choose data type to taking average'
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
      call read_ene_spectr_header(id_file_rms,   ione)
      call read_ene_spectr_header(id_file_rms_l, izero)
      call read_ene_spectr_header(id_file_rms_m, izero)
      call read_ene_spectr_header(id_file_rms_lm, izero)
!
      end subroutine open_org_ene_spec_data
!
!   --------------------------------------------------------------------
!
      subroutine open_tave_ene_spec_data
!
      character(len = kchara) :: fname_tave_rms_l
      character(len = kchara) :: fname_tave_rms_m
      character(len = kchara) :: fname_tave_rms_lm
      character(len = kchara) :: fname_tave_rms
!
!
      write(fname_tave_rms_l, '(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms_l)
      write(fname_tave_rms_m, '(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms_m)
      write(fname_tave_rms_lm,'(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms_lm)
      write(fname_tave_rms,   '(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms)
!
      open(id_file_rms,   file=fname_tave_rms)
      open(id_file_rms_l, file=fname_tave_rms_l)
      open(id_file_rms_m, file=fname_tave_rms_m)
      open(id_file_rms_lm,file=fname_tave_rms_lm)
!
      call write_average_ene_sph_head
!
      end subroutine open_tave_ene_spec_data
!
!   --------------------------------------------------------------------
!
      subroutine open_tsigma_ene_spec_data
!
      character(len = kchara) :: fname_tave_rms_l
      character(len = kchara) :: fname_tave_rms_m
      character(len = kchara) :: fname_tave_rms_lm
      character(len = kchara) :: fname_tave_rms
!
!
      write(fname_tave_rms_l, '(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms_l)
      write(fname_tave_rms_m, '(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms_m)
      write(fname_tave_rms_lm,'(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms_lm)
      write(fname_tave_rms,   '(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms)
!
      open(id_file_rms,   file=fname_tave_rms)
      open(id_file_rms_l, file=fname_tave_rms_l)
      open(id_file_rms_m, file=fname_tave_rms_m)
      open(id_file_rms_lm,file=fname_tave_rms_lm)
!
      call write_average_ene_sph_head
!
      end subroutine open_tsigma_ene_spec_data
!
!   --------------------------------------------------------------------
!
      subroutine close_ene_spec_data
!
!
      close(id_file_rms)
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      end subroutine close_ene_spec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ene_spectr_header(id_file, iflag_total)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file, iflag_total
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nfld, ncomp_sph_tave
      read(id_file,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name) - iflag_total
      read(id_file,*)  ene_sph_spec_name(1:num)
!
      end subroutine read_ene_spectr_header
!
!   --------------------------------------------------------------------
!
      subroutine write_average_ene_sph_head
!
      use write_field_labels
!
      integer(kind = kint) :: num
!
!
      num = ncomp_sph_tave + num_time_labels
      write(ene_sph_spec_name(num_time_labels),'(a)') 'degree'
      call write_multi_labels(id_file_rms, num, ene_sph_spec_name)
      call write_multi_labels(id_file_rms_l, num, ene_sph_spec_name)
!
      write(ene_sph_spec_name(num_time_labels),'(a)') 'order'
      call write_multi_labels(id_file_rms_m, num, ene_sph_spec_name)
!
      write(ene_sph_spec_name(num_time_labels),'(a)')                   &
     &                                           'diff_deg_order'
      call write_multi_labels(id_file_rms_lm, num, ene_sph_spec_name)
!
      write(id_file_rms,*   )
      write(id_file_rms_l,* )
      write(id_file_rms_m,* )
      write(id_file_rms_lm,*)
!
      end subroutine write_average_ene_sph_head
!
!   --------------------------------------------------------------------
!
      end module m_tave_sph_ene_spectr
