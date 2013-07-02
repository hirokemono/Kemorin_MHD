!m_all_layerd_correlate.f90
!      module m_all_layerd_correlate
!
      module m_all_layerd_correlate
!
!     Written by H. Matsui on Nov., 2009
!
      use m_precision
!
      implicit none
!
      integer (kind=kint) :: nfld_correlate
      integer (kind=kint) :: ntot_correlate, nlayer_correlate
      character(len=kchara) :: cor_comp_name(6)
      character(len=kchara), allocatable :: cor_name(:)
      real(kind = kreal), allocatable :: ave_ref(:,:)
      real(kind = kreal), allocatable :: ave_tgt(:,:)
      real(kind = kreal), allocatable :: rms_ref(:,:)
      real(kind = kreal), allocatable :: rms_tgt(:,:)
      real(kind = kreal), allocatable :: ave_ratio(:,:)
      real(kind = kreal), allocatable :: rms_ratio(:,:)
!
      real(kind = kreal), allocatable :: cor_data(:,:)
      real(kind = kreal), allocatable :: cov_data(:,:)
!
!
      integer (kind=kint), parameter :: id_layer_ave_ref = 30
      integer (kind=kint), parameter :: id_layer_ave_tgt = 31
      integer (kind=kint), parameter :: id_layer_rms_ref = 32
      integer (kind=kint), parameter :: id_layer_rms_tgt = 33
      integer (kind=kint), parameter :: id_layer_ave_rto = 34
      integer (kind=kint), parameter :: id_layer_rms_rto = 35
      integer (kind=kint), parameter :: id_layer_correlt = 36
      integer (kind=kint), parameter :: id_layer_covarit = 37
!
      character(len=kchara), parameter                                  &
     &        :: fname_layer_ave_ref = 'ave_reference_layer.dat'
      character(len=kchara), parameter                                  &
     &        :: fname_layer_ave_tgt = 'ave_target_layer.dat'
      character(len=kchara), parameter                                  &
     &        :: fname_layer_rms_ref = 'rms_reference_layer.dat'
      character(len=kchara), parameter                                  &
     &        :: fname_layer_rms_tgt = 'rms_target_layer.dat'
      character(len=kchara), parameter                                  &
     &        :: fname_layer_correlt = 'correlate_layer.dat'
      character(len=kchara), parameter                                  &
     &        :: fname_layer_covarnt = 'covariant_layer.dat'
      character(len=kchara), parameter                                  &
     &        :: fname_layer_ave_rto = 'ave_ratio_layer.dat'
      character(len=kchara), parameter                                  &
     &        :: fname_layer_rms_rto = 'rms_ratio_layer.dat'
!
!      subroutine allocate_all_layer_correlate
!      subroutine allocate_name_layer_correlate
!      subroutine deallocate_all_layer_correlate
!      subroutine deallocate_name_layer_correlate
!
!      subroutine write_layerd_correlate_data(i_step)
!      subroutine read_layerd_correlate_data(i_step, ierr)
!
!      subroutine write_layerd_correlate_header
!      subroutine read_layerd_correlate_header
!
!      subroutine open_layerd_correlate_files
!      subroutine close_layerd_correlate_files
!
!      subroutine write_layerd_data(id_file, i_step, data)
!      subroutine read_layerd_data(id_file, i_step, data, ierr)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_all_layer_correlate
!
!
      allocate(ave_ref(nlayer_correlate,ntot_correlate))
      allocate(ave_tgt(nlayer_correlate,ntot_correlate))
      allocate(ave_ratio(nlayer_correlate,ntot_correlate))
      allocate(rms_ref(nlayer_correlate,ntot_correlate))
      allocate(rms_tgt(nlayer_correlate,ntot_correlate))
      allocate(rms_ratio(nlayer_correlate,ntot_correlate))
!
      allocate(cor_data(nlayer_correlate,ntot_correlate))
      allocate(cov_data(nlayer_correlate,ntot_correlate))
!
      ave_ref =   0.0d0
      ave_tgt =   0.0d0
      rms_ref =   0.0d0
      rms_tgt =   0.0d0
      ave_ratio = 0.0d0
      rms_ratio = 0.0d0
      cor_data =  0.0d0
      cov_data =  0.0d0
!
      end subroutine allocate_all_layer_correlate
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_name_layer_correlate
!
!
      allocate(cor_name(ntot_correlate))
!
      end subroutine allocate_name_layer_correlate
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_all_layer_correlate
!
!
      deallocate(ave_ref, ave_tgt, ave_ratio)
      deallocate(rms_ref, rms_tgt, rms_ratio)
      deallocate(cor_data, cov_data)
!
      end subroutine deallocate_all_layer_correlate
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_name_layer_correlate
!
!
      deallocate(cor_name)
!
      end subroutine deallocate_name_layer_correlate
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_layerd_correlate_data(i_step)
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call write_layerd_data(id_layer_ave_ref, i_step, ave_ref)
      call write_layerd_data(id_layer_ave_tgt, i_step, ave_tgt)
      call write_layerd_data(id_layer_ave_rto, i_step, ave_ratio)
      call write_layerd_data(id_layer_rms_ref, i_step, rms_ref)
      call write_layerd_data(id_layer_rms_tgt, i_step, rms_tgt)
      call write_layerd_data(id_layer_rms_rto, i_step, rms_ratio)
      call write_layerd_data(id_layer_correlt, i_step, cor_data)
      call write_layerd_data(id_layer_covarit, i_step, cov_data)
!
      end subroutine write_layerd_correlate_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_layerd_correlate_data(i_step, ierr)
!
      integer(kind = kint), intent(inout) :: i_step, ierr
!
!
      call read_layerd_data(id_layer_ave_ref, i_step, ave_ref, ierr)
      call read_layerd_data(id_layer_ave_tgt, i_step, ave_tgt, ierr)
      call read_layerd_data(id_layer_ave_rto, i_step, ave_ratio, ierr)
      call read_layerd_data(id_layer_rms_ref, i_step, rms_ref, ierr)
      call read_layerd_data(id_layer_rms_tgt, i_step, rms_tgt, ierr)
      call read_layerd_data(id_layer_rms_rto, i_step, rms_ratio, ierr)
      call read_layerd_data(id_layer_correlt, i_step, cor_data, ierr)
      call read_layerd_data(id_layer_covarit, i_step, cov_data, ierr)
!
      end subroutine read_layerd_correlate_data
!
!  ---------------------------------------------------------------------
!
      subroutine write_layerd_correlate_header
!
!
      call  write_layerd_header(id_layer_ave_ref)
      call  write_layerd_header(id_layer_ave_tgt)
      call  write_layerd_header(id_layer_ave_rto)
      call  write_layerd_header(id_layer_rms_ref)
      call  write_layerd_header(id_layer_rms_tgt)
      call  write_layerd_header(id_layer_rms_rto)
      call  write_layerd_header(id_layer_correlt)
      call  write_layerd_header(id_layer_covarit)
!
      end subroutine write_layerd_correlate_header
!
!  ---------------------------------------------------------------------
!
      subroutine read_layerd_correlate_header
!
      use m_constants
      use read_layer_evo_file_header
!
!
      call count_num_comp_layer_evo_file(id_layer_ave_ref,              &
     &    fname_layer_ave_ref, ithree, ntot_correlate,                  &
     &    nlayer_correlate)
!      write(*,*)  'ntot_correlate',ntot_correlate, nlayer_correlate
!
      call allocate_name_layer_correlate
!
      call read_field_name_evo_file(id_layer_ave_ref,                   &
     &    fname_layer_ave_ref, ithree, ntot_correlate, cor_name)
      call read_field_name_evo_file(id_layer_ave_tgt,                   &
     &    fname_layer_ave_tgt, ithree, ntot_correlate, cor_name)
      call read_field_name_evo_file(id_layer_ave_rto,                   &
     &    fname_layer_ave_rto, ithree, ntot_correlate, cor_name)
      call read_field_name_evo_file(id_layer_rms_ref,                   &
     &    fname_layer_rms_ref, ithree, ntot_correlate, cor_name)
      call read_field_name_evo_file(id_layer_rms_tgt,                   &
     &    fname_layer_rms_tgt, ithree, ntot_correlate, cor_name)
      call read_field_name_evo_file(id_layer_rms_rto,                   &
     &    fname_layer_rms_rto, ithree, ntot_correlate, cor_name)
      call read_field_name_evo_file(id_layer_correlt,                   &
     &    fname_layer_correlt, ithree, ntot_correlate, cor_name)
      call read_field_name_evo_file(id_layer_covarit,                   &
     &    fname_layer_covarnt, ithree, ntot_correlate, cor_name)
!
      call allocate_all_layer_correlate
!
      end subroutine read_layerd_correlate_header
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine open_layerd_correlate_files
!
!
      open(id_layer_ave_ref, file = fname_layer_ave_ref)
      open(id_layer_ave_tgt, file = fname_layer_ave_tgt)
      open(id_layer_rms_ref, file = fname_layer_rms_ref)
      open(id_layer_rms_tgt, file = fname_layer_rms_tgt)
      open(id_layer_correlt, file = fname_layer_correlt)
      open(id_layer_covarit, file = fname_layer_covarnt)
      open(id_layer_ave_rto, file = fname_layer_ave_rto)
      open(id_layer_rms_rto, file = fname_layer_rms_rto)
!
      end subroutine open_layerd_correlate_files
!
!  ---------------------------------------------------------------------
!
      subroutine close_layerd_correlate_files
!
!
      close(id_layer_ave_ref)
      close(id_layer_ave_tgt)
      close(id_layer_rms_ref)
      close(id_layer_rms_tgt)
      close(id_layer_correlt)
      close(id_layer_covarit)
      close(id_layer_ave_rto)
      close(id_layer_rms_rto)
!
      end subroutine close_layerd_correlate_files
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_layerd_data(id_file, i_step, data)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind= kreal), intent(in)                                     &
     &          :: data(nlayer_correlate,ntot_correlate)
!
      integer(kind = kint) :: igrp
!
!      write(*,*) 'nlayer_correlate', nlayer_correlate
      do igrp = 1, nlayer_correlate
        write(id_file,'(i10,1pE25.15e3,i10,1p255E25.15e3)')             &
     &            i_step, zero, igrp, data(igrp,1:ntot_correlate)
      end do
!
      end subroutine write_layerd_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_layerd_data(id_file, i_step, data, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind= kreal), intent(inout)                                  &
     &          :: data(nlayer_correlate,ntot_correlate)
!
      integer(kind = kint) :: igrp, itmp
      real(kind = kreal) :: rtmp
!
      do igrp = 1, nlayer_correlate
        read(id_file,*,ERR=99, END=99) i_step, rtmp, itmp,              &
     &        data(igrp,1:ntot_correlate)
      end do
      ierr = 0
      return
!
  99  continue
      ierr = 1
!
      end subroutine read_layerd_data
!
!  ---------------------------------------------------------------------
!
      subroutine write_layerd_header(id_file)
!
      use write_field_labels
!
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i_comp
!
!
!      write(id_file,'(a)')  '! Num. of components, layer '
!      write(id_file,'(2i10)') ntot_correlate, nlayer_correlate
!
      write(id_file,'(a)', advance='NO')                                &
     &                   'time_step, not_in_use, group_id, '
      do i_comp = 1, ntot_correlate
        call write_one_label(id_file, cor_name(i_comp))
      end do
      write(id_file,*)
!
      end subroutine write_layerd_header
!
!  ---------------------------------------------------------------------
!
      end module m_all_layerd_correlate
