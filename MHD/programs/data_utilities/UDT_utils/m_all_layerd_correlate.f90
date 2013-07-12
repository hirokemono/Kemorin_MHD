!m_all_layerd_correlate.f90
!      module m_all_layerd_correlate
!
!     Written by H. Matsui on Nov., 2009
!
!      subroutine allocate_all_layer_correlate
!      subroutine allocate_name_layer_correlate
!      subroutine deallocate_all_layer_correlate
!      subroutine deallocate_name_layer_correlate
!
!      subroutine write_layerd_correlate_data(my_rank, i_step)
!      subroutine read_layerd_data(id_file, i_step, data, ierr)
!
      module m_all_layerd_correlate
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
      integer (kind=kint), parameter :: id_layerd_data = 30
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
      private :: open_layerd_data, write_layerd_data
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
      subroutine write_layerd_correlate_data(my_rank, i_step)
!
      integer(kind = kint), intent(in) :: my_rank, i_step
!
!
      if(my_rank .ne. 0) return
!
      call write_layerd_data(id_layerd_data, fname_layer_ave_ref,       &
     &    i_step, ave_ref)
      call write_layerd_data(id_layerd_data, fname_layer_ave_tgt,       &
     &    i_step, ave_tgt)
      call write_layerd_data(id_layerd_data, fname_layer_ave_rto,       &
     &    i_step, ave_ratio)
      call write_layerd_data(id_layerd_data, fname_layer_rms_ref,       &
     &    i_step, rms_ref)
      call write_layerd_data(id_layerd_data, fname_layer_rms_tgt,       &
     &    i_step, rms_tgt)
      call write_layerd_data(id_layerd_data, fname_layer_rms_rto,       &
     &    i_step, rms_ratio)
      call write_layerd_data(id_layerd_data, fname_layer_correlt,       &
     &    i_step, cor_data)
      call write_layerd_data(id_layerd_data, fname_layer_covarnt,       &
     &    i_step, cov_data)
!
      end subroutine write_layerd_correlate_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine open_layerd_data(id_file, file_name)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint) :: i_comp
!
!
      open(id_file, file=file_name, form='formatted', status='old',     &
     &     position='append', err = 99)
      return
!
  99  continue
      open(id_file, file=file_name, form='formatted', status='new')
!
      write(id_file,'(a)', advance='NO')                                &
     &                   'time_step, not_in_use, group_id, '
      do i_comp = 1, ntot_correlate
        call write_one_label(id_file, cor_name(i_comp))
      end do
      write(id_file,*)
!
      end subroutine open_layerd_data
!
! -----------------------------------------------------------------------
!
      subroutine write_layerd_data(id_file, file_name, i_step, data)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_step
      real(kind= kreal), intent(in)                                     &
     &          :: data(nlayer_correlate,ntot_correlate)
!
      integer(kind = kint) :: igrp
!
!
      call open_layerd_data(id_file, file_name)
!
!      write(*,*) 'nlayer_correlate', nlayer_correlate
      do igrp = 1, nlayer_correlate
        write(id_file,'(i10,1pE25.15e3,i10,1p255E25.15e3)')             &
     &            i_step, zero, igrp, data(igrp,1:ntot_correlate)
      end do
      close(id_file)
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
      end module m_all_layerd_correlate
