!t_work_layer_correlate.f90
!      module t_work_layer_correlate
!
!  Written by Kemorin
!
!      subroutine alloc_work_layer_correlate(n_layer_d, wk_cor)
!      subroutine dealloc_work_layer_correlate(wk_cor)
!
!      subroutine sum_layerd_averages(n_layer_d, wk_cor)
!      subroutine sum_layerd_correlation(n_layer_d, wk_cor)
!
!      subroutine sum_whole_averages(wk_cor)
!      subroutine sum_whole_correlation(wk_cor)
!
      module t_work_layer_correlate
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type dynamic_correlation_data
        integer(kind = kint) :: nlayer
        integer(kind = kint) :: ncomp_sgl
        integer(kind = kint) :: ncomp_dble
        real(kind = kreal), allocatable :: ave_l(:,:)
        real(kind = kreal), allocatable :: rms_l(:,:)
        real(kind = kreal), allocatable :: sig_l(:,:)
        real(kind = kreal), allocatable :: cov_l(:,:)
        real(kind = kreal), allocatable :: ave_w(:)
        real(kind = kreal), allocatable :: rms_w(:)
        real(kind = kreal), allocatable :: sig_w(:)
        real(kind = kreal), allocatable :: cov_w(:)
!
        real(kind = kreal), allocatable :: ave_les(:,:)
        real(kind = kreal), allocatable :: rms_les(:,:)
        real(kind = kreal), allocatable :: sig_les(:,:)
        real(kind = kreal), allocatable :: cov_les(:,:)
        real(kind = kreal), allocatable :: ave_wg(:)
        real(kind = kreal), allocatable :: rms_wg(:)
        real(kind = kreal), allocatable :: sig_wg(:)
        real(kind = kreal), allocatable :: cov_wg(:)
!
        real(kind = kreal), allocatable :: ave_l_smp(:,:)
        real(kind = kreal), allocatable :: rms_l_smp(:,:)
        real(kind = kreal), allocatable :: sig_l_smp(:,:)
        real(kind = kreal), allocatable :: cor_l_smp(:,:)
      end type dynamic_correlation_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_layer_correlate                             &
     &         (n_layer_d, ncomp_correlate, wk_cor)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, ncomp_correlate
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
      wk_cor%nlayer =    n_layer_d
      wk_cor%ncomp_sgl = ncomp_correlate
      wk_cor%ncomp_dble = itwo * ncomp_correlate
!
      allocate (wk_cor%ave_w(wk_cor%ncomp_dble)  )
      allocate (wk_cor%rms_w(wk_cor%ncomp_dble)  )
      allocate (wk_cor%ave_wg(wk_cor%ncomp_dble)  )
      allocate (wk_cor%rms_wg(wk_cor%ncomp_dble)  )
      allocate (wk_cor%ave_l(wk_cor%nlayer,wk_cor%ncomp_dble)  )
      allocate (wk_cor%rms_l(wk_cor%nlayer,wk_cor%ncomp_dble)  )
      allocate (wk_cor%ave_les(wk_cor%nlayer,wk_cor%ncomp_dble))
      allocate (wk_cor%rms_les(wk_cor%nlayer,wk_cor%ncomp_dble))
!
      allocate (wk_cor%sig_w(wk_cor%ncomp_dble)  )
      allocate (wk_cor%cov_w(wk_cor%ncomp_sgl)  )
      allocate (wk_cor%sig_wg(wk_cor%ncomp_dble)  )
      allocate (wk_cor%cov_wg(wk_cor%ncomp_sgl)  )
      allocate (wk_cor%sig_l(wk_cor%nlayer,wk_cor%ncomp_dble)  )
      allocate (wk_cor%cov_l(wk_cor%nlayer,wk_cor%ncomp_sgl  )   )
      allocate (wk_cor%sig_les(wk_cor%nlayer,wk_cor%ncomp_dble))
      allocate (wk_cor%cov_les(wk_cor%nlayer,wk_cor%ncomp_sgl  ) )
!
      allocate (wk_cor%ave_l_smp(np_smp,wk_cor%ncomp_dble))
      allocate (wk_cor%rms_l_smp(np_smp,wk_cor%ncomp_dble))
      allocate (wk_cor%sig_l_smp(np_smp,wk_cor%ncomp_dble))
      allocate (wk_cor%cor_l_smp(np_smp,wk_cor%ncomp_sgl))
!
      if(wk_cor%nlayer .gt. 0) then
        wk_cor%ave_l = zero
        wk_cor%rms_l = zero
        wk_cor%cov_l = zero
        wk_cor%rms_l = zero
        wk_cor%ave_les = zero
        wk_cor%rms_les = zero
        wk_cor%sig_les = zero
        wk_cor%cov_les = zero
      end if
!
      wk_cor%sig_w = 0.0d0
      wk_cor%cov_w = 0.0d0
      wk_cor%ave_w = 0.0d0
      wk_cor%rms_w = 0.0d0
      wk_cor%sig_wg = 0.0d0
      wk_cor%cov_wg = 0.0d0
      wk_cor%ave_wg = 0.0d0
      wk_cor%rms_wg = 0.0d0
      wk_cor%ave_l_smp = 0.0d0
      wk_cor%rms_l_smp = 0.0d0
      wk_cor%sig_l_smp = 0.0d0
      wk_cor%cor_l_smp = 0.0d0
!
      end subroutine alloc_work_layer_correlate
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_layer_correlate(wk_cor)
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
      deallocate(wk_cor%ave_w, wk_cor%rms_w)
      deallocate(wk_cor%ave_wg, wk_cor%rms_wg)
      deallocate(wk_cor%ave_l, wk_cor%rms_l)
      deallocate(wk_cor%ave_les, wk_cor%rms_les)
      deallocate(wk_cor%sig_w, wk_cor%cov_w)
      deallocate(wk_cor%sig_wg, wk_cor%cov_wg)
      deallocate(wk_cor%sig_l, wk_cor%cov_l)
      deallocate(wk_cor%sig_les, wk_cor%cov_les)
      deallocate(wk_cor%ave_l_smp, wk_cor%rms_l_smp)
      deallocate(wk_cor%sig_l_smp, wk_cor%cor_l_smp)
!
      end subroutine dealloc_work_layer_correlate
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sum_layerd_averages(n_layer_d, wk_cor)
!
      use calypso_mpi
!
      integer (kind = kint), intent(in) :: n_layer_d
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      integer (kind = kint) :: num
!
!
      num = wk_cor%ncomp_dble * wk_cor%nlayer
!
      wk_cor%ave_les(1:n_layer_d,1:wk_cor%ncomp_dble) = 0.0d0
      wk_cor%rms_les(1:n_layer_d,1:wk_cor%ncomp_dble) = 0.0d0
!
      call MPI_allREDUCE(wk_cor%ave_l, wk_cor%ave_les, num,             &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(wk_cor%rms_l, wk_cor%rms_les, num,             &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_layerd_averages
!
! ----------------------------------------------------------------------
!
      subroutine sum_layerd_correlation(n_layer_d, wk_cor)
!
      use calypso_mpi
!
      integer (kind = kint), intent(in) :: n_layer_d
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      integer (kind = kint) :: num_1, num_2
!
!
      num_1 = wk_cor%ncomp_sgl *  wk_cor%nlayer
      num_2 = wk_cor%ncomp_dble * wk_cor%nlayer
!
      wk_cor%sig_les(1:n_layer_d,1:wk_cor%ncomp_dble) = 0.0d0
      wk_cor%cov_les(1:n_layer_d,1:wk_cor%ncomp_sgl ) = 0.0d0
!
      call MPI_allREDUCE(wk_cor%sig_l, wk_cor%sig_les, num_2,           &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(wk_cor%cov_l, wk_cor%cov_les, num_1,           &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_layerd_correlation
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sum_whole_averages(wk_cor)
!
      use calypso_mpi
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
      wk_cor%ave_wg(1:wk_cor%ncomp_dble) = 0.0d0
      wk_cor%rms_wg(1:wk_cor%ncomp_dble) = 0.0d0
!
      call MPI_allREDUCE                                                &
     &   (wk_cor%ave_w, wk_cor%ave_wg, wk_cor%ncomp_dble,               &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE                                                &
     &   (wk_cor%rms_w, wk_cor%rms_wg, wk_cor%ncomp_dble,               &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_whole_averages
!
! ----------------------------------------------------------------------
!
      subroutine sum_whole_correlation(wk_cor)
!
      use calypso_mpi
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
      wk_cor%sig_wg(1:wk_cor%ncomp_dble) = 0.0d0
      wk_cor%cov_wg(1:wk_cor%ncomp_sgl ) = 0.0d0
!
      call MPI_allREDUCE                                                &
     &   (wk_cor%sig_w, wk_cor%sig_wg, wk_cor%ncomp_dble,               &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(wk_cor%cov_w, wk_cor%cov_wg, wk_cor%ncomp_sgl, &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_whole_correlation
!
!  ---------------------------------------------------------------------
!
      end module t_work_layer_correlate
