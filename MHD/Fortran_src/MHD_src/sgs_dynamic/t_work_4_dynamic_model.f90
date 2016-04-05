!
!      module t_work_4_dynamic_model
!
!  Written by Kemorin
!
!!      subroutine alloc_work_4_dynamic(n_layer_d, wk_lsq)
!!      subroutine dealloc_work_4_dynamic
!!
!!      subroutine sum_lsq_coefs_4_comps(ncomp_sgs, wk_lsq)
!!      subroutine sum_lsq_whole_coefs(ncomp_sgs)
!
      module t_work_4_dynamic_model
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: ncomp_lsq = 18
!
      type dynamis_least_suare_data
        integer(kind = kint) :: nlayer
!
        real(kind=kreal), pointer :: slocal(:,:)
        real(kind=kreal), pointer :: slsq(:,:)
!
        real(kind=kreal), pointer :: wlocal(:)
        real(kind=kreal), pointer :: wlsq(:)
!
        real(kind=kreal), pointer :: dnorm(:)
!
        real(kind = kreal), pointer :: slocal_smp(:,:)
      end type dynamis_least_suare_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_4_dynamic(n_layer_d, wk_lsq)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: n_layer_d
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!
!
      wk_lsq%nlayer = n_layer_d
      allocate (wk_lsq%wlsq(ncomp_lsq) )
      allocate (wk_lsq%wlocal(ncomp_lsq) )
      allocate (wk_lsq%slocal(wk_lsq%nlayer,ncomp_lsq))
      allocate (wk_lsq%slsq(wk_lsq%nlayer,ncomp_lsq))
      allocate (wk_lsq%dnorm(wk_lsq%nlayer))

      wk_lsq%wlsq =   0.0d0
      wk_lsq%wlocal = 0.0d0
      wk_lsq%slocal = 0.0d0
      wk_lsq%slsq =   0.0d0
!
      allocate (wk_lsq%slocal_smp(np_smp,ncomp_lsq))
      wk_lsq%slocal_smp = 0.0d0
!
      end subroutine alloc_work_4_dynamic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_work_4_dynamic(wk_lsq)
!
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!
!
      deallocate(wk_lsq%wlsq, wk_lsq%wlocal)
      deallocate(wk_lsq%slocal, wk_lsq%slsq, wk_lsq%dnorm)
      deallocate(wk_lsq%slocal_smp)
!
      end subroutine dealloc_work_4_dynamic
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sum_lsq_coefs_4_comps(ncomp_sgs, wk_lsq)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: ncomp_sgs
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      integer(kind = kint) :: num
!
!
      num = ncomp_sgs*wk_lsq%nlayer
      wk_lsq%slsq(1:wk_lsq%nlayer,1:ncomp_sgs) = 0.0d0
      call MPI_allREDUCE(wk_lsq%slocal, wk_lsq%slsq, num,               &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_lsq_coefs_4_comps
!
!  ---------------------------------------------------------------------
!
      subroutine sum_lsq_whole_coefs(ncomp_sgs, wk_lsq)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: ncomp_sgs
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!
!
      wk_lsq%wlsq(1:ncomp_sgs) = 0.0d0
      call MPI_allREDUCE(wk_lsq%wlocal, wk_lsq%wlsq, ncomp_sgs,         &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'sgs_les_whole', icomp_f, wk_lsq%slsq
!
      end subroutine sum_lsq_whole_coefs
!
!  ---------------------------------------------------------------------
!
      end module t_work_4_dynamic_model
