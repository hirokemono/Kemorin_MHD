!
!      module m_work_4_dynamic_model
!
!  Written by Kemorin
!
!      subroutine allocate_work_4_dynamic
!      subroutine allocate_work_4_dynamic_by_egrp
!
!      subroutine deallocate_work_4_dynamic
!
!      subroutine lsq_model_coefs_4_comps(ncomp_sgs)
!      subroutine lsq_whole_model_coefs(ncomp_sgs)
!
      module m_work_4_dynamic_model
!
      use m_precision
!
      implicit none
!
      real(kind=kreal), allocatable :: sgs_l(:,:)
      real(kind=kreal), allocatable :: sgs_les(:,:)
!
      real(kind=kreal), allocatable :: sgs_w(:), sgs_wg(:)
!
      real(kind=kreal), allocatable :: dnum(:)
      real(kind=kreal) :: dnum_w
!
      real(kind = kreal), allocatable :: sgs_l_smp(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_dynamic
!
      use m_machine_parameter
      use m_layering_ele_list
!
!
      allocate (sgs_wg(18) )
      allocate (sgs_w(18) )
      allocate (sgs_l(n_layer_d,18))
      allocate (sgs_les(n_layer_d,18))
      allocate (dnum(n_layer_d))
      sgs_wg =   0.0d0
      sgs_w =    0.0d0
      sgs_l =    0.0d0
      sgs_les =  0.0d0
!
      allocate (sgs_l_smp(np_smp,18))
      sgs_l_smp = 0.0d0
!
      end subroutine allocate_work_4_dynamic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_work_4_dynamic
!
      deallocate (sgs_wg, sgs_w, sgs_l, sgs_les, dnum)
      deallocate (sgs_l_smp)
!
      end subroutine deallocate_work_4_dynamic
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine lsq_model_coefs_4_comps(ncomp_sgs)
!
      use calypso_mpi
      use m_layering_ele_list
!
      integer(kind = kint), intent(in) :: ncomp_sgs
!
!
      sgs_les(1:n_layer_d,1:ncomp_sgs) = 0.0d0
        call MPI_allREDUCE ( sgs_l, sgs_les, ncomp_sgs*n_layer_d,       &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine lsq_model_coefs_4_comps
!
!  ---------------------------------------------------------------------
!
      subroutine lsq_whole_model_coefs(ncomp_sgs)
!
      use calypso_mpi

!
      integer(kind = kint), intent(in) :: ncomp_sgs
!
!
      sgs_wg(1:ncomp_sgs) = 0.0d0
      call MPI_allREDUCE ( sgs_w, sgs_wg, ncomp_sgs,                    &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'sgs_les_whole', icomp_f, sgs_les
!
      end subroutine lsq_whole_model_coefs
!
!  ---------------------------------------------------------------------
!
      end module m_work_4_dynamic_model
