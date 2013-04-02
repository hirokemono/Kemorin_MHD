!m_work_layer_correlate.f90
!      module m_work_layer_correlate
!
!  Written by Kemorin
!
!      subroutine allocate_work_layer_correlate
!      subroutine deallocate_work_layer_correlate
!
!      subroutine sum_layerd_averages
!      subroutine sum_layerd_correlation
!
!      subroutine sum_whole_averages
!      subroutine sum_whole_correlation
!
      module m_work_layer_correlate
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      integer(kind = kint) :: ncomp_correlate, ncomp_correlate_2
      real(kind = kreal), allocatable :: ave_l(:,:),   rms_l(:,:)
      real(kind = kreal), allocatable :: sig_l(:,:),   cor_l(:,:)
      real(kind = kreal), allocatable :: ave_w(:),     rms_w(:)
      real(kind = kreal), allocatable :: sig_w(:),     cor_w(:)
!
      real(kind = kreal), allocatable :: ave_les(:,:), rms_les(:,:)
      real(kind = kreal), allocatable :: sig_les(:,:), cor_les(:,:)
      real(kind = kreal), allocatable :: ave_wg(:),    rms_wg(:)
      real(kind = kreal), allocatable :: sig_wg(:),    cor_wg(:)
!
      real(kind = kreal), allocatable :: ave_l_smp(:,:)
      real(kind = kreal), allocatable :: rms_l_smp(:,:)
      real(kind = kreal), allocatable :: sig_l_smp(:,:)
      real(kind = kreal), allocatable :: cor_l_smp(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_layer_correlate
!
      use m_machine_parameter
      use m_layering_ele_list
!
!
      ncomp_correlate_2 = itwo * ncomp_correlate
!
      allocate (ave_w(ncomp_correlate_2)  )
      allocate (rms_w(ncomp_correlate_2)  )
      allocate (ave_wg(ncomp_correlate_2)  )
      allocate (rms_wg(ncomp_correlate_2)  )
      allocate (ave_l(n_layer_d,ncomp_correlate_2)  )
      allocate (rms_l(n_layer_d,ncomp_correlate_2)  )
      allocate (ave_les(n_layer_d,ncomp_correlate_2))
      allocate (rms_les(n_layer_d,ncomp_correlate_2))
!
      allocate (sig_w(ncomp_correlate_2)  )
      allocate (cor_w(ncomp_correlate)  )
      allocate (sig_wg(ncomp_correlate_2)  )
      allocate (cor_wg(ncomp_correlate)  )
      allocate (sig_l(n_layer_d,ncomp_correlate_2)  )
      allocate (cor_l(n_layer_d,ncomp_correlate  )   )
      allocate (sig_les(n_layer_d,ncomp_correlate_2))
      allocate (cor_les(n_layer_d,ncomp_correlate  ) )
!
      allocate (ave_l_smp(np_smp,ncomp_correlate_2))
      allocate (rms_l_smp(np_smp,ncomp_correlate_2))
      allocate (sig_l_smp(np_smp,ncomp_correlate_2))
      allocate (cor_l_smp(np_smp,ncomp_correlate))
!
      if(n_layer_d .gt. 0) then
        ave_l = zero
        rms_l = zero
        cor_l = zero
        rms_l = zero
        ave_les = zero
        rms_les = zero
        sig_les = zero
        cor_les = zero
      end if
!
      sig_w = 0.0d0
      cor_w = 0.0d0
      ave_w = 0.0d0
      rms_w = 0.0d0
      sig_wg = 0.0d0
      cor_wg = 0.0d0
      ave_wg = 0.0d0
      rms_wg = 0.0d0
      ave_l_smp = 0.0d0
      rms_l_smp = 0.0d0
      sig_l_smp = 0.0d0
      cor_l_smp = 0.0d0
!
      end subroutine allocate_work_layer_correlate
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_layer_correlate
!
      deallocate (ave_w, rms_w, ave_wg, rms_wg)
      deallocate (ave_l, rms_l, ave_les, rms_les)
      deallocate (sig_w, cor_w, sig_wg, cor_wg)
      deallocate (sig_l, cor_l, sig_les, cor_les )
      deallocate (ave_l_smp, rms_l_smp, sig_l_smp, cor_l_smp)
!
      end subroutine deallocate_work_layer_correlate
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sum_layerd_averages
!
      use m_parallel_var_dof
      use m_layering_ele_list
!
      integer (kind = kint) :: num
!
!
      num = ncomp_correlate_2*n_layer_d
!
      ave_les(1:n_layer_d,1:ncomp_correlate_2) = 0.0d0
      rms_les(1:n_layer_d,1:ncomp_correlate_2) = 0.0d0
!
        call MPI_allREDUCE ( ave_l(1,1), ave_les(1,1), num,             &
     &     MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
        call MPI_allREDUCE ( rms_l(1,1), rms_les(1,1), num,             &
     &     MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
!
      end subroutine sum_layerd_averages
!
! ----------------------------------------------------------------------
!
      subroutine sum_layerd_correlation
!
      use m_parallel_var_dof
      use m_layering_ele_list
!
      integer (kind = kint) :: num_1, num_2
!
!
      num_1 = ncomp_correlate *   n_layer_d
      num_2 = ncomp_correlate_2 * n_layer_d
!
      sig_les(1:n_layer_d,1:ncomp_correlate_2) = 0.0d0
      cor_les(1:n_layer_d,1:ncomp_correlate  ) = 0.0d0
!
      call MPI_allREDUCE ( sig_l, sig_les, num_2,                       &
     &    MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
      call MPI_allREDUCE ( cor_l, cor_les, num_1,                       &
     &    MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
!
      end subroutine sum_layerd_correlation
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sum_whole_averages
!
      use m_parallel_var_dof
!
!
      ave_wg(1:ncomp_correlate_2) = 0.0d0
      rms_wg(1:ncomp_correlate_2) = 0.0d0
!
        call MPI_allREDUCE (ave_w, ave_wg, ncomp_correlate_2,           &
     &     MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
        call MPI_allREDUCE (rms_w, rms_wg, ncomp_correlate_2,           &
     &     MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
!
      end subroutine sum_whole_averages
!
! ----------------------------------------------------------------------
!
      subroutine sum_whole_correlation
!
      use m_parallel_var_dof
!
!
      sig_wg(1:ncomp_correlate_2) = 0.0d0
      cor_wg(1:ncomp_correlate  ) = 0.0d0
!
      call MPI_allREDUCE ( sig_w, sig_wg, ncomp_correlate_2,            &
     &    MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
      call MPI_allREDUCE ( cor_w, cor_wg, ncomp_correlate,              &
     &    MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
!
      end subroutine sum_whole_correlation
!
!  ---------------------------------------------------------------------
!
      end module m_work_layer_correlate
