!
!      module count_whole_num_element
!
!      Written by H. Matsui on June, 2007
!
!      subroutine check_whole_num_of_elements
!
      module count_whole_num_element
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_whole_num_of_elements
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
!
      integer (kind = kint) :: iproc, iele
      integer (kind = kint) :: nele_l, nele_g
      integer (kind = kint) :: nele_smp(np_smp)
!
!
      nele_g = 0
      nele_l = 0
      nele_smp = 0
!
!$omp parallel do private(iele)
      do iproc = 1, np_smp
        do iele = iele_smp_stack(iproc-1)+1, iele_smp_stack(iproc)
          nele_smp(iproc) = nele_smp(iproc) + int(e_multi(iele))
        end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        nele_l = nele_l + nele_smp(iproc)
      end do
!
      call MPI_allREDUCE ( nele_l, nele_g, 1, MPI_INTEGER,              &
     &   MPI_SUM, SOLVER_COMM, ierr)
!
      if (my_rank.eq.0) write(*,*)                                      &
     &      'number of element for whole domain:  ', nele_g
!
      end subroutine check_whole_num_of_elements
!
! ----------------------------------------------------------------------
!
      end module count_whole_num_element
