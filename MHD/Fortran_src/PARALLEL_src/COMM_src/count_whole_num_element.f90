!
!      module count_whole_num_element
!
!      Written by H. Matsui on June, 2007
!
!      subroutine check_whole_num_of_elements(ele)
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
      subroutine check_whole_num_of_elements(ele)
!
      use calypso_mpi
      use m_machine_parameter
      use t_geometry_data
!
      type(element_data), intent(inout) :: ele
!
      integer (kind = kint) :: iproc, iele, ist, ied
      integer (kind = kint) :: nele_l, nele_g
      integer (kind = kint) :: nele_smp(np_smp)
!
!
      nele_g = 0
      nele_l = 0
      nele_smp = 0
!
!$omp parallel do private(iele,ist,ied)
      do iproc = 1, np_smp
        ist = ele%istack_ele_smp(iproc-1)+1
        ied = ele%istack_ele_smp(iproc)
        do iele = ist, ied
          nele_smp(iproc) = nele_smp(iproc) + ele%interior_ele(iele)
        end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        nele_l = nele_l + nele_smp(iproc)
      end do
!
      call MPI_allREDUCE ( nele_l, nele_g, 1, CALYPSO_INTEGER,          &
     &   MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (my_rank.eq.0) write(*,*)                                      &
     &      'number of element for whole domain:  ', nele_g
!
      end subroutine check_whole_num_of_elements
!
! ----------------------------------------------------------------------
!
      end module count_whole_num_element
