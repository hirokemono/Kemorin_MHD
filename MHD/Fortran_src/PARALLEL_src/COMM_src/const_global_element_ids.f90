!>@file   const_global_element_ids.f90
!!@brief  module const_global_element_ids
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Construct global element IDs by number of internal elements
!!
!!@verbatim
!!      subroutine count_number_of_node_stack(nnod, istack_nod_list)
!!      subroutine set_global_ele_id                                    &
!!     &         (nele, istack_internal_e, internal_flag,               &
!!     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,&
!!     &          istack_export_e, item_export_e, iele_global)
!!@endverbatim
!!
      module const_global_element_ids
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_number_of_node_stack(nnod, istack_nod_list)
!
      integer(kind = kint) :: nnod
      integer(kind = kint_gl), intent(inout)                            &
     &            :: istack_nod_list(0:nprocs)
!
      integer(kind = kint), allocatable :: nnod_list_gl(:)
!
      integer(kind = kint) :: ip
!
!
      allocate(nnod_list_gl(nprocs))
      nnod_list_gl = 0
!
      call MPI_Allgather(nnod, ione, CALYPSO_INTEGER,                   &
     &    nnod_list_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
!
      istack_nod_list(0) = 0
      do ip = 1, nprocs
        istack_nod_list(ip) = istack_nod_list(ip-1) + nnod_list_gl(ip)
      end do
!
      deallocate(nnod_list_gl)
!
      end subroutine count_number_of_node_stack
!
!-----------------------------------------------------------------------
!
      subroutine set_global_ele_id                                      &
     &         (nele, istack_internal_e, internal_flag,                 &
     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,  &
     &          istack_export_e, item_export_e, iele_global)
!
      use solver_SR_int
!
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: internal_flag(nele)
      integer(kind = kint_gl), intent(in)                               &
     &        :: istack_internal_e(0:nprocs)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in)                                  &
     &        :: item_import_e(istack_import_e(num_neib_e))
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
      integer(kind = kint), intent(in)                                  &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
      integer(kind = kint_gl), intent(inout)  :: iele_global(nele)
!
      integer(kind = kint) :: iele, icou
!
!
      icou = 0
      do iele = 1, nele
        if(internal_flag(iele) .gt. 0) then
          icou = icou + 1
          iele_global(iele) = icou + istack_internal_e(my_rank)
        else
          iele_global(iele) = 0
        end if
      end do
!
      call solver_send_recv_i8                                          &
     &   (nele, num_neib_e, id_neib_e, istack_import_e, item_import_e,  &
     &    istack_export_e, item_export_e, iele_global)
!
      do iele = 1, nele
        if(iele_global(iele) .eq. 0)  write(*,*)                        &
     &        'Missing communication for element  ', iele
      end do
!
      end subroutine set_global_ele_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_element_position(nele, x_ele,                    &
     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,  &
     &          istack_export_e, item_export_e)
!
      use solver_SR_3
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in)  :: x_ele(nele,3)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in)                                  &
     &        :: item_import_e(istack_import_e(num_neib_e))
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
      integer(kind = kint), intent(in)                                  &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
!
      real(kind = kreal), parameter :: tiny = 1.0d-15
      real(kind = kreal) :: dx, dy, dz
      real(kind = kreal), allocatable :: x_test(:)
      integer(kind = kint) :: iele
!
!
      allocate(x_test(3*nele))
!
!$omp parallel do
      do iele = 1, nele
        x_test(3*iele-2) = x_ele(iele,1)
        x_test(3*iele-1) = x_ele(iele,2)
        x_test(3*iele  ) = x_ele(iele,3)
      end do
!$omp end parallel do
!
      call solver_send_recv_3                                           &
     &   (nele, num_neib_e, id_neib_e, istack_import_e, item_import_e,  &
     &    istack_export_e, item_export_e, x_test(1))
!
      do iele = 1, nele
        dx = x_test(3*iele-2) - x_ele(iele,1)
        dy = x_test(3*iele-1) - x_ele(iele,2)
        dz = x_test(3*iele  ) - x_ele(iele,3)
        if(     (abs(dx) .ge. tiny)  .or. (abs(dy) .ge. tiny)           &
     &     .or. (abs(dz) .ge. tiny)) then
          write(*,*) 'wrong element position at: ', my_rank, iele,      &
     &         x_ele(iele,1:3), dx, dy, dz
        end if
      end do
!
      deallocate(x_test)
!
      end subroutine check_element_position
!
!-----------------------------------------------------------------------
!
      end module const_global_element_ids
