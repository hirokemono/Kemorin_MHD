!> @file  t_export_grp_list_extend.f90
!!      module t_export_grp_list_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine const_export_grp_list_extend                         &
!!     &         (nod_comm, pe_list_extend, grp_list_export)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(pe_list_for_marks_extend), intent(in) :: pe_list_extend
!!        type(export_grp_list_extend), intent(inout) :: grp_list_export
!!@endverbatim
!
      module t_export_grp_list_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_comm_table
      use t_pe_list_for_marks_extend
!
      implicit none
!
      type export_grp_list_extend
        integer(kind = kint), allocatable :: nset_import_recv(:)
        integer(kind = kint), allocatable :: istack_set_import_recv(:)
        integer(kind = kint), allocatable :: iset_import_recv(:,:)
      end type export_grp_list_extend
!
      private :: alloc_istack_set_import_recv, alloc_iset_import_recv
      private :: count_istack_set_import_recv, set_iset_import_recv
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_export_grp_list_extend                           &
     &         (nod_comm, pe_list_extend, grp_list_export)
!
      use calypso_mpi
!
      type(communication_table), intent(in) :: nod_comm
      type(pe_list_for_marks_extend), intent(in) :: pe_list_extend
      type(export_grp_list_extend), intent(inout) :: grp_list_export
!
!
      call alloc_istack_set_import_recv(nprocs, grp_list_export)
      call count_istack_set_import_recv                                 &
     &                         (nprocs, nod_comm, pe_list_extend,       &
     &                          grp_list_export%nset_import_recv,       &
     &                          grp_list_export%istack_set_import_recv)
!
      call alloc_iset_import_recv(nprocs, grp_list_export)
      call set_iset_import_recv(nprocs, nod_comm, pe_list_extend,       &
     &                          grp_list_export%istack_set_import_recv, &
     &                          grp_list_export%nset_import_recv,       &
     &                          grp_list_export%iset_import_recv)
!
      end subroutine const_export_grp_list_extend
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iset_import_recv(grp_list_export)
!
      type(export_grp_list_extend), intent(inout) :: grp_list_export
!
      deallocate(grp_list_export%nset_import_recv)
      deallocate(grp_list_export%istack_set_import_recv)
      deallocate(grp_list_export%iset_import_recv)
!
      end subroutine dealloc_iset_import_recv
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_set_import_recv(nprocs, grp_list_export)
!
      integer, intent(in) :: nprocs
      type(export_grp_list_extend), intent(inout) :: grp_list_export
!
!
      allocate(grp_list_export%nset_import_recv(nprocs))
      allocate(grp_list_export%istack_set_import_recv(0:nprocs))
!
      grp_list_export%istack_set_import_recv(0) = 0
!$omp parallel workshare
      grp_list_export%nset_import_recv(1:nprocs) =       0
      grp_list_export%istack_set_import_recv(1:nprocs) = 0
!$omp end parallel workshare
!
      end subroutine alloc_istack_set_import_recv
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iset_import_recv(nprocs, grp_list_export)
!
      integer, intent(in) :: nprocs
      type(export_grp_list_extend), intent(inout) :: grp_list_export
!
      integer(kind = kint) :: ntot_import_recv
!
!
      ntot_import_recv = grp_list_export%istack_set_import_recv(nprocs)
      allocate(grp_list_export%iset_import_recv(ntot_import_recv,2))
!
      if(ntot_import_recv .le. 0) return
!$omp parallel workshare
      grp_list_export%iset_import_recv(1:ntot_import_recv,1) = -1
      grp_list_export%iset_import_recv(1:ntot_import_recv,2) =  0
!$omp end parallel workshare
!
      end subroutine alloc_iset_import_recv
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_istack_set_import_recv                           &
     &         (nprocs, nod_comm, pe_list_extend,                       &
     &          nset_import_recv, istack_set_import_recv)
!
      integer, intent(in) :: nprocs
      type(communication_table), intent(in) :: nod_comm
      type(pe_list_for_marks_extend), intent(in) :: pe_list_extend
!
      integer(kind = kint), intent(inout) :: nset_import_recv(nprocs)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_set_import_recv(0:nprocs)
!
      integer(kind = kint) :: ip, icou, jp
!
      do ip = 1, nod_comm%num_neib
        do icou = 1, pe_list_extend%npe_dist_recv(ip)
          jp = pe_list_extend%irank_dist_recv(icou,ip) + 1
          nset_import_recv(jp) = nset_import_recv(jp) + 1
        end do
      end do
!
      do jp = 1, nprocs
        istack_set_import_recv(jp) = istack_set_import_recv(jp-1)       &
     &                              + nset_import_recv(jp)
      end do
!
      end subroutine count_istack_set_import_recv
!
!  ---------------------------------------------------------------------
!
      subroutine set_iset_import_recv(nprocs, nod_comm, pe_list_extend, &
     &          istack_set_import_recv, nset_import_recv,               &
     &          iset_import_recv)
!
      integer, intent(in) :: nprocs
      type(communication_table), intent(in) :: nod_comm
      type(pe_list_for_marks_extend), intent(in) :: pe_list_extend
      integer(kind = kint), intent(in)                                  &
     &      :: istack_set_import_recv(0:nprocs)
!
      integer(kind = kint), intent(inout) :: nset_import_recv(nprocs)
      integer(kind = kint), intent(inout)                               &
     &      :: iset_import_recv(istack_set_import_recv(nprocs),2)
!
      integer(kind = kint) :: ip, icou, jp, jcou
!
!$omp parallel workshare
      nset_import_recv(1:nprocs) = 0
!$omp end parallel workshare
      do ip = 1, nod_comm%num_neib
        do icou = 1, pe_list_extend%npe_dist_recv(ip)
          jp = pe_list_extend%irank_dist_recv(icou,ip) + 1
!
          nset_import_recv(jp) = nset_import_recv(jp) + 1
          jcou = nset_import_recv(jp) + istack_set_import_recv(jp-1)
          iset_import_recv(jcou,1) = ip
          iset_import_recv(jcou,2) = icou
        end do
      end do
!
      end subroutine set_iset_import_recv
!
!  ---------------------------------------------------------------------
!
      end module t_export_grp_list_extend
