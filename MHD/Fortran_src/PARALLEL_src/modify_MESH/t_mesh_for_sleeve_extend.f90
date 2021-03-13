!> @file  t_mesh_for_sleeve_extend.f90
!!      module t_mesh_for_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine alloc_node_data_sleeve_ext(ntot_comm, comm_position)
!!      subroutine dealloc_node_data_sleeve_ext(comm_position)
!!        integer(kind = kint), intent(in) :: ntot_comm
!!        type(node_data_for_sleeve_ext), intent(inout) :: comm_position
!!
!!      subroutine alloc_ele_data_sleeve_ext(ntot_comm, nnod_4_ele,     &
!!     &                                     comm_connect)
!!      subroutine dealloc_ele_data_sleeve_ext(comm_connect)
!!        integer(kind = kint), intent(in) :: ntot_comm, nnod_4_ele
!!        type(ele_data_for_sleeve_ext), intent(inout) :: comm_connect
!!@endverbatim
!
      module t_mesh_for_sleeve_extend
!
      use m_precision
!
!>
      type node_data_for_sleeve_ext
!>        Numper of node
        integer(kind = kint) :: nnod_comm
!>        global node ID
        integer(kind = kint_gl), allocatable :: inod_gl_comm(:)
!>        node position in 1D-array
        real(kind = kreal), allocatable :: xx_comm(:)
!>        Home process
        integer(kind = kint), allocatable :: irank_comm(:)
!>        Minimum distance from neighboring node
        real(kind = kreal), allocatable :: distance(:)
      end type node_data_for_sleeve_ext
!
!>
      type ele_data_for_sleeve_ext
!>        Numper of node
        integer(kind = kint) :: nele_comm
!>        Numper of connectivity
        integer(kind = kint) :: nnod_4_ele
!>        global element ID
        integer(kind = kint_gl), allocatable :: iele_gl_comm(:)
!>        element connectivity
        integer(kind = kint), allocatable :: ie_comm(:,:)
!>        Home process
        integer(kind = kint), allocatable :: irank_comm(:)
      end type ele_data_for_sleeve_ext
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_node_data_sleeve_ext(ntot_comm, comm_position)
!
      integer(kind = kint), intent(in) :: ntot_comm
      type(node_data_for_sleeve_ext), intent(inout) :: comm_position
!
!
      comm_position%nnod_comm = ntot_comm
!
      allocate(comm_position%inod_gl_comm(comm_position%nnod_comm))
      allocate(comm_position%irank_comm(comm_position%nnod_comm))
      allocate(comm_position%distance(comm_position%nnod_comm))
      allocate(comm_position%xx_comm(3*comm_position%nnod_comm))
!
      if(comm_position%nnod_comm .le. 0) return
!
!$omp parallel workshare
      comm_position%inod_gl_comm(1:comm_position%nnod_comm) = 0
      comm_position%irank_comm(1:comm_position%nnod_comm) = -1
      comm_position%distance(1:comm_position%nnod_comm) =  0.0d0
      comm_position%xx_comm(1:3*comm_position%nnod_comm) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_node_data_sleeve_ext
!
! ----------------------------------------------------------------------
!
      subroutine alloc_ele_data_sleeve_ext(ntot_comm, nnod_4_ele,       &
     &                                     comm_connect)
!
      integer(kind = kint), intent(in) :: ntot_comm, nnod_4_ele
      type(ele_data_for_sleeve_ext), intent(inout) :: comm_connect
!
!
      comm_connect%nele_comm =  ntot_comm
      comm_connect%nnod_4_ele = nnod_4_ele
!
      allocate(comm_connect%iele_gl_comm(comm_connect%nele_comm))
      allocate(comm_connect%ie_comm(comm_connect%nele_comm,nnod_4_ele))
      allocate(comm_connect%irank_comm(comm_connect%nele_comm))
!
      if(comm_connect%nele_comm .le. 0) return
!
!$omp parallel workshare
      comm_connect%iele_gl_comm(1:comm_connect%nele_comm) =       0
      comm_connect%irank_comm(1:comm_connect%nele_comm) =        -1
!$omp end parallel workshare
!
!$omp parallel workshare
      comm_connect%ie_comm(1:comm_connect%nele_comm,1:nnod_4_ele) = 0
!$omp end parallel workshare
!
      end subroutine alloc_ele_data_sleeve_ext
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_node_data_sleeve_ext(comm_position)
!
      type(node_data_for_sleeve_ext), intent(inout) :: comm_position
!
!
      if(allocated(comm_position%xx_comm) .eqv. .FALSE.) return
!
      deallocate(comm_position%inod_gl_comm)
      deallocate(comm_position%irank_comm)
      deallocate(comm_position%distance)
      deallocate(comm_position%xx_comm)
!
      end subroutine dealloc_node_data_sleeve_ext
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_ele_data_sleeve_ext(comm_connect)
!
      type(ele_data_for_sleeve_ext), intent(inout) :: comm_connect
!
!
      if(allocated(comm_connect%ie_comm) .eqv. .FALSE.) return
!
      deallocate(comm_connect%iele_gl_comm)
      deallocate(comm_connect%ie_comm)
      deallocate(comm_connect%irank_comm)
!
      end subroutine dealloc_ele_data_sleeve_ext
!
! ----------------------------------------------------------------------
!
      end module t_mesh_for_sleeve_extend
