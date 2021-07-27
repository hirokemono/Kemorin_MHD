!> @file  element_mark_sleeve_extend.f90
!!      module element_mark_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of marking and distance for sleeveextension
!!
!!@verbatim
!!      subroutine mark_surround_ele_of_import                          &
!!     &         (ineib, ele_comm, node, ele, iflag_node, iflag_exp_ele)
!!        integer(kind = kint), intent(in) :: ineib
!!        type(communication_table), intent(in) :: ele_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!!        integer(kind = kint), intent(inout):: iflag_exp_ele(ele%numele)
!!      subroutine ele_distance_to_mark_list                            &
!!     &        (iflag_ref, ele, each_exp_flags, iflag_exp_ele,         &
!!     &         num_marked, istack_marked_smp, idx_marked, dist_marked)
!!        integer, intent(in) :: iflag_ref
!!        type(element_data), intent(in) :: ele
!!        type(flags_each_comm_extend), intent(in) :: each_exp_flags
!!        integer(kind = kint), intent(inout):: iflag_exp_ele(ele%numele)
!!        integer(kind = kint), intent(in) :: num_marked
!!        integer(kind = kint), intent(in) :: istack_marked_smp(0:np_smp)
!!        integer(kind = kint), intent(inout) :: idx_marked(num_marked)
!!        real(kind = kreal), intent(inout) :: dist_marked(num_marked)
!!      subroutine set_ele_mark_from_mark_list                          &
!!       &         (iflag_ref, mark_ele, numele, iflag_exp_ele)
!!        integer, intent(in) :: iflag_ref
!!        type(mark_for_each_comm), intent(in) :: mark_ele
!!        integer(kind = kint), intent(in) :: numele
!!        integer(kind = kint), intent(inout) :: iflag_exp_ele(numele)
!!@endverbatim
!
      module element_mark_sleeve_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_geometry_data
      use t_comm_table
      use t_comm_table_for_each_pe
      use t_flags_each_comm_extend
      use t_mark_node_ele_to_extend
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mark_surround_ele_of_import                            &
     &         (ineib, ele_comm, node, ele, iflag_node, iflag_exp_ele)
!
      integer(kind = kint), intent(in) :: ineib
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!
      integer(kind = kint), intent(inout) :: iflag_exp_ele(ele%numele)
!
      integer(kind = kint) :: iele, k1, inod, ist, ied, inum
!
!
!$omp parallel do private(iele,k1,inod)
      do iele = 1, ele%numele
        iflag_exp_ele(iele) = 2
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          if(iflag_node(inod) .gt. -2) then
            iflag_exp_ele(iele) = 0
            exit
          end if
        end do
      end do
!$omp end parallel do
!
      ist = ele_comm%istack_import(ineib-1) + 1
      ied = ele_comm%istack_import(ineib)
!$omp parallel do private(iele,inum)
      do inum = ist, ied
        iele = ele_comm%item_import(inum)
        iflag_exp_ele(iele) = 2
      end do
!$omp end parallel do
!
      end subroutine mark_surround_ele_of_import
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ele_distance_to_mark_list                              &
     &        (iflag_ref, ele, each_exp_flags, iflag_exp_ele,           &
     &         num_marked, istack_marked_smp, idx_marked, dist_marked)
!
      integer, intent(in) :: iflag_ref
      type(element_data), intent(in) :: ele
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
      integer(kind = kint), intent(inout) :: iflag_exp_ele(ele%numele)
!
      integer(kind = kint), intent(in) :: num_marked
      integer(kind = kint), intent(in) :: istack_marked_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: idx_marked(num_marked)
      real(kind = kreal), intent(inout) :: dist_marked(num_marked)
!
      integer(kind = kint) :: ip, ist, ied, iele
      integer(kind = kint) :: inod, icou, k1
      real(kind = kreal) :: anum
!
!
      if(num_marked .le. 0) return
      anum = one / real(ele%nnod_4_ele)
!
!$omp parallel do private(ip,icou,ist,ied,iele,k1,inod)
      do ip = 1, np_smp
        icou = istack_marked_smp(ip-1)
        ist = ele%istack_ele_smp(ip-1) + 1
        ied = ele%istack_ele_smp(ip  )
        do iele = ist, ied
          if(iflag_exp_ele(iele) .eq. iflag_ref) then
            icou = icou + 1
            idx_marked(icou) = iele
            dist_marked(icou) = 0.0d0
            do k1 = 1, ele%nnod_4_ele
              inod = ele%ie(iele,k1)
              dist_marked(icou)                                         &
     &             = dist_marked(icou) + each_exp_flags%distance(inod)
            end do
            dist_marked(icou) = dist_marked(icou) * anum
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine ele_distance_to_mark_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_mark_from_mark_list                            &
     &         (iflag_ref, mark_ele, numele, iflag_exp_ele)
!
      integer, intent(in) :: iflag_ref
      type(mark_for_each_comm), intent(in) :: mark_ele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(inout) :: iflag_exp_ele(numele)
!
      integer(kind = kint) :: icou, iele
!
!
!$omp parallel do private(icou,iele)
      do icou = 1, mark_ele%num_marked
        iele = mark_ele%idx_marked(icou)
        iflag_exp_ele(iele) = iflag_ref
      end do
!$omp end parallel do
!
      end subroutine set_ele_mark_from_mark_list
!
!  ---------------------------------------------------------------------
!
      end module element_mark_sleeve_extend
