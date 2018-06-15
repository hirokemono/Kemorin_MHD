!> @file  find_extended_node_and_ele.f90
!!      module find_extended_node_and_ele
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine count_nodes_by_extend_sleeve                         &
!!     &         (added_comm, org_node, new_node)
!!      subroutine set_nodes_by_extend_sleeve(added_comm, recv_nbuf,    &
!!     &          org_node, dbl_id1, new_node, dbl_id2)
!!        type(communication_table), intent(in) :: added_comm
!!        type(node_data), intent(in) :: org_node
!!        type(parallel_double_numbering), intent(in) :: dbl_id1
!!        type(node_buffer_2_extend), intent(in) :: recv_nbuf
!!        type(node_data), intent(inout) :: new_node
!!        type(parallel_double_numbering), intent(inout) :: dbl_id2
!!
!!      subroutine count_ele_by_extend_sleeve                           &
!!     &         (added_comm, org_ele, new_ele)
!!      subroutine set_ele_by_extend_sleeve(added_comm, recv_ebuf,      &
!!     &          org_ele, new_node, dbl_id2, new_ele)
!!        type(communication_table), intent(in) :: added_comm
!!        type(ele_buffer_2_extend), intent(in) :: recv_ebuf
!!        type(element_data), intent(in) :: org_ele
!!        type(node_data), intent(in) :: new_node
!!        type(parallel_double_numbering), intent(in) :: dbl_id2
!!        type(element_data), intent(inout) :: new_ele
!!
!!      subroutine check_nodes_by_extend_sleeve                         &
!!     &         (org_node, new_node, dbl_id2)
!!        type(node_data), intent(in) :: org_node
!!        type(node_data), intent(in) :: new_node
!!        type(parallel_double_numbering), intent(in) :: dbl_id2
!!@endverbatim
!
      module find_extended_node_and_ele
!
      use m_precision
      use m_constants
      use m_phys_constants
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_work_extend_comm_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_by_extend_sleeve                           &
     &         (added_comm, org_node, new_node)
!
      type(communication_table), intent(in) :: added_comm
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inum
!
!
      new_node%internal_node = org_node%internal_node
      new_node%numnod = org_node%numnod
      do inum = 1, added_comm%ntot_import
        if(added_comm%item_import(inum) .eq. 0) then
          new_node%numnod = new_node%numnod + 1
        end if
      end do
!
      end subroutine count_nodes_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_by_extend_sleeve(added_comm, recv_nbuf,      &
     &          org_node, dbl_id1, new_node, dbl_id2)
!
      type(communication_table), intent(in) :: added_comm
      type(node_data), intent(in) :: org_node
      type(parallel_double_numbering), intent(in) :: dbl_id1
      type(node_buffer_2_extend), intent(in) :: recv_nbuf
!
      type(node_data), intent(inout) :: new_node
      type(parallel_double_numbering), intent(inout) :: dbl_id2
!
      integer(kind = kint) :: inum, inod, icou
!
!
!$omp parallel do
      do inod = 1, org_node%numnod
        new_node%inod_global(inod) = org_node%inod_global(inod)
        new_node%xx(inod,1) = org_node%xx(inod,1)
        new_node%xx(inod,2) = org_node%xx(inod,2)
        new_node%xx(inod,3) = org_node%xx(inod,3)
        dbl_id2%inod_local(inod) = dbl_id1%inod_local(inod)
        dbl_id2%irank_home(inod) = dbl_id1%irank_home(inod)
      end do
!$omp end parallel do
!
      icou = org_node%numnod
      do inum = 1, added_comm%ntot_import
        if(added_comm%item_import(inum) .eq. 0) then
          icou = icou + 1
          added_comm%item_import(inum) = icou
          new_node%inod_global(icou) = recv_nbuf%inod_gl_add(inum)
          new_node%xx(icou,1) = recv_nbuf%xx_add(inum,1)
          new_node%xx(icou,2) = recv_nbuf%xx_add(inum,2)
          new_node%xx(icou,3) = recv_nbuf%xx_add(inum,3)
          dbl_id2%inod_local(icou) = recv_nbuf%inod_add(inum)
          dbl_id2%irank_home(icou) = recv_nbuf%irank_add(inum)
        end if
      end do
!
      end subroutine set_nodes_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_ele_by_extend_sleeve                             &
     &         (added_comm, org_ele, new_ele)
!
      type(communication_table), intent(in) :: added_comm
      type(element_data), intent(in) :: org_ele
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: inum
!
!
      new_ele%numele =     org_ele%numele
      new_ele%nnod_4_ele = org_ele%nnod_4_ele
      new_ele%internal_ele = org_ele%internal_ele
      do inum = 1, added_comm%ntot_import
        if(added_comm%item_import(inum) .eq. 0) then
          new_ele%numele = new_ele%numele + 1
        end if
      end do
!
      end subroutine count_ele_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_by_extend_sleeve(added_comm, recv_ebuf,        &
     &          org_ele, new_node, dbl_id2, new_ele)
!
      type(communication_table), intent(in) :: added_comm
      type(ele_buffer_2_extend), intent(in) :: recv_ebuf
      type(element_data), intent(in) :: org_ele
      type(node_data), intent(in) :: new_node
      type(parallel_double_numbering), intent(in) :: dbl_id2
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: inum, icou, ist, ied, i, iele
      integer(kind = kint) :: k1, jnod
!
!
!$omp parallel do
      do iele = 1, org_ele%numele
        new_ele%elmtyp(iele) = org_ele%elmtyp(iele)
        new_ele%nodelm(iele) = org_ele%nodelm(iele)
        new_ele%iele_global(iele) = org_ele%iele_global(iele)
      end do
!$omp end parallel do
!$omp parallel do
      do iele = org_ele%numele+1, new_ele%numele
        new_ele%elmtyp(iele) = org_ele%elmtyp(1)
        new_ele%nodelm(iele) = org_ele%nodelm(1)
      end do
!$omp end parallel do
!
!$omp parallel
      do k1 = 1, org_ele%nnod_4_ele
!$omp workshare
        new_ele%ie(1:org_ele%numele,k1) = org_ele%ie(1:org_ele%numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      icou = org_ele%numele
      do i = 1, added_comm%num_neib
        ist = added_comm%istack_import(i-1) + 1
        ied = added_comm%istack_import(i)
        do inum = ist, ied
          if(added_comm%item_import(inum) .lt. 0)  cycle
!
          icou = icou + 1
          new_ele%iele_global(icou) = recv_ebuf%iele_gl_add(inum)
          do k1 = 1, org_ele%nnod_4_ele
            do jnod = new_node%numnod, 1, -1
              if(recv_ebuf%ip_added(inum,k1)                            &
     &              .eq. dbl_id2%irank_home(jnod)                       &
     &          .and. recv_ebuf%ie_added(inum,k1)                       &
     &              .eq. dbl_id2%inod_local(jnod)) then
                new_ele%ie(icou,k1) = jnod
                exit
              end if
            end do
          end do
        end do
      end do
!
      end subroutine set_ele_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_nodes_by_extend_sleeve                           &
     &         (org_node, new_node, dbl_id2)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(in) :: new_node
      type(parallel_double_numbering), intent(in) :: dbl_id2
!
      integer(kind = kint) :: inod
!
!
      write(100+my_rank,*) new_node%numnod,                             &
     &             new_node%internal_node, org_node%numnod
      do inod = 1, new_node%numnod
        write(100+my_rank,*) inod, dbl_id2%inod_local(inod),            &
     &         dbl_id2%irank_home(inod), new_node%inod_global(inod) 
      end do
!
      end subroutine check_nodes_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      end module find_extended_node_and_ele

