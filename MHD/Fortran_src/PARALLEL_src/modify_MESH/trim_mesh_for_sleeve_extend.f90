!> @file  trim_mesh_for_sleeve_extend.f90
!!      module trim_mesh_for_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine trim_imported_expand_node(add_nod_comm, ext_nod_trim,&
!!     &          exp_import_xx, trim_import_xx)
!!        type(communication_table), intent(in) :: add_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        type(data_for_trim_import), intent(in) :: ext_nod_trim
!!        type(node_data_for_sleeve_ext), intent(inout) :: trim_import_xx
!!      subroutine set_trimmed_import_items                             &
!!     &         (ele, expand_ele_comm, ext_ele_trim, exp_import_ie,    &
!!     &          iele_lc_import_trim, add_ele_comm, trim_import_ie)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in) :: exp_import_ie
!!        type(data_for_trim_import), intent(in) :: ext_ele_trim
!!        integer(kind = kint), intent(inout)                           &
!!     &   :: iele_lc_import_trim(add_ele_comm%ntot_import)
!!        type(calypso_comm_table), intent(inout) :: add_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(inout) :: trim_import_ie
!!
!!      subroutine renumber_extended_ele_import(my_rank,                &
!!     &          node, ele, nod_comm, expand_nod_comm, expand_ele_comm,&
!!     &          inod_added_import, ie_new_import)
!!        integer, intent(in) :: my_rank
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        integer(kind = kint), intent(in)                              &
!!     &     :: inod_added_import(expand_nod_comm%ntot_import)
!!        integer(kind = kint), intent(inout)                           &
!!     &     :: ie_new_import(expand_ele_comm%ntot_import,ele%nnod_4_ele)
!!
!!      subroutine find_original_import_address                         &
!!     &         (node, expand_nod_comm, add_nod_comm, ext_nod_trim,    &
!!     &          idx_nod_extend_to_trimmed, inod_added_import)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(communication_table), intent(in) :: add_nod_comm
!!        type(data_for_trim_import), intent(in) :: ext_nod_trim
!!        integer(kind = kint), intent(in)                              &
!!     &        :: idx_nod_extend_to_trimmed(expand_nod_comm%ntot_import)
!!        integer(kind = kint), intent(inout)                           &
!!     &      :: inod_added_import(expand_nod_comm%ntot_import)
!!@endverbatim
!
      module trim_mesh_for_sleeve_extend
!
      use m_precision
      use t_geometry_data
      use t_comm_table
      use t_calypso_comm_table
      use t_mesh_for_sleeve_extend
      use t_trim_overlapped_import
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine trim_imported_expand_node(add_nod_comm, ext_nod_trim,  &
     &          exp_import_xx, trim_import_xx)
!
      type(calypso_comm_table), intent(in) :: add_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
      type(data_for_trim_import), intent(in) :: ext_nod_trim
!
      type(node_data_for_sleeve_ext), intent(inout) :: trim_import_xx
!
      integer(kind = kint) :: i, irank, ist, jst
      integer(kind = kint) :: inum,jcou,jnum
!
!
      do i = 1, add_nod_comm%nrank_import
        irank = add_nod_comm%irank_import(i)
        ist = ext_nod_trim%istack_trimmed_pe(irank)
        jst = add_nod_comm%istack_import(i-1)
!$omp parallel do private(inum,jcou,jnum)
        do inum = 1, add_nod_comm%num_import(i)
          jcou = inum + jst
          jnum = ext_nod_trim%idx_trimmed_to_sorted(inum+ist)
!
          trim_import_xx%irank_comm(jcou)                               &
     &              = exp_import_xx%irank_comm(jnum)
          trim_import_xx%distance(jcou)                                 &
     &              = exp_import_xx%distance(jnum)
!
          trim_import_xx%xx_comm(3*jcou-2)                              &
     &              = exp_import_xx%xx_comm(3*jnum-2)
          trim_import_xx%xx_comm(3*jcou-1)                              &
     &              = exp_import_xx%xx_comm(3*jnum-1)
          trim_import_xx%xx_comm(3*jcou  )                              &
     &              = exp_import_xx%xx_comm(3*jnum  )
          trim_import_xx%inod_gl_comm(jcou)                             &
     &              = exp_import_xx%inod_gl_comm(jnum)
        end do
!$omp end parallel do
      end do
!
      end subroutine trim_imported_expand_node
!
!  ---------------------------------------------------------------------
!
      subroutine set_trimmed_import_items                               &
     &         (ele, expand_ele_comm, ext_ele_trim, exp_import_ie,      &
     &          iele_lc_import_trim, add_ele_comm, trim_import_ie)
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: expand_ele_comm
      type(ele_data_for_sleeve_ext), intent(in) :: exp_import_ie
      type(data_for_trim_import), intent(in) :: ext_ele_trim
!
      type(calypso_comm_table), intent(inout) :: add_ele_comm
      type(ele_data_for_sleeve_ext), intent(inout) :: trim_import_ie
      integer(kind = kint), intent(inout)                               &
     &   :: iele_lc_import_trim(add_ele_comm%ntot_import)
!
      integer(kind = kint) :: i, irank, ist, jst, k1, inum, jnum, jcou
!
!
      do i = 1, add_ele_comm%nrank_import
        irank = add_ele_comm%irank_import(i)
        ist = ext_ele_trim%istack_trimmed_pe(irank)
        jst = add_ele_comm%istack_import(i-1)
!$omp parallel do private(inum,jcou,jnum,k1)
        do inum = 1, add_ele_comm%num_import(i)
          jnum = ext_ele_trim%idx_trimmed_to_sorted(inum+ist)
          jcou = inum + jst
          add_ele_comm%item_import(jcou) = jcou + ele%numele
!
          iele_lc_import_trim(jcou) = expand_ele_comm%item_import(jnum)
          trim_import_ie%irank_comm(jcou)                               &
     &              = exp_import_ie%irank_comm(jnum)
!
          trim_import_ie%iele_gl_comm(jcou)                             &
     &          = exp_import_ie%iele_gl_comm(jnum)
          do k1 = 1, ele%nnod_4_ele
            trim_import_ie%ie_comm(jcou,k1)                             &
     &          = exp_import_ie%ie_comm(jnum,k1)
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine set_trimmed_import_items
!
! ----------------------------------------------------------------------
!
      subroutine renumber_extended_ele_import(my_rank,                  &
     &          ele, nod_comm, expand_nod_comm, expand_ele_comm,  &
     &          inod_added_import, ie_new_import)
!
      integer, intent(in) :: my_rank
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(communication_table), intent(in) :: expand_ele_comm
!
      integer(kind = kint), intent(in)                                  &
     &     :: inod_added_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint), intent(inout)                               &
     &     :: ie_new_import(expand_ele_comm%ntot_import,ele%nnod_4_ele)
!
      integer(kind = kint) :: k1, i, ist_org, ist_exp, ist_ele, num
      integer(kind = kint) :: inum, jnum
!
!
!$omp parallel private(k1,i,ist_org,ist_exp,ist_ele,num)
      do k1 = 1, ele%nnod_4_ele
        do i = 1, nod_comm%num_neib
          ist_ele = expand_ele_comm%istack_import(i-1)
          num =     expand_ele_comm%istack_import(i) - ist_ele
          ist_org = nod_comm%istack_import(i-1)
          ist_exp = expand_nod_comm%istack_import(i-1)
!$omp do private(inum,jnum)
          do inum = 1, num
            jnum = ie_new_import(inum+ist_ele,k1)
            if(jnum .lt. 0) then
              ie_new_import(inum+ist_ele,k1)                            &
     &           = nod_comm%item_import(-jnum+ist_org)
            else if(jnum .gt. 0) then
              ie_new_import(inum+ist_ele,k1)                            &
     &           = inod_added_import(jnum+ist_exp)
            else
              write(*,*) my_rank, 'Failed renumber ie_new_import',      &
      &                 inum, k1
            end if
          end do
!$omp end do
        end do
      end do
!$omp end parallel
!
      end subroutine renumber_extended_ele_import
!
!  ---------------------------------------------------------------------
!
      subroutine find_original_import_address                           &
     &         (node, expand_nod_comm, add_nod_comm, ext_nod_trim,      &
     &          idx_nod_extend_to_trimmed, inod_added_import)
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: expand_nod_comm
      type(calypso_comm_table), intent(in) :: add_nod_comm
      type(data_for_trim_import), intent(in) :: ext_nod_trim
      integer(kind = kint), intent(in)                                  &
     &        :: idx_nod_extend_to_trimmed(expand_nod_comm%ntot_import)
!
      integer(kind = kint), intent(inout)                               &
     &      :: inod_added_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint) :: i, irank, ist, jst, inod, isort
      integer(kind = kint) :: inum, jnum
!
!
      do i = 1, add_nod_comm%nrank_import
        irank = add_nod_comm%irank_import(i)
        ist = ext_nod_trim%istack_trimmed_pe(irank)
        jst = add_nod_comm%istack_import(i-1)
        do inum = 1, add_nod_comm%num_import(i)
          jnum = ext_nod_trim%idx_trimmed_to_sorted(inum+ist)
          inod = inum + jst + node%numnod
          inod_added_import(jnum) = inod
        end do
      end do
!
      do jnum = 1, expand_nod_comm%ntot_import
        if(inod_added_import(jnum) .eq. 0) then
          isort = idx_nod_extend_to_trimmed(jnum)
          inod_added_import(jnum) = inod_added_import(isort)
        end if
      end do
!
      end subroutine find_original_import_address
!
!  ---------------------------------------------------------------------
!
      end module trim_mesh_for_sleeve_extend
