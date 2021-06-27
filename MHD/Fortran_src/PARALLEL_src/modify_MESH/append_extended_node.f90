!>@file   append_extended_node.f90
!!@brief  module append_extended_node
!!
!!@author H. Matsui
!!@date Programmed in March, 2021
!
!>@brief  Append extended sleeve element
!!
!!@verbatim
!!      subroutine s_append_extended_node(node, inod_dbl, add_nod_comm, &
!!     &          trimmed_import_position, inod_lc_new_import_trim,     &
!!     &          new_node, new_inod_dbl)
!!        type(node_data), intent(in) :: node
!!        type(node_ele_double_number), intent(inout) :: inod_dbl
!!        type(calypso_comm_table), intent(in) :: add_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in)                    &
!!     &           :: trimmed_import_position
!!        integer(kind = kint), intent(in)                              &
!!     &           :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
!!        type(node_data), intent(inout) :: new_node
!!        type(node_ele_double_number) :: new_inod_dbl
!!@endverbatim
      module append_extended_node
!
      use m_precision
      use t_calypso_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_mesh_for_sleeve_extend
!
      implicit none
!
      private :: add_num_extended_node, append_extended_node_position
      private :: append_extended_dbl_nod_number
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_append_extended_node(node, inod_dbl, add_nod_comm,   &
     &          trimmed_import_position, inod_lc_new_import_trim,       &
     &          new_node, new_inod_dbl)
!
      type(node_data), intent(in) :: node
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(calypso_comm_table), intent(in) :: add_nod_comm
      integer(kind = kint), intent(in)                                  &
     &           :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
      type(node_data_for_sleeve_ext), intent(in)                        &
     &           :: trimmed_import_position
!
      type(node_data), intent(inout) :: new_node
      type(node_ele_double_number), intent(inout) :: new_inod_dbl
!
!
      call add_num_extended_node(node, add_nod_comm,                    &
     &    new_node%numnod, new_node%internal_node)
!
      call alloc_node_geometry_base(new_node)
      call append_extended_node_position(node, add_nod_comm,            &
     &    trimmed_import_position%inod_gl_comm,                         &
     &    trimmed_import_position%xx_comm,                              &
     &    new_node%numnod, new_node%inod_global, new_node%xx)
!
      call alloc_double_numbering(new_node%numnod, new_inod_dbl)
      call append_extended_dbl_nod_number(node, inod_dbl, add_nod_comm, &
     &    inod_lc_new_import_trim, trimmed_import_position%irank_comm,  &
     &    new_inod_dbl)
!
      end subroutine s_append_extended_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_num_extended_node(node, add_nod_comm,              &
     &                                 num_newnod, internal_newnod)
!
      type(node_data), intent(in) :: node
      type(calypso_comm_table), intent(in) :: add_nod_comm
!
      integer(kind = kint), intent(inout) :: num_newnod
      integer(kind = kint), intent(inout) :: internal_newnod
!
!
      num_newnod =      node%numnod + add_nod_comm%ntot_import
      internal_newnod = node%internal_node
!
      end subroutine add_num_extended_node
!
!  ---------------------------------------------------------------------
!
      subroutine append_extended_node_position(node, add_nod_comm,      &
     &          inod_gl_new_import_trim, xx_new_import_trim,            &
     &          num_newnod, inod_new_global, xx_new)
!
      type(node_data), intent(in) :: node
      type(calypso_comm_table), intent(in) :: add_nod_comm
      integer(kind = kint_gl), intent(in)                               &
     &           :: inod_gl_new_import_trim(add_nod_comm%ntot_import)
      real(kind = kreal), intent(in)                                    &
     &           :: xx_new_import_trim(3*add_nod_comm%ntot_import)
!
      integer(kind = kint), intent(in) :: num_newnod
!
      integer(kind = kint_gl), intent(inout)                            &
     &                        :: inod_new_global(num_newnod)
      real(kind = kreal), intent(inout) :: xx_new(num_newnod,3)
!
      integer(kind = kint) :: inod, inum, icou
!
!
!$omp parallel do
      do inod = 1, node%numnod
        inod_new_global(inod) = node%inod_global(inod)
        xx_new(inod,1) = node%xx(inod,1)
        xx_new(inod,2) = node%xx(inod,2)
        xx_new(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,icou)
      do inum = 1, add_nod_comm%ntot_import
        icou = inum + node%numnod
        inod_new_global(icou) = inod_gl_new_import_trim(inum)
        xx_new(icou,1) = xx_new_import_trim(3*inum-2)
        xx_new(icou,2) = xx_new_import_trim(3*inum-1)
        xx_new(icou,3) = xx_new_import_trim(3*inum  )
      end do
!$omp end parallel do
!
      end subroutine append_extended_node_position
!
!  ---------------------------------------------------------------------
!
      subroutine append_extended_dbl_nod_number                         &
     &         (node, inod_dbl, add_nod_comm,                           &
     &          inod_lc_new_import_trim, irank_new_import_trim,         &
     &          new_inod_dbl)
!
      type(node_data), intent(in) :: node
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(calypso_comm_table), intent(in) :: add_nod_comm
      integer(kind = kint), intent(in)                                  &
     &           :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &           :: irank_new_import_trim(add_nod_comm%ntot_import)
!
      type(node_ele_double_number), intent(inout) :: new_inod_dbl
!
      integer(kind = kint) :: inod, inum, icou
!
!
!$omp parallel do
      do inod = 1, node%numnod
        new_inod_dbl%index(inod) = inod_dbl%index(inod)
        new_inod_dbl%irank(inod) = inod_dbl%irank(inod)
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,icou)
      do inum = 1, add_nod_comm%ntot_import
        icou = inum + node%numnod
        new_inod_dbl%index(icou) = inod_lc_new_import_trim(inum)
        new_inod_dbl%irank(icou) = irank_new_import_trim(inum)
      end do
!$omp end parallel do
!
      end subroutine append_extended_dbl_nod_number
!
!  ---------------------------------------------------------------------
!
      end module append_extended_node
!
