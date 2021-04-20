!>@file   append_extended_element.f90
!!@brief  module append_extended_element
!!
!!@author H. Matsui
!!@date Programmed in March, 2021
!
!>@brief  Append extended sleeve element
!!
!!@verbatim
!!      subroutine s_append_extended_element(ele, add_ele_comm,         &
!!     &          trimmed_import_connect, new_ele)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: add_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in)                     &
!!     &                              :: trimmed_import_connect
!!        type(element_data), intent(inout) :: new_ele
!!@endverbatim
      module append_extended_element
!
      use m_precision
      use t_geometry_data
      use t_calypso_comm_table
      use t_mesh_for_sleeve_extend
!
      implicit none
!
      private :: add_num_extended_element, copy_eletype_to_extended
      private :: append_extended_ele_connenct
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_append_extended_element(ele, add_ele_comm,           &
     &          trimmed_import_connect, new_node, new_ele)
!
      use calypso_mpi
      use quicksort
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: add_ele_comm
      type(ele_data_for_sleeve_ext), intent(in)                         &
     &                              :: trimmed_import_connect
      type(node_data), intent(in) :: new_node
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint), allocatable :: iele_lc_tmp(:)
      integer(kind = kint), allocatable :: iele_gl_tmp(:)
      integer :: iele, icou
      logical :: flag2(2)
!
      allocate(iele_lc_tmp(add_ele_comm%ntot_import))
      allocate(iele_gl_tmp(add_ele_comm%ntot_import))
      do iele = 1, add_ele_comm%ntot_import
        iele_lc_tmp(iele) = iele
        iele_gl_tmp(iele) = trimmed_import_connect%iele_gl_comm(iele)
      end do
      call quicksort_w_index(add_ele_comm%ntot_import, iele_gl_tmp,  &
     &                   ione, add_ele_comm%ntot_import, iele_lc_tmp)
!
      icou = 0
      do iele = 2, add_ele_comm%ntot_import
        if(iele_gl_tmp(iele).eq.iele_gl_tmp(iele-1)) icou = icou+1
!        if(iele_gl_tmp(iele).eq.iele_gl_tmp(iele-1)) then
!          write(*,*) my_rank, 'address', iele_gl_tmp(iele-1:iele), &
!     &              iele_lc_tmp(iele-1:iele), &
!     &    (new_ele%ie(iele_lc_tmp(iele-1:iele),1) .le. new_node%internal_node)
!        end if
      end do
      write(*,*) my_rank, 'overlapped in add_ele_comm: ', icou
      deallocate(iele_lc_tmp, iele_gl_tmp)
!
      call add_num_extended_element(ele, add_ele_comm,                  &
     &                              new_ele%numele, new_ele%nnod_4_ele)
!
      call alloc_element_types(new_ele)
      call copy_eletype_to_extended                                     &
     &   (ele, new_ele%numele, new_ele%elmtyp, new_ele%nodelm)
!
      call alloc_ele_connectivity(new_ele)
      call append_extended_ele_connenct(ele, add_ele_comm,              &
     &    trimmed_import_connect, new_ele%numele,                       &
     &    new_ele%nnod_4_ele, new_ele%iele_global, new_ele%ie)
!
!
      allocate(iele_lc_tmp(new_ele%numele))
      allocate(iele_gl_tmp(new_ele%numele))
      do iele = 1, new_ele%numele
        iele_lc_tmp(iele) = iele
        iele_gl_tmp(iele) = new_ele%iele_global(iele)
      end do
      call quicksort_w_index(new_ele%numele, iele_gl_tmp,  &
     &                   ione, new_ele%numele, iele_lc_tmp)
!
      icou = 0
      do iele = 2, new_ele%numele
        if(iele_gl_tmp(iele).eq.iele_gl_tmp(iele-1)) icou = icou+1
!        if(iele_gl_tmp(iele).eq.iele_gl_tmp(iele-1)) then
!          write(*,*) my_rank, 'address', iele_gl_tmp(iele-1:iele), &
!     &              iele_lc_tmp(iele-1:iele), &
!     &    (new_ele%ie(iele_lc_tmp(iele-1:iele),1) .le. new_node%internal_node)
!        end if
      end do
      write(*,*) my_rank, 'overlapped in new_ele: ', icou
      deallocate(iele_lc_tmp, iele_gl_tmp)
!
      end subroutine s_append_extended_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_num_extended_element(ele, add_ele_comm,            &
     &                                   num_newele, nnod_4_ele_new)
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: add_ele_comm
!
      integer(kind = kint), intent(inout) :: num_newele
      integer(kind = kint), intent(inout) :: nnod_4_ele_new
!
!
      num_newele =     ele%numele + add_ele_comm%ntot_import
      nnod_4_ele_new = ele%nnod_4_ele
!
      end subroutine add_num_extended_element
!
!  ---------------------------------------------------------------------
!
      subroutine copy_eletype_to_extended                               &
     &         (ele, num_newele, elmtyp_new, nodelm_new)
!
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: num_newele
      integer(kind = kint), intent(inout) :: elmtyp_new(num_newele)
      integer(kind = kint), intent(inout) :: nodelm_new(num_newele)
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do
      do iele = 1, num_newele
        elmtyp_new(iele) = ele%elmtyp(1)
        nodelm_new(iele) = ele%nodelm(1)
      end do
!$omp end parallel do
!
      end subroutine copy_eletype_to_extended
!
!  ---------------------------------------------------------------------
!
      subroutine append_extended_ele_connenct                           &
     &         (ele, add_ele_comm, trimmed_import_connect,              &
     &          num_newele, nnod_4_ele_new, iele_new_global, ie_new)
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: add_ele_comm
      type(ele_data_for_sleeve_ext), intent(in)                         &
     &                              :: trimmed_import_connect
!
      integer(kind = kint), intent(in) :: num_newele, nnod_4_ele_new
!
      integer(kind = kint_gl), intent(inout)                            &
     &                        :: iele_new_global(num_newele)
      integer(kind = kint), intent(inout)                               &
     &                        :: ie_new(num_newele,nnod_4_ele_new)
!
      integer(kind = kint) :: iele, k1, inum, jele
!
!
!$omp parallel do
      do iele = 1, ele%numele
        iele_new_global(iele) = ele%iele_global(iele)
      end do
!$omp end parallel do
      do k1 = 1, ele%nnod_4_ele
!$omp parallel do
        do iele = 1, ele%numele
          ie_new(iele,k1) = ele%ie(iele,k1)
        end do
!$omp end parallel do
      end do
!
!$omp parallel do private(iele,jele)
      do iele = 1, add_ele_comm%ntot_import
        jele = iele + ele%numele
        iele_new_global(jele)                                           &
     &       = trimmed_import_connect%iele_gl_comm(iele)
      end do
!$omp end parallel do
!
      do k1 = 1, ele%nnod_4_ele
!$omp parallel do private(inum,jele)
        do inum = 1, add_ele_comm%ntot_import
          jele = inum + ele%numele
          ie_new(jele,k1) = trimmed_import_connect%ie_comm(inum,k1)
        end do
!$omp end parallel do
      end do
!
      end subroutine append_extended_ele_connenct
!
!  ---------------------------------------------------------------------
!
      end module append_extended_element
!
