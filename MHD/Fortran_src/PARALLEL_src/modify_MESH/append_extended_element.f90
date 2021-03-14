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
!!        type(communication_table), intent(in) :: add_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in)                     &
!!     &                              :: trimmed_import_connect
!!        type(element_data), intent(inout) :: new_ele
!!@endverbatim
      module append_extended_element
!
      use m_precision
      use t_comm_table
      use t_geometry_data
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
     &          trimmed_import_connect, new_ele)
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: add_ele_comm
      type(ele_data_for_sleeve_ext), intent(in)                         &
     &                              :: trimmed_import_connect
!
      type(element_data), intent(inout) :: new_ele
!
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
      end subroutine s_append_extended_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_num_extended_element(ele, add_ele_comm,            &
     &                                   num_newele, nnod_4_ele_new)
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: add_ele_comm
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
      type(communication_table), intent(in) :: add_ele_comm
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
      integer(kind = kint) :: i, iele, k1, ist, inum, jele
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
        jele = iele +  ele%numele
        iele_new_global(jele)                                           &
     &       = trimmed_import_connect%iele_gl_comm(iele)
      end do
!$omp end parallel do
!
      do k1 = 1, ele%nnod_4_ele
        do i = 1, add_ele_comm%num_neib
          ist = add_ele_comm%istack_import(i-1)
!$omp parallel do private(inum,jele)
          do inum = 1, add_ele_comm%num_import(i)
            jele = inum + ist + ele%numele
            ie_new(jele,k1)                                             &
     &           = trimmed_import_connect%ie_comm(inum+ist,k1)
          end do
!$omp end parallel do
        end do
      end do
!
      end subroutine append_extended_ele_connenct
!
!  ---------------------------------------------------------------------
!
      end module append_extended_element
!
