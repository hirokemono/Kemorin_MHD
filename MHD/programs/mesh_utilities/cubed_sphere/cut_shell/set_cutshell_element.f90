!
!      module set_cutshell_element
!
!     Written by H. Matsui
!
!      subroutine s_set_new_elements(org_ele, new_ele)
!
      module set_cutshell_element
!
      use m_precision
!
      use m_cutshell_nod_ele_flag
      use t_geometry_data
!
!
      implicit none
!
      private :: count_new_connect, set_new_connect
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_elements(org_ele, new_ele)
!
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
!
      call count_new_connect(org_ele, new_ele)
!
      call allocate_ele_connect_type(new_ele)
!
      call set_new_connect(org_ele, new_ele)
!
      end subroutine s_set_new_elements
!
!  ---------------------------------------------------------------------
!
      subroutine count_new_connect(org_ele, new_ele)
!
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: inod, iele, i, isig
!
!
       new_ele%nnod_4_ele = org_ele%nnod_4_ele
       new_ele%numele = 0
       do iele = 1, org_ele%numele
!
         isig = 1
         do i = 1, org_ele%nnod_4_ele
           inod = org_ele%ie(iele,i)
           if (mark_new_node(inod) .eq. 0) then
             isig = 0
             exit
           end if
         end do
!
         if ( isig .ne. 0 ) new_ele%numele = new_ele%numele + 1
       end do
!
      end subroutine count_new_connect
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_connect(org_ele, new_ele)
!
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: inod, iele, i, icou, isig
!
!
       icou = 0
       do iele = 1, org_ele%numele
!
         isig = 1
         do i = 1, org_ele%nnod_4_ele
           inod = org_ele%ie(iele,i)
           if (mark_new_node(inod) .eq. 0) then
             isig = 0
             exit
           end if
         end do
! 
         if ( isig .ne. 0 ) then
           icou = icou + 1
           new_ele%iele_global(icou) = icou
           new_ele%elmtyp(icou) = org_ele%elmtyp(iele)
           new_ele%nodelm(icou) = org_ele%nodelm(iele)
           do i = 1, org_ele%nodelm(iele)
             inod = org_ele%ie(iele,i)
             new_ele%ie(icou,i) = mark_new_node(inod)
           end do
!
           mark_new_ele(iele) = icou
         end if
       end do
!
      end subroutine set_new_connect
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_element
