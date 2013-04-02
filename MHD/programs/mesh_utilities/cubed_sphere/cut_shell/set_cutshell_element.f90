!
!      module set_cutshell_element
!
      module set_cutshell_element
!
!     Written by H. Matsui
!
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_cutshell_nod_ele_flag
!
!
      implicit none
!
      private :: count_new_connect, set_new_connect
!
!      subroutine s_set_new_elements
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_elements
!
!
      call count_new_connect
!
      call allocate_2nd_element_connect
!
      call set_new_connect
!
      end subroutine s_set_new_elements
!
!  ---------------------------------------------------------------------
!
      subroutine count_new_connect
!
      integer(kind = kint) :: inod, iele, i, isig
!
!
       nnod_4_ele_2nd = nnod_4_ele
       nele_2nd = 0
       do iele = 1, numele
!
         isig = 1
         do i = 1, nnod_4_ele
           inod = ie(iele,i)
           if (mark_new_node(inod) .eq. 0) then
             isig = 0
             exit
           end if
         end do
!
         if ( isig .ne. 0 ) nele_2nd = nele_2nd + 1
!
       end do
!
      end subroutine count_new_connect
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_connect
!
      integer(kind = kint) :: inod, iele, i, icou, isig
!
!
       icou = 0
       do iele = 1, numele
!
         isig = 1
         do i = 1, nnod_4_ele
           inod = ie(iele,i)
           if (mark_new_node(inod) .eq. 0) then
             isig = 0
             exit
           end if
         end do
! 
         if ( isig .ne. 0 ) then
           icou = icou + 1
           globalelmid_2nd(icou) = icou
           elmtyp_2nd(icou) = elmtyp(iele)
           nodelm_2nd(icou) = nodelm(iele)
           do i = 1, nodelm(iele)
             inod = ie(iele,i)
             ie_2nd(icou,i) = mark_new_node(inod)
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
