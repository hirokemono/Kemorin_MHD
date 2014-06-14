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
      call allocate_ele_connect_type(ele_2nd)
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
       ele_2nd%nnod_4_ele = nnod_4_ele
       ele_2nd%numele = 0
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
         if ( isig .ne. 0 ) ele_2nd%numele = ele_2nd%numele + 1
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
           ele_2nd%iele_global(icou) = icou
           ele_2nd%elmtyp(icou) = elmtyp(iele)
           ele_2nd%nodelm(icou) = nodelm(iele)
           do i = 1, nodelm(iele)
             inod = ie(iele,i)
             ele_2nd%ie(icou,i) = mark_new_node(inod)
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
