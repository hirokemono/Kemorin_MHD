!set_refine_flags_4_tri.f90
!      module set_refine_flags_4_tri
!
      module set_refine_flags_4_tri
!
!      Writen by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable, private :: imark_nod(:)
      integer(kind = kint), allocatable, private :: imark_ele(:)
      integer(kind = kint), allocatable, private :: imark_tmp(:)
!
      private :: set_refine_id_for_n1
      private :: set_refine_id_for_n2, set_refine_id_for_n4
      private :: set_refine_id_for_n3, set_refine_id_for_n6
      private :: change_refine_id_for_n5, change_refine_id_for_n7
      private :: set_refine_id_for_stri_n4, set_refine_id_for_stri_n2
!
!      subroutine s_set_refine_flags_4_tri
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refine_flags_4_tri
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
      use m_refined_element_data
!
      integer(kind = kint) :: inum, iele, icou, iflag
!
!
      allocate(imark_ele(numele))
      imark_ele = 0
!
      if(iflag_tmp_tri_refine .eq. 0) then
        write(*,*) 'mark_refine_node_flag '
        call mark_refine_node_flag
      else
        write(*,*) 'redefine_refine_node_flag '
        call redefine_refine_node_flag
      end if
!
      icou = 0
      iflag = 1
      do while (iflag .gt. 0)
        call remark_refine_data(iflag)
        icou = icou + 1
        write(*,*) 'remark_refine_data end ', icou, iflag
      end do
!
      write(*,*) 'remark_refine_data finish: ', icou
!
      if(iflag_debug .gt. 0) then
!
       write(50,'(a)') 'error nummber of marks:  0'
       do iele = 1, numele
        if(imark_ele(iele) .eq. 0) then
          icou = icou + 1
          if( iflag_refine_ele(iele) .ne. 0) then
            write(50,'(i10,8i3,i6)') iele, imark_nod(ie(iele,1:8)),     &
     &                            iflag_refine_ele(iele)
          end if
        end if
       end do
!
        inum = 3
         write(50,'(a,i2)') 'nummber of marks:  ', inum
         do iele = 1, numele
          if(imark_ele(iele) .eq. inum) then
            write(50,'(i10,8i3,i6)') iele, imark_nod(ie(iele,1:8)),     &
     &                            iflag_refine_ele(iele)
          end if
         end do
!
       do inum = 5, 7
        write(50,'(a,i2)') 'nummber of marks:  ', inum
        do iele = 1, numele
          if(imark_ele(iele) .eq. inum) then
            write(50,'(i10,8i3,i6)') iele, imark_nod(ie(iele,1:8)),     &
     &                            iflag_refine_ele(iele)
          end if
        end do
       end do
!
       icou = 0
       write(50,'(a)') 'error nummber of marks:  8'
       do iele = 1, numele
        if(imark_ele(iele) .eq. 8) then
          icou = icou + 1
          if( iflag_refine_ele(iele) .ne. 300) then
            write(50,'(i10,8i3,i6)') iele, imark_nod(ie(iele,1:8)),     &
     &                            iflag_refine_ele(iele)
          end if
        end if
       end do
       write(50,'(a,2i10)') 'total refine ele: ', icou, numele
      end if
!
      write(*,*) 'deallocate imark_ele'
      deallocate(imark_ele)
!
      end subroutine s_set_refine_flags_4_tri
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine remark_refine_data(iflag_retry)
!
      use m_control_param_4_refiner
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
      use m_refined_element_data
!
      integer(kind = kint), intent(inout) :: iflag_retry
      integer(kind = kint) :: iele, k1, inod
!
!
      imark_ele = 0
      do iele = 1, numele
        do k1 = 1, 8
          inod = ie(iele,k1)
          imark_ele(iele) = imark_ele(iele) + imark_nod(inod)
        end do
      end do
!
      write(*,*) 'marking finish'
!
      iflag_retry = 0
      do iele = 1, numele
        if(imark_ele(iele) .eq. 5) then
          call change_refine_id_for_n5(iele,iflag_retry)
        end if
      end do
      write(*,*) 'n=5 fixed'
!
      do iele = 1, numele
        if(imark_ele(iele) .eq. 7) then
          call change_refine_id_for_n7(iele, iflag_retry)
        end if
      end do
      write(*,*) 'n=7 fixed'
!
      do iele = 1, numele
        if(imark_ele(iele) .eq. 2) then
            call set_refine_id_for_n2(iele, iflag_refine_ele(iele),     &
     &          iflag_retry)
        else if(imark_ele(iele) .eq. 3) then
          call set_refine_id_for_n3(iele, iflag_refine_ele(iele))
        else if(imark_ele(iele) .eq. 6) then
          call set_refine_id_for_n6(iele, iflag_refine_ele(iele),       &
     &        iflag_retry)
        end if
      end do
      write(*,*) 'fifth marking finished'
!
      if (iflag_retry .gt. 0) return
!
      iflag_tmp_tri_refine = 0
      do iele = 1, numele
        if( iflag_refine_ele(iele) .ge. iflag_five_x                    &
     &   .and. iflag_refine_ele(iele) .le. iflag_five_s6) then
          iflag_tmp_tri_refine = 1
          exit
        end if
      end do
!
      if(iflag_tmp_tri_refine .eq. 0) then
        if (iflag_small_tri_refine .eq.1)  then
          do iele = 1, numele
            if(imark_ele(iele) .eq. 1) then
              iflag_refine_ele(iele) = iflag_nothing
            else if(imark_ele(iele) .eq. 2) then
              call set_refine_id_for_stri_n2(iele,                      &
     &            iflag_refine_ele(iele), iflag_retry)
            else if(imark_ele(iele) .eq. 4) then
              call set_refine_id_for_stri_n4(iele,                      &
     &            iflag_refine_ele(iele) )
            else if(imark_ele(iele) .eq. 8) then
              iflag_refine_ele(iele) = iflag_tri_full_eq
            end if
          end do
!
        else
!
          do iele = 1, numele
            if(imark_ele(iele) .eq. 1) then
              call set_refine_id_for_n1(iele, iflag_refine_ele(iele))
            else if(imark_ele(iele) .eq. 2) then
              call set_refine_id_for_n2(iele, iflag_refine_ele(iele),   &
     &          iflag_retry)
            else if(imark_ele(iele) .eq. 4) then
              call set_refine_id_for_n4(iele, iflag_refine_ele(iele))
            else if(imark_ele(iele) .eq. 8) then
              iflag_refine_ele(iele) = iflag_tri_full
            end if
          end do
        end if
!
      else
!
        do iele = 1, numele
          if(    imark_ele(iele) .eq. 1 .or. imark_ele(iele) .eq. 2     &
     &      .or. imark_ele(iele) .eq. 4 .or. imark_ele(iele) .eq. 8     &
     &      ) then
            iflag_refine_ele(iele) = iflag_nothing
          end if
        end do
      end if
!
      end subroutine remark_refine_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_refine_id_for_n1(iele, iflag_refine_ele)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_refine_ele
      integer(kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
!
      i1 = ie(iele,1)
      i2 = ie(iele,2)
      i3 = ie(iele,3)
      i4 = ie(iele,4)
      i5 = ie(iele,5)
      i6 = ie(iele,6)
      i7 = ie(iele,7)
      i8 = ie(iele,8)
!
      if(imark_nod(i1) .gt. 0) iflag_refine_ele = iflag_tri_n1
      if(imark_nod(i2) .gt. 0) iflag_refine_ele = iflag_tri_n2
      if(imark_nod(i3) .gt. 0) iflag_refine_ele = iflag_tri_n3
      if(imark_nod(i4) .gt. 0) iflag_refine_ele = iflag_tri_n4
      if(imark_nod(i5) .gt. 0) iflag_refine_ele = iflag_tri_n5
      if(imark_nod(i6) .gt. 0) iflag_refine_ele = iflag_tri_n6
      if(imark_nod(i7) .gt. 0) iflag_refine_ele = iflag_tri_n7
      if(imark_nod(i8) .gt. 0) iflag_refine_ele = iflag_tri_n8
!
      end subroutine set_refine_id_for_n1
!
!  ---------------------------------------------------------------------
!
      subroutine set_refine_id_for_n2(iele, iflag_refine_ele,           &
     &          iflag_retry)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_retry
      integer(kind = kint), intent(inout) :: iflag_refine_ele
      integer(kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
!
      i1 = ie(iele,1)
      i2 = ie(iele,2)
      i3 = ie(iele,3)
      i4 = ie(iele,4)
      i5 = ie(iele,5)
      i6 = ie(iele,6)
      i7 = ie(iele,7)
      i8 = ie(iele,8)
!
      if((imark_nod(i1)*imark_nod(i2)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e1
!
      else if((imark_nod(i2)*imark_nod(i3)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e2
!
      else if((imark_nod(i3)*imark_nod(i4)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e3
!
      else if((imark_nod(i4)*imark_nod(i1)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e4
!
      else if((imark_nod(i5)*imark_nod(i6)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e5
!
      else if((imark_nod(i6)*imark_nod(i7)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e6
!
      else if((imark_nod(i7)*imark_nod(i8)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e7
!
      else if((imark_nod(i8)*imark_nod(i5)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e8
!
      else if((imark_nod(i1)*imark_nod(i5)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e9
!
      else if((imark_nod(i2)*imark_nod(i6)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e10
!
      else if((imark_nod(i3)*imark_nod(i7)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e11
!
      else if((imark_nod(i4)*imark_nod(i8)) .gt. 0) then
        iflag_refine_ele = iflag_tri_e12
!
      else
        iflag_retry = 1
        imark_nod(i1) = 0
        imark_nod(i2) = 0
        imark_nod(i3) = 0
        imark_nod(i4) = 0
        imark_nod(i5) = 0
        imark_nod(i6) = 0
        imark_nod(i7) = 0
        imark_nod(i8) = 0
      end if
!
      end subroutine set_refine_id_for_n2
!
!  ---------------------------------------------------------------------
!
      subroutine set_refine_id_for_n4(iele, iflag_refine_ele)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_refine_ele
      integer(kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8, isig
!
!
      i1 = ie(iele,1)
      i2 = ie(iele,2)
      i3 = ie(iele,3)
      i4 = ie(iele,4)
      i5 = ie(iele,5)
      i6 = ie(iele,6)
      i7 = ie(iele,7)
      i8 = ie(iele,8)
!
      isig =  imark_nod(i1)*imark_nod(i5)*imark_nod(i8)*imark_nod(i4)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_s1
      isig =  imark_nod(i2)*imark_nod(i3)*imark_nod(i6)*imark_nod(i7)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_s2
      isig =  imark_nod(i1)*imark_nod(i2)*imark_nod(i5)*imark_nod(i6)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_s3
      isig =  imark_nod(i3)*imark_nod(i4)*imark_nod(i7)*imark_nod(i8)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_s4
      isig =  imark_nod(i1)*imark_nod(i2)*imark_nod(i3)*imark_nod(i4)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_s5
      isig =  imark_nod(i5)*imark_nod(i6)*imark_nod(i7)*imark_nod(i8)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_s6
!
      end subroutine set_refine_id_for_n4
!
!  ---------------------------------------------------------------------
!
      subroutine set_refine_id_for_stri_n2(iele, iflag_refine_ele,      &
     &          iflag_retry)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_retry
      integer(kind = kint), intent(inout) :: iflag_refine_ele
      integer(kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
!
      i1 = ie(iele,1)
      i2 = ie(iele,2)
      i3 = ie(iele,3)
      i4 = ie(iele,4)
      i5 = ie(iele,5)
      i6 = ie(iele,6)
      i7 = ie(iele,7)
      i8 = ie(iele,8)
!
      if((imark_nod(i1)*imark_nod(i2)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e1
!
      else if((imark_nod(i2)*imark_nod(i3)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e2
!
      else if((imark_nod(i3)*imark_nod(i4)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e3
!
      else if((imark_nod(i4)*imark_nod(i1)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e4
!
      else if((imark_nod(i5)*imark_nod(i6)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e5
!
      else if((imark_nod(i6)*imark_nod(i7)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e6
!
      else if((imark_nod(i7)*imark_nod(i8)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e7
!
      else if((imark_nod(i8)*imark_nod(i5)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e8
!
      else if((imark_nod(i1)*imark_nod(i5)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e9
!
      else if((imark_nod(i2)*imark_nod(i6)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e10
!
      else if((imark_nod(i3)*imark_nod(i7)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e11
!
      else if((imark_nod(i4)*imark_nod(i8)) .gt. 0) then
        iflag_refine_ele = iflag_stri_e12
!
      else
        iflag_retry = 1
        imark_nod(i1) = 0
        imark_nod(i2) = 0
        imark_nod(i3) = 0
        imark_nod(i4) = 0
        imark_nod(i5) = 0
        imark_nod(i6) = 0
        imark_nod(i7) = 0
        imark_nod(i8) = 0
      end if
!
      end subroutine set_refine_id_for_stri_n2
!
!  ---------------------------------------------------------------------
!
      subroutine set_refine_id_for_stri_n4(iele, iflag_refine_ele)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_refine_ele
      integer(kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8, isig
!
!
      i1 = ie(iele,1)
      i2 = ie(iele,2)
      i3 = ie(iele,3)
      i4 = ie(iele,4)
      i5 = ie(iele,5)
      i6 = ie(iele,6)
      i7 = ie(iele,7)
      i8 = ie(iele,8)
!
      isig =  imark_nod(i1)*imark_nod(i5)*imark_nod(i8)*imark_nod(i4)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_xs1
      isig =  imark_nod(i2)*imark_nod(i3)*imark_nod(i6)*imark_nod(i7)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_xs2
      isig =  imark_nod(i1)*imark_nod(i2)*imark_nod(i5)*imark_nod(i6)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_ys3
      isig =  imark_nod(i3)*imark_nod(i4)*imark_nod(i7)*imark_nod(i8)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_ys4
      isig =  imark_nod(i1)*imark_nod(i2)*imark_nod(i3)*imark_nod(i4)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_zs5
      isig =  imark_nod(i5)*imark_nod(i6)*imark_nod(i7)*imark_nod(i8)
      if(isig .gt. 0) iflag_refine_ele = iflag_tri_zs6
!
      end subroutine set_refine_id_for_stri_n4
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_refine_id_for_n3(iele, iflag_refine_ele)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_refine_ele
      integer(kind = kint) :: js1, isf, k1, inod
      integer(kind = kint) :: isig(6)
!
!
      isig = 0
      do js1 = 1, nsurf_4_ele
        do isf = 1, num_linear_sf
          k1 = node_on_sf_4(isf,js1)
          inod = ie(iele,k1)
          isig(js1) = isig(js1) + imark_nod(inod)
        end do
      end do
!
      if(isig(1) .eq. 3) then
        iflag_refine_ele = iflag_five_s1
      else if(isig(2) .eq. 3) then
        iflag_refine_ele = iflag_five_s2
      else if(isig(3) .eq. 3) then
        iflag_refine_ele = iflag_five_s3
      else if(isig(4) .eq. 3) then
        iflag_refine_ele = iflag_five_s4
      else if(isig(5) .eq. 3) then
        iflag_refine_ele = iflag_five_s5
      else if(isig(6) .eq. 3) then
        iflag_refine_ele = iflag_five_s6
      end if
!
      end subroutine set_refine_id_for_n3
!
!  ---------------------------------------------------------------------
!
      subroutine set_refine_id_for_n6(iele, iflag_refine_ele,           &
     &          iflag_retry)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_refine_ele
      integer(kind = kint), intent(inout) :: iflag_retry
      integer(kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
!
      i1 = ie(iele,1)
      i2 = ie(iele,2)
      i3 = ie(iele,3)
      i4 = ie(iele,4)
      i5 = ie(iele,5)
      i6 = ie(iele,6)
      i7 = ie(iele,7)
      i8 = ie(iele,8)
!
      if((imark_nod(i1)+imark_nod(i2)) .eq. 0) then
        iflag_refine_ele = iflag_five_x
!
      else if((imark_nod(i2)+imark_nod(i3)) .eq. 0) then
        iflag_refine_ele = iflag_five_y
!
      else if((imark_nod(i3)+imark_nod(i4)) .eq. 0) then
        iflag_refine_ele = iflag_five_x
!
      else if((imark_nod(i4)+imark_nod(i1)) .eq. 0) then
        iflag_refine_ele = iflag_five_y
!
      else if((imark_nod(i5)+imark_nod(i6)) .eq. 0) then
        iflag_refine_ele = iflag_five_x
!
      else if((imark_nod(i6)+imark_nod(i7)) .eq. 0) then
        iflag_refine_ele = iflag_five_y
!
      else if((imark_nod(i7)+imark_nod(i8)) .eq. 0) then
        iflag_refine_ele = iflag_five_x
!
      else if((imark_nod(i8)+imark_nod(i5)) .eq. 0) then
        iflag_refine_ele = iflag_five_y
!
      else if((imark_nod(i1)+imark_nod(i5)) .eq. 0) then
        iflag_refine_ele = iflag_five_z
!
      else if((imark_nod(i2)+imark_nod(i6)) .eq. 0) then
        iflag_refine_ele = iflag_five_z
!
      else if((imark_nod(i3)+imark_nod(i7)) .eq. 0) then
        iflag_refine_ele = iflag_five_z
!
      else if((imark_nod(i4)+imark_nod(i8)) .eq. 0) then
        iflag_refine_ele = iflag_five_z
!
      else
        iflag_retry = 1
        imark_nod(i1) = 1
        imark_nod(i2) = 1
        imark_nod(i3) = 1
        imark_nod(i4) = 1
        imark_nod(i5) = 1
        imark_nod(i6) = 1
        imark_nod(i7) = 1
        imark_nod(i8) = 1
      end if
!
      end subroutine set_refine_id_for_n6
!
!  ---------------------------------------------------------------------
!
      subroutine change_refine_id_for_n5(iele, iflag_retry)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_retry
!
      integer(kind = kint) :: js1, isf, k1, inod
      integer(kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
      integer(kind = kint) :: isig(6)
!
!
      i1 = ie(iele,1)
      i2 = ie(iele,2)
      i3 = ie(iele,3)
      i4 = ie(iele,4)
      i5 = ie(iele,5)
      i6 = ie(iele,6)
      i7 = ie(iele,7)
      i8 = ie(iele,8)
!
      isig = 0
      do js1 = 1, nsurf_4_ele
        do isf = 1, num_linear_sf
          k1 = node_on_sf_4(isf,js1)
          inod = ie(iele,k1)
          isig(js1) = isig(js1) + imark_nod(inod)
        end do
      end do
!
      if(isig(1) .eq. 4) then
        if(imark_nod(i2) .gt. 0) then
          imark_nod(i3) = 1
        else if(imark_nod(i3) .gt. 0) then
          imark_nod(i2) = 1
        else if(imark_nod(i6) .gt. 0) then
          imark_nod(i7) = 1
        else if(imark_nod(i7) .gt. 0) then
          imark_nod(i6) = 1
        end if
      else if(isig(2) .eq. 4) then
        if(imark_nod(i1) .gt. 0) then
          imark_nod(i5) = 1
        else if(imark_nod(i5) .gt. 0) then
          imark_nod(i1) = 1
        else if(imark_nod(i4) .gt. 0) then
          imark_nod(i8) = 1
        else if(imark_nod(i8) .gt. 0) then
          imark_nod(i4) = 1
        end if
      else if(isig(3) .eq. 4) then
        if(imark_nod(i3) .gt. 0) then
          imark_nod(i4) = 1
        else if(imark_nod(i4) .gt. 0) then
          imark_nod(i3) = 1
        else if(imark_nod(i7) .gt. 0) then
          imark_nod(i8) = 1
        else if(imark_nod(i8) .gt. 0) then
          imark_nod(i7) = 1
        end if
      else if(isig(4) .eq. 4) then
        if(imark_nod(i1) .gt. 0) then
          imark_nod(i2) = 1
        else if(imark_nod(i2) .gt. 0) then
          imark_nod(i1) = 1
        else if(imark_nod(i5) .gt. 0) then
          imark_nod(i6) = 1
        else if(imark_nod(i6) .gt. 0) then
          imark_nod(i5) = 1
        end if
      else if(isig(5) .eq. 4) then
        if(imark_nod(i5) .gt. 0) then
          imark_nod(i6) = 1
        else if(imark_nod(i6) .gt. 0) then
          imark_nod(i5) = 1
        else if(imark_nod(i7) .gt. 0) then
          imark_nod(i8) = 1
        else if(imark_nod(i8) .gt. 0) then
          imark_nod(i7) = 1
        end if
      else if(isig(6) .eq. 4) then
        if(imark_nod(i1) .gt. 0) then
          imark_nod(i2) = 1
        else if(imark_nod(i2) .gt. 0) then
          imark_nod(i1) = 1
        else if(imark_nod(i3) .gt. 0) then
          imark_nod(i4) = 1
        else if(imark_nod(i4) .gt. 0) then
          imark_nod(i3) = 1
        end if
      end if
!
      imark_ele(iele) = 6
      iflag_retry = 1
!
      end subroutine change_refine_id_for_n5
!
!  ---------------------------------------------------------------------
!
      subroutine change_refine_id_for_n7(iele, iflag_retry)
!
      use m_geometry_parameter
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(inout) :: iflag_retry
      integer(kind = kint) :: k1, inod
!
!
      do k1 = 1, 8
        inod = ie(iele,k1)
        imark_nod(inod) = 1
      end do
      imark_ele(iele) = 8
      iflag_retry = 1
!
      end subroutine change_refine_id_for_n7
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mark_refine_node_flag
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refine_flag_parameters
      use m_refined_element_data
!
      integer(kind = kint) :: iele, k1, inod
!
!
      allocate(imark_nod(numnod))
      imark_nod = 0
!
      imark_nod(inod) = 0
      do iele = 1, numele
        if(iflag_refine_ele(iele) .eq. iflag_tri_full) then
          do k1 = 1, 8
            inod = ie(iele,k1)
            imark_nod(inod) = 1
          end do
        end if
      end do
!
      end subroutine mark_refine_node_flag
!
!  ---------------------------------------------------------------------
!
      subroutine redefine_refine_node_flag
!
      use m_geometry_parameter
!
      integer(kind = kint) :: num
!
!
      num = size(imark_nod)
      allocate(imark_tmp(num))
      imark_tmp(1:num) = imark_nod(1:num)
!
      deallocate(imark_nod)
      allocate(imark_nod(numnod))
      imark_nod = 0
!
      imark_nod(1:num) = imark_tmp(1:num)
      deallocate(imark_tmp)
!
      end subroutine redefine_refine_node_flag
!
!  ---------------------------------------------------------------------
!
      end module set_refine_flags_4_tri
