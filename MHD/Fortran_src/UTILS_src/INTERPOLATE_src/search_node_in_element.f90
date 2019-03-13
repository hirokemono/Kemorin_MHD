!
!      module search_node_in_element
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine search_node_in_element_1st(id_rank,                  &
!!     &          org_node, org_ele, org_blk, dest_node, itp_coef_dest)
!!      subroutine search_node_in_element_2nd(iinc, id_rank,            &
!!     &          org_node, org_ele, org_blk, dest_node, itp_coef_dest)
!!      subroutine search_node_in_all_element(my_rank_2nd, error_level  &
!!     &          org_node, org_ele, dest_node, itp_coef_dest)
!!      subroutine giveup_to_search_element(my_rank_2nd, error_level,   &
!!     &          inod_next_stack_4_node, org_node, org_ele, dest_node, &
!!     &          itp_coef_dest)
!!       type(node_data), intent(in) :: org_node
!!       type(element_data), intent(in) :: org_ele
!!       type(block_4_interpolate), intent(in) :: org_blk
!!       type(node_data), intent(in) :: dest_node
!!       type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      module search_node_in_element
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_work_const_itp_table
      use cal_interpolate_coefs
!
      use m_search_bolck_4_itp
      use t_geometry_data
      use t_interpolate_coefs_dest
!
      implicit none
!
      integer(kind = kint) :: iflag_org_tmp
      private :: iflag_org_tmp
!
! ----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine search_node_in_element_1st(id_rank,                    &
     &          org_node, org_ele, org_blk, dest_node, itp_coef_dest)
!
      integer, intent(in) :: id_rank
!
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(block_4_interpolate), intent(in) :: org_blk
!
      type(node_data), intent(in) :: dest_node
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      integer(kind = kint) :: ip, ist, ied, inod
      integer(kind = kint) :: ihash, jst, jed, jnum, jele
      integer(kind = kint), parameter :: iflag_nomessage = 0
!
!
      do ip = 1, np_smp
        ist = dest_node%istack_internal_smp(ip-1) + 1
        ied = dest_node%istack_internal_smp(ip)
!
        do inod = ist, ied
          ihash = org_blk%iblock_tgt_node(inod,4)
!
          if(ihash .gt. 0) then
            jst = org_blk%ele_list_by_rng%istack_grp(ihash-1) + 1
            jed = org_blk%ele_list_by_rng%istack_grp(ihash)
!
            do jnum = jst, jed
              jele = org_blk%ele_list_by_rng%item_grp(jnum)
!
              if (   dest_node%xx(inod,1) .ge. org_blk%xele_min(jele,1) &
     &         .and. dest_node%xx(inod,1) .le. org_blk%xele_max(jele,1) &
     &         .and. dest_node%xx(inod,2) .ge. org_blk%xele_min(jele,2) &
     &         .and. dest_node%xx(inod,2) .le. org_blk%xele_max(jele,2) &
     &         .and. dest_node%xx(inod,3) .ge. org_blk%xele_min(jele,3) &
     &         .and. dest_node%xx(inod,3) .le. org_blk%xele_max(jele,3) &
     &              ) then
!
                 call s_cal_interpolate_coefs                           &
     &              (dest_node, org_node, org_ele, id_rank,             &
     &               inod, jele, zero, iflag_nomessage,                 &
     &               iflag_org_tmp, itp_coef_dest)
                 if ( iflag_org_domain(inod) .gt. 0) go to 10
               end if
!
            end do
          end if
 10       continue
        end do
!
      end do
!
      end subroutine search_node_in_element_1st
!
!-----------------------------------------------------------------------
!
      subroutine search_node_in_element_2nd(iinc, id_rank,              &
     &          org_node, org_ele, org_blk, dest_node, itp_coef_dest)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iinc
!
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(block_4_interpolate), intent(in) :: org_blk
!
      type(node_data), intent(in) :: dest_node
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      integer(kind = kint) :: ip, ist, ied, inod, i1, i2, i3
      integer(kind = kint) :: igh(3)
      integer(kind = kint) :: ihash, jst, jed, jnum, jele
      integer(kind = kint), parameter :: iflag_nomessage = 0
!
!
      do ip = 1, np_smp
        ist = dest_node%istack_internal_smp(ip-1) + 1
        ied = dest_node%istack_internal_smp(ip)
!
        do inod = ist, ied
          igh(1:3) = org_blk%iblock_tgt_node(inod,1:3)
          do i1 = igh(1)-iinc+1, igh(1)+iinc-1, iinc
            if(i1.lt.1 .or. i1.gt.org_blk%num_itp_block(1)) cycle
           do i2 = igh(2)-iinc+1, igh(2)+iinc-1, iinc
             if(i2.lt.1 .or. i2.gt.org_blk%num_itp_block(2)) cycle
            do i3 = igh(3)-iinc+1, igh(3)+iinc-1, iinc
              if(i3.lt.1 .or. i3.gt.org_blk%num_itp_block(3)) cycle
              ihash =  i1 + (i2 - 1) * org_blk%num_itp_block(1)      &
     &                + (i3 - 1) * org_blk%num_itp_block(1)          &
     &                           * org_blk%num_itp_block(2)
!
          if(ihash .gt. 0) then
            jst = org_blk%ele_list_by_rng%istack_grp(ihash-1) + 1
            jed = org_blk%ele_list_by_rng%istack_grp(ihash)
!
            do jnum = jst, jed
              jele = org_blk%ele_list_by_rng%item_grp(jnum)
!
                call s_cal_interpolate_coefs                            &
     &              (dest_node, org_node, org_ele, id_rank,             &
     &               inod, jele, zero, iflag_nomessage,                 &
     &               iflag_org_tmp, itp_coef_dest)
                if ( iflag_org_domain(inod) .gt. 0) go to 10
!
            end do
          end if
            end do
            end do
            end do
 10       continue
        end do
!
      end do
!
      end subroutine search_node_in_element_2nd
!
!-----------------------------------------------------------------------
!
      subroutine search_node_in_all_element(my_rank_2nd, error_level,   &
     &          org_node, org_ele, dest_node, itp_coef_dest)
!
      integer(kind = kint), intent(in) :: my_rank_2nd
      real(kind = kreal), intent(in) :: error_level
!
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
!
      type(node_data), intent(in) :: dest_node
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      integer(kind = kint) :: ip, ist, ied, inod, jele
      integer(kind = kint), parameter :: iflag_message = 1
!
!
      do ip = 1, np_smp
        ist = dest_node%istack_internal_smp(ip-1) + 1
        ied = dest_node%istack_internal_smp(ip)
!
        do inod = ist, ied
!
          if ( iflag_org_domain(inod) .le. 0) then
!
            differ_tmp = 1.0d20
            iflag_org_tmp = 0
            do jele = 1, org_ele%numele
!
              call s_cal_interpolate_coefs                              &
     &           (dest_node, org_node, org_ele, my_rank_2nd,            &
     &            inod, jele,  error_level, iflag_message,              &
     &            iflag_org_tmp, itp_coef_dest)
              if ( iflag_org_domain(inod) .gt. 0) go to  10
            end do
!
          end if
   10     continue
        end do
!
      end do
!
      end subroutine search_node_in_all_element
!
!-----------------------------------------------------------------------
!
      subroutine giveup_to_search_element(my_rank_2nd, error_level,     &
     &          inod_next_stack_4_node, org_node, org_ele, dest_node,   &
     &          itp_coef_dest)
!
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
!
      type(node_data), intent(in) :: dest_node
!
      integer(kind = kint), intent(in)                                  &
     &                 :: inod_next_stack_4_node(0:dest_node%numnod)
!
      integer(kind = kint), intent(in) :: my_rank_2nd
      real(kind = kreal), intent(in) :: error_level
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      integer(kind = kint) :: ip, ist, ied, inod, jele
      integer(kind = kint) :: kst, ked
      integer(kind = kint), parameter :: iflag_message = 1
!
!
      do ip = 1, np_smp
        ist = dest_node%istack_internal_smp(ip-1) + 1
        ied = dest_node%istack_internal_smp(ip)
!
        do inod = ist, ied
!
          if ( iflag_org_domain(inod) .le. 0) then
            kst = inod_next_stack_4_node(inod-1) + 1
            ked = inod_next_stack_4_node(inod)
            write(*,*) 'inod_next_4_node(:)', inod, kst,ked
            
!
            differ_tmp = 1.0d20
            iflag_org_tmp = 0
            do jele = 1, org_ele%numele
!
              call s_cal_interpolate_coefs                              &
     &           (dest_node, org_node, org_ele, my_rank_2nd,            &
     &            inod, jele, error_level, iflag_message,               &
     &            iflag_org_tmp, itp_coef_dest)
              if ( iflag_org_domain(inod) .gt. 0) go to  10
            end do
!
            iflag_org_domain(inod) = iflag_org_tmp
!
          end if
   10     continue
        end do
!
      end do
!
      end subroutine giveup_to_search_element
!
!-----------------------------------------------------------------------
!
      end module search_node_in_element
