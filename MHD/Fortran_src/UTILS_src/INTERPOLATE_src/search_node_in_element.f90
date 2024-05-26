!
!      module search_node_in_element
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine search_node_in_element_1st                           &
!!     &         (id_rank, gen_itp_p, org_node, org_ele, org_blk,       &
!!     &          nnod_dest, internod_dest, xx_dest,                    &
!!     &          itp_coef_dest, iflag_org_domain)
!!      subroutine search_node_in_element_2nd                           &
!!     &         (iinc, id_rank, gen_itp_p, org_node, org_ele, org_blk, &
!!     &          nnod_dest, internod_dest, xx_dest,                    &
!!     &          itp_coef_dest, iflag_org_domain)
!!      subroutine search_node_in_all_element(my_rank_2nd, error_level, &
!!     &          gen_itp_p, org_node, org_ele,                         &
!!     &          nnod_dest, internod_dest, xx_dest,                    &
!!     &          itp_coef_dest, iflag_org_domain)
!!      subroutine giveup_to_search_element                             &
!!     &         (my_rank_2nd, error_level, inod_next_stack_4_node,     &
!!     &          gen_itp_p, org_node, org_ele,                         &
!!     &          nnod_dest, internod_dest, xx_dest,                    &
!!     &          itp_coef_dest, iflag_org_domain)
!!       type(node_data), intent(in) :: org_node
!!       type(element_data), intent(in) :: org_ele
!!       type(block_4_interpolate), intent(in) :: org_blk
!!       type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      module search_node_in_element
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use cal_interpolate_coefs
!
      use t_search_block_4_itp
      use t_geometry_data
      use t_interpolate_coefs_dest
      use t_ctl_params_4_gen_table
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
      subroutine search_node_in_element_1st                             &
     &         (id_rank, gen_itp_p, org_node, org_ele, org_blk,         &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
!
      use subroutines_4_search_table
!
      integer, intent(in) :: id_rank
!
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(block_4_interpolate), intent(in) :: org_blk
!
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout)                               &
     &                    :: iflag_org_domain(nnod_dest)
!
      real(kind = kreal) :: x_target(3)
      integer(kind = kint) :: ip, ist, ied, inod
      integer(kind = kint) :: ihash, jst, jed, jnum, jele
      integer(kind = kint), parameter :: iflag_nomessage = 0
!
!
      do inod = 1, internod_dest
        ihash = org_blk%iblock_tgt_node(inod,4)
!
        if(ihash .gt. 0) then
          jst = org_blk%ele_list_by_rng%istack_grp(ihash-1) + 1
          jed = org_blk%ele_list_by_rng%istack_grp(ihash)
!
          do jnum = jst, jed
            jele = org_blk%ele_list_by_rng%item_grp(jnum)
            x_target(1:3) = xx_dest(inod,1:3)
!
            if (     x_target(1) .ge. org_blk%xele_min(jele,1)          &
     &         .and. x_target(1) .le. org_blk%xele_max(jele,1)          &
     &         .and. x_target(2) .ge. org_blk%xele_min(jele,2)          &
     &         .and. x_target(2) .le. org_blk%xele_max(jele,2)          &
     &         .and. x_target(3) .ge. org_blk%xele_min(jele,3)          &
     &         .and. x_target(3) .le. org_blk%xele_max(jele,3)          &
     &              ) then
!
              call s_cal_interpolate_coefs                              &
     &           (x_target, gen_itp_p, org_node, org_ele, id_rank,      &
     &            inod, jele, zero, iflag_nomessage, iflag_org_tmp,     &
     &            iflag_org_domain(inod), itp_coef_dest)
              if ( iflag_org_domain(inod) .gt. 0) go to 10
            end if
!
          end do
        end if
 10     continue
      end do
!
      end subroutine search_node_in_element_1st
!
!-----------------------------------------------------------------------
!
      subroutine search_node_in_element_2nd                             &
     &         (iinc, id_rank, gen_itp_p, org_node, org_ele, org_blk,   &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
!
      use subroutines_4_search_table
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iinc
!
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(block_4_interpolate), intent(in) :: org_blk
!
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout)                               &
     &                    :: iflag_org_domain(nnod_dest)
!
      real(kind = kreal) :: x_target(3)
      integer(kind = kint) :: ip, ist, ied, inod, i1, i2, i3
      integer(kind = kint) :: igh(3)
      integer(kind = kint) :: ihash, jst, jed, jnum, jele
      integer(kind = kint), parameter :: iflag_nomessage = 0
!
!
      do inod = 1, internod_dest
        igh(1:3) = org_blk%iblock_tgt_node(inod,1:3)
        do i1 = igh(1)-iinc+1, igh(1)+iinc-1, iinc
          if(i1.lt.1 .or. i1.gt.org_blk%num_itp_block(1)) cycle
          do i2 = igh(2)-iinc+1, igh(2)+iinc-1, iinc
            if(i2.lt.1 .or. i2.gt.org_blk%num_itp_block(2)) cycle
            do i3 = igh(3)-iinc+1, igh(3)+iinc-1, iinc
              if(i3.lt.1 .or. i3.gt.org_blk%num_itp_block(3)) cycle
              ihash =  i1 + (i2 - 1) * org_blk%num_itp_block(1)         &
     &                + (i3 - 1) * org_blk%num_itp_block(1)             &
     &                           * org_blk%num_itp_block(2)
!
              if(ihash .gt. 0) then
                jst = org_blk%ele_list_by_rng%istack_grp(ihash-1) + 1
                jed = org_blk%ele_list_by_rng%istack_grp(ihash)
!
                do jnum = jst, jed
                  jele = org_blk%ele_list_by_rng%item_grp(jnum)
                  x_target(1:3) = xx_dest(inod,1:3)
                  call s_cal_interpolate_coefs(x_target, gen_itp_p,     &
     &                org_node, org_ele, id_rank, inod, jele, zero,     &
     &                iflag_nomessage, iflag_org_tmp,                   &
     &                iflag_org_domain(inod), itp_coef_dest)
                  if ( iflag_org_domain(inod) .gt. 0) go to 10
!
                end do
              end if
            end do
          end do
        end do
 10     continue
      end do
!
      end subroutine search_node_in_element_2nd
!
!-----------------------------------------------------------------------
!
      subroutine search_node_in_all_element(my_rank_2nd, error_level,   &
     &          gen_itp_p, org_node, org_ele,                           &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
!
      use subroutines_4_search_table
!
      integer, intent(in) :: my_rank_2nd
      real(kind = kreal), intent(in) :: error_level
!
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
!
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout)                               &
     &                    :: iflag_org_domain(nnod_dest)
!
      real(kind = kreal) :: x_target(3)
      integer(kind = kint) :: ip, ist, ied, inod, jele
      integer(kind = kint), parameter :: iflag_message = 1
!
!
      do inod = 1, internod_dest
        if ( iflag_org_domain(inod) .le. 0) then
!
          differ_tmp = 1.0d20
          iflag_org_tmp = 0
          do jele = 1, org_ele%numele
            x_target(1:3) = xx_dest(inod,1:3)
            call s_cal_interpolate_coefs                                &
     &         (x_target, gen_itp_p, org_node, org_ele, my_rank_2nd,    &
     &          inod, jele, error_level, iflag_message, iflag_org_tmp,  &
     &         iflag_org_domain(inod), itp_coef_dest)
            if ( iflag_org_domain(inod) .gt. 0) go to  10
          end do
!
        end if
   10   continue
      end do
!
      end subroutine search_node_in_all_element
!
!-----------------------------------------------------------------------
!
      subroutine giveup_to_search_element                               &
     &         (my_rank_2nd, error_level, inod_next_stack_4_node,       &
     &          gen_itp_p, org_node, org_ele,                           &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
!
      use subroutines_4_search_table
!
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
!
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
!
      integer(kind = kint), intent(in)                                  &
     &                 :: inod_next_stack_4_node(0:nnod_dest)
!
      integer, intent(in) :: my_rank_2nd
      real(kind = kreal), intent(in) :: error_level
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout)                               &
     &                    :: iflag_org_domain(nnod_dest)
!
      integer(kind = kint) :: ip, ist, ied, inod, jele
      integer(kind = kint) :: kst, ked
      real(kind = kreal) :: x_target(3)
      integer(kind = kint), parameter :: iflag_message = 1
!
!
      do inod = 1, internod_dest
        if ( iflag_org_domain(inod) .le. 0) then
          kst = inod_next_stack_4_node(inod-1) + 1
          ked = inod_next_stack_4_node(inod)
          write(*,*) 'inod_next_4_node(:)', inod, kst,ked
            
!
          differ_tmp = 1.0d20
          iflag_org_tmp = 0
          do jele = 1, org_ele%numele
            x_target(1:3) = xx_dest(inod,1:3)
            call s_cal_interpolate_coefs                                &
     &        (x_target, gen_itp_p, org_node, org_ele, my_rank_2nd,     &
     &         inod, jele, error_level, iflag_message, iflag_org_tmp,   &
     &         iflag_org_domain(inod), itp_coef_dest)
            if ( iflag_org_domain(inod) .gt. 0) go to  10
          end do
!
          iflag_org_domain(inod) = iflag_org_tmp
        end if
   10   continue
!
      end do
!
      end subroutine giveup_to_search_element
!
!-----------------------------------------------------------------------
!
      end module search_node_in_element
