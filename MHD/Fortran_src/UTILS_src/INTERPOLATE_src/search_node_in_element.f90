!
!      module search_node_in_element
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine search_node_in_element_1st(my_rank, new_node, new_ele)
!      subroutine search_node_in_element_2nd(my_rank, i_sleeve,         &
!     &          error_level, new_node, new_ele)
!      subroutine search_node_in_all_element(my_rank_2nd, error_level   &
!     &          new_node, new_ele)
!      subroutine giveup_to_search_element(my_rank_2nd, error_level,    &
!     &          new_node, new_ele)
!
      module search_node_in_element
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_interpolate_table_dest
      use m_work_const_itp_table
      use m_sphere_bin_4_table
      use m_data_4_interpolate_org
      use cal_interpolate_coefs
!
      use t_geometry_data
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
      subroutine search_node_in_element_1st(my_rank, new_node, new_ele)
!
      integer(kind = kint), intent(in) :: my_rank
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
      integer(kind = kint) :: ip, ist, ied, inod, iflag
      integer(kind = kint) :: ihash, jst, jed, jnum, jele
      integer(kind = kint), parameter :: iflag_nomessage = 0
!
!
      do ip = 1, np_smp
!
        ist = inter_smp_stack(ip-1) + 1
        ied = inter_smp_stack(ip)
!
        do inod = ist, ied
!
          if ( iflag_org_domain(inod) .le. 0) then
            ihash =   id_search_area(inod,1)                            &
     &             + (id_search_area(inod,3) - 1) * num_sph_bin(1)      &
     &             + (id_search_area(inod,2) - 1) * num_sph_bin(1)      &
     &                                           * num_sph_bin(3)
!
            jst = iele_stack_bin(ihash-1) + 1
            jed = iele_stack_bin(ihash)
!
            do jnum = jst, jed
!
              jele = iele_in_bin(jnum)
              iflag = 0
!
              if (   radius(inod)     .ge. min_sph_each_ele(jele,1)     &
     &         .and. radius(inod)     .le. max_sph_each_ele(jele,1)     &
     &         .and. colatitude(inod) .ge. min_sph_each_ele(jele,2)     &
     &         .and. colatitude(inod) .le. max_sph_each_ele(jele,2)     &
     &         .and. longitude(inod)  .ge. min_sph_each_ele(jele,3)     &
     &         .and. longitude(inod)  .le. max_sph_each_ele(jele,3)     &
     &              ) then
!
                 call s_cal_interpolate_coefs                           &
     &              (new_node, new_ele, my_rank, inod, jele,            &
     &               zero, iflag_nomessage, iflag_org_tmp)
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
      subroutine search_node_in_element_2nd(my_rank, i_sleeve,          &
     &          error_level, new_node, new_ele)
!
      integer(kind = kint), intent(in) :: my_rank, i_sleeve
      real(kind = kreal), intent(in) :: error_level
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
      integer(kind = kint) :: ip, ist, ied, inod
      integer(kind = kint) :: ihash, ihash_r, ihash_t, ihash_p
      integer(kind = kint) :: jst, jed, jnum, jele
      integer(kind = kint) :: kr, kt, kp
      integer(kind = kint), parameter :: iflag_nomessage = 0
!
!
      do ip = 1, np_smp
!
        ist = inter_smp_stack(ip-1) + 1
        ied = inter_smp_stack(ip)
!
        do inod = ist, ied
!
          if ( iflag_org_domain(inod) .le. 0) then
            do kr = -i_sleeve, i_sleeve
              do kt = -i_sleeve, i_sleeve
                do kp = -i_sleeve, i_sleeve
!
                  ihash_r = id_search_area(inod,1) + kr
                  ihash_t = id_search_area(inod,2) + kt
                  ihash_p = mod( (id_search_area(inod,3)+num_sph_bin(3) &
     &                     +kp-1), num_sph_bin(3) ) + 1
                  ihash =   ihash_r                                     &
     &                   + (ihash_p-1) * num_sph_bin(1)                 &
     &                   + (ihash_t-1) * num_sph_bin(1)*num_sph_bin(3)
!
!
                  if (   ihash_r.ge.1 .and. ihash_r .le. num_sph_bin(1) &
     &             .and. ihash_t.ge.1 .and. ihash_t .le. num_sph_bin(2) &
     &               ) then
!
                    jst = iele_stack_bin(ihash-1) + 1
                    jed = iele_stack_bin(ihash)
!
                    do jnum = jst, jed
!
                      jele = iele_in_bin(jnum)
                      call s_cal_interpolate_coefs                      &
     &                   (new_node, new_ele, my_rank,                   &
     &                    inod, jele, error_level, iflag_nomessage,     &
     &                    iflag_org_tmp)
                      if ( iflag_org_domain(inod) .gt. 0) go to 10
                    end do
                  end if
!
                end do
              end do
            end do
!
          end if
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
     &          new_node, new_ele)
!
      integer(kind = kint), intent(in) :: my_rank_2nd
      real(kind = kreal), intent(in) :: error_level
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
      integer(kind = kint) :: ip, ist, ied, inod, jele
      integer(kind = kint), parameter :: iflag_message = 1
!
!
      do ip = 1, np_smp
!
        ist = inter_smp_stack(ip-1) + 1
        ied = inter_smp_stack(ip)
!
        do inod = ist, ied
!
          if ( iflag_org_domain(inod) .le. 0) then
!
            differ_tmp = 1.0d20
            iflag_org_tmp = 0
            do jele = 1, new_ele%numele
!
              call s_cal_interpolate_coefs                              &
     &           (new_node, new_ele, my_rank_2nd, inod, jele,           &
     &            error_level, iflag_message, iflag_org_tmp)
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
     &          new_node, new_ele)
!
      integer(kind = kint), intent(in) :: my_rank_2nd
      real(kind = kreal), intent(in) :: error_level
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
      integer(kind = kint) :: ip, ist, ied, inod, jele
      integer(kind = kint), parameter :: iflag_message = 1
!
!
      do ip = 1, np_smp
!
        ist = inter_smp_stack(ip-1) + 1
        ied = inter_smp_stack(ip)
!
        do inod = ist, ied
!
          if ( iflag_org_domain(inod) .le. 0) then
!
            differ_tmp = 1.0d20
            iflag_org_tmp = 0
            do jele = 1, new_ele%numele
!
              call s_cal_interpolate_coefs                              &
      &          (new_node, new_ele, my_rank_2nd, inod, jele,           &
     &            error_level, iflag_message, iflag_org_tmp)
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
