!
!      module set_org_ele_4_each_bin
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_count_num_org_ele_4_each_bin
!      subroutine s_set_org_ele_4_each_bin
!      subroutine s_set_bin_stack_4_org_ele
!
      module set_org_ele_4_each_bin
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: s_set_start_and_end_3d_bin, s_set_start_and_end_1d_bin
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_count_num_org_ele_4_each_bin
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_element_geometry_data
!
      use m_sphere_bin_4_table
      use m_data_4_interpolate_org
!
      integer(kind = kint) :: ip, ist, ied, iele, itmp, ihash
      integer(kind = kint) :: jr_bin, jt_bin, jp_bin
      integer(kind = kint) :: isph_st(3), isph_ed(3)
      real(kind = kreal) :: x_min(3), x_max(3)
!
!
!$omp parallel do                                                       &
!$omp&  private(ist,ied,iele,ihash,jr_bin,jt_bin,jp_bin,                &
!$omp&         isph_st,isph_ed,itmp,x_min,x_max)
      do ip = 1, np_smp
        ist = iele_smp_stack_2nd(ip-1) + 1
        ied = iele_smp_stack_2nd(ip)
        do iele = ist, ied
!
          if (ie_2nd(iele,1) .le. internal_nod_2nd) then
!
            x_min(1:3) = min_sph_each_ele(iele,1:3)
            x_max(1:3) = max_sph_each_ele(iele,1:3)
            call s_set_start_and_end_3d_bin(num_sph_grid(1),            &
     &          num_sph_grid(2), num_sph_grid(3), isph_st, isph_ed,     &
     &          x_min, x_max, r_divide, theta_divide, phi_divide)
!
            do jr_bin = isph_st(1), isph_ed(1)
              do jt_bin = isph_st(2), isph_ed(2)
                do jp_bin = isph_st(3), isph_ed(3)
                  itmp = mod(jp_bin-1,num_sph_grid(3)) + 1
                  ihash = jr_bin + (itmp - 1) * num_sph_bin(1)          &
     &                 + (jt_bin-1) * num_sph_bin(1) * num_sph_bin(3)
                  nele_bin_smp(ip,ihash) = nele_bin_smp(ip,ihash) + 1
                end do
              end do
            end do
!
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_count_num_org_ele_4_each_bin
!
!-----------------------------------------------------------------------
!
      subroutine s_set_org_ele_4_each_bin
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_element_geometry_data
!
      use m_sphere_bin_4_table
      use m_data_4_interpolate_org
!
      integer(kind = kint) :: ip, ist, ied, iele, i, j, ihash
      integer(kind = kint) :: jr_bin, jt_bin, jp_bin, itmp
      integer(kind = kint) :: isph_st(3), isph_ed(3)
      real(kind = kreal) :: x_min(3), x_max(3)
!
!
      nele_bin_smp = 0
!
!$omp parallel do                                                       &
!$omp&  private(ist,ied,iele,ihash,jr_bin,jt_bin,jp_bin,                &
!$omp&         isph_st,isph_ed,x_min,x_max,i,j,itmp)
      do ip = 1, np_smp
        ist = iele_smp_stack_2nd(ip-1) + 1
        ied = iele_smp_stack_2nd(ip)
        do iele = ist, ied
!
          if (ie_2nd(iele,1) .le. internal_nod_2nd) then
!
            x_min(1:3) = min_sph_each_ele(iele,1:3)
            x_max(1:3) = max_sph_each_ele(iele,1:3)
            call s_set_start_and_end_3d_bin(num_sph_grid(1),            &
     &          num_sph_grid(2), num_sph_grid(3), isph_st, isph_ed,     &
     &          x_min, x_max, r_divide, theta_divide, phi_divide)
!
            do jr_bin = isph_st(1), isph_ed(1)
              do jt_bin = isph_st(2), isph_ed(2)
                do jp_bin = isph_st(3), isph_ed(3)
!
                  itmp = mod(jp_bin-1,num_sph_grid(3)) + 1
                  ihash = jr_bin + (itmp - 1) * num_sph_bin(1)         &
     &                 + (jt_bin-1) * num_sph_bin(1) * num_sph_bin(3)
                 i = (ihash-1) * np_smp + ip
                  nele_bin_smp(ip,ihash) = nele_bin_smp(ip,ihash) + 1
                  j = iele_stack_bin_smp(i-1) + nele_bin_smp(ip,ihash)
                  iele_in_bin(j) = iele
                end do
              end do
            end do
!
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_set_org_ele_4_each_bin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_set_bin_stack_4_org_ele
!
      use m_2nd_geometry_param
      use m_sphere_bin_4_table
      use m_data_4_interpolate_org
!
      integer(kind = kint) :: ihash, ip, i
!
      nele_bin = 0
      iele_stack_bin(0) = 0
      iele_stack_bin_smp(0) = 0
      do ihash = 1, ntot_sph_bin
        do ip = 1, np_smp
          i = (ihash-1) * np_smp + ip
          iele_stack_bin_smp(i) = iele_stack_bin_smp(i-1)               &
     &                          + nele_bin_smp(ip,ihash)
          nele_bin(ihash) = nele_bin(ihash) + nele_bin_smp(ip,ihash)
        end do
        iele_stack_bin(ihash) = iele_stack_bin(ihash-1)                 &
     &                         + nele_bin(ihash)
      end do
      ntot_org_ele_in_bin = iele_stack_bin(ntot_sph_bin)
!
      end subroutine s_set_bin_stack_4_org_ele
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_start_and_end_3d_bin(num_bin1, num_bin2,         &
     &          num_bin3, ist, ied, x_min, x_max, ref_vect1,            &
     &          ref_vect2, ref_vect3)
!
      integer(kind = kint), intent(in) :: num_bin1, num_bin2, num_bin3
      real(kind = kreal), intent(in) :: x_min(3), x_max(3)
      real(kind = kreal), intent(in) :: ref_vect1(0:num_bin1)
      real(kind = kreal), intent(in) :: ref_vect2(0:num_bin2)
      real(kind = kreal), intent(in) :: ref_vect3(0:num_bin3)
!
      integer(kind = kint), intent(inout) :: ist(3), ied(3)
!
!
      call s_set_start_and_end_1d_bin(num_bin1, ist(1), ied(1),         &
     &          x_min(1), x_max(1), ref_vect1)
      call s_set_start_and_end_1d_bin(num_bin2, ist(2), ied(2),         &
     &          x_min(2), x_max(2), ref_vect2)
      call s_set_start_and_end_1d_bin(num_bin3, ist(3), ied(3),         &
     &          x_min(3), x_max(3), ref_vect3)
!
      if (x_max(1) .ge. ref_vect1(num_bin1)) then
         ied(1) = num_bin1 + 1
      end if
!
      end subroutine s_set_start_and_end_3d_bin
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_start_and_end_1d_bin(num_bin, ist, ied,          &
     &          x_min, x_max, ref_vect)
!
      integer(kind = kint), intent(in) :: num_bin
      real(kind = kreal), intent(in) :: x_min, x_max
      real(kind = kreal), intent(in) :: ref_vect(0:num_bin)
!
      integer(kind = kint), intent(inout) :: ist, ied
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_bin
        if (x_min .le. ref_vect(i)) then
          ist = i
          exit
        end if
      end do
      ied = num_bin
      do i = ist, num_bin
        if (x_max .lt. ref_vect(i-1)) then
          ied = i-1
          exit
        end if
      end do
!
      end subroutine s_set_start_and_end_1d_bin
!
!  ---------------------------------------------------------------------
!
      end module set_org_ele_4_each_bin
