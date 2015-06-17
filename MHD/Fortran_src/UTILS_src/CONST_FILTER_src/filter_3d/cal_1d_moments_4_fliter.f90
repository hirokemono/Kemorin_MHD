!
!      module cal_1d_moments_4_fliter
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_cal_1d_moments
!      subroutine s_cal_1d_moments_4_filter
!
      module cal_1d_moments_4_fliter
!
      use m_precision
!
      use m_constants
      use m_ctl_params_4_gen_filter
      use m_filter_elength
      use m_reference_moments
!
      use int_tophat_moments
      use int_linear_moments
      use int_gaussian_moments
!
      implicit none
!
      real(kind= kreal), allocatable :: f_mom(:)
      private :: f_mom
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_1d_moments
!
      integer(kind = kint) :: i
!
!
      allocate( f_mom(0:itwo) )
!
      do i = 1, num_ref_filter
!
        if      (iref_filter_type(i) .eq. iflag_tophat_filter) then
          call int_tophat_moment_infty(itwo, f_mom, f_width(i) )
        else if (iref_filter_type(i) .eq. iflag_linear_filter) then
          call int_linear_moment_infty(itwo, f_mom, f_width(i) )
        else if (iref_filter_type(i) .eq. iflag_gaussian_filter) then
          call int_gaussian_moment_infty(itwo, f_mom, f_width(i) )
        end if
!
        filter_conf1%xmom_1d_org(i,0:itwo) = f_mom(0:itwo)
!
      end do
!
      deallocate( f_mom )
!
      end subroutine s_cal_1d_moments
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_1d_moments_4_filter
!
      integer(kind = kint) :: num_moment3
!
!
      num_moment3 = 3*max_num_order_1d
!
      if      (iref_filter_type(1) .eq. iflag_tophat_filter) then
        call int_tophat_moment_infty(num_moment3, ref_moments_1d(0),    &
     &      f_width(1) )
      else if (iref_filter_type(1) .eq. iflag_linear_filter) then
        call int_linear_moment_infty(num_moment3, ref_moments_1d(0),    &
     &      f_width(1) )
      else if (iref_filter_type(1) .eq. iflag_gaussian_filter) then
        call int_gaussian_moment_infty(num_moment3, ref_moments_1d(0),  &
     &      f_width(1) )
      end if
!
      end subroutine s_cal_1d_moments_4_filter
!
! -----------------------------------------------------------------------!
      end module cal_1d_moments_4_fliter

