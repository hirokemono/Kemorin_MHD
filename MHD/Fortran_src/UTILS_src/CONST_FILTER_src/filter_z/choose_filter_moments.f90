!
!      module choose_filter_moments
!
      module choose_filter_moments
!
!        programmed by H. Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_choose_filter_moments
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_choose_filter_moments
!
      use m_commute_filter_z
      use m_filter_values
      use int_gaussian_moments
      use int_linear_moments
      use int_tophat_moments
!
!
      if ( iflag_filter .eq. 0) then
        call int_tophat_moment_infty(nfilter6_1,f_mom_full,f_width)
      else if (iflag_filter .eq. 1) then
        call int_linear_moment_infty(nfilter6_1,f_mom_full,f_width)
      else
        call int_gaussian_moment_infty(nfilter6_1,f_mom_full,f_width)
      end if
!
!
      end subroutine s_choose_filter_moments
!
!-----------------------------------------------------------------------
!
      end module choose_filter_moments
