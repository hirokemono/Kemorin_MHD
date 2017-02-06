!merge_coefs_whole_dynamic.f90
!      module merge_coefs_whole_dynamic
!
!      written by H. Matsui on Nov., 2009
!
!!      subroutine s_merge_coefs_w_dynamic(iflag_Csim_marging,          &
!!     &          numdir, cor, sgs_wg,c_comps, c_fields)
!
      module merge_coefs_whole_dynamic
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: sum_by_direction_w_dynamic, ave_by_direction_w_dynamic
      private :: ave_by_correlate_w_dynamic
      private :: cal_each_components_w_coefs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_merge_coefs_w_dynamic(iflag_Csim_marging,            &
     &          numdir, cor, sgs_wg,c_comps, c_fields)
!
      use t_SGS_control_parameter
!
      integer (kind = kint), intent(in) :: iflag_Csim_marging
      integer (kind = kint), intent(in) :: numdir
      real(kind = kreal), intent(in) :: cor(numdir)
!
      real(kind=kreal), intent(inout) ::  sgs_wg(18)
      real(kind = kreal), intent(inout) :: c_comps(numdir)
      real(kind = kreal), intent(inout) :: c_fields
!
!
      call cal_each_components_w_coefs(numdir, sgs_wg, c_comps)
!
      if(iflag_Csim_marging .eq. id_SGS_DIR_AVERAGE) then
        call ave_by_direction_w_dynamic                                 &
     &     (numdir, sgs_wg, c_comps, c_fields)
      else if (iflag_Csim_marging .eq. id_SGS_DIR_CORRELATE) &
     & then
        call ave_by_correlate_w_dynamic                                 &
     &     (numdir, cor, sgs_wg, c_comps, c_fields)
      else
        call sum_by_direction_w_dynamic(numdir, sgs_wg, c_fields)
      end if
!
      end subroutine s_merge_coefs_w_dynamic
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_each_components_w_coefs(numdir, sgs_wg, c_comps)
!
      integer (kind = kint), intent(in) :: numdir
      real(kind=kreal), intent(in) ::  sgs_wg(18)
      real(kind = kreal), intent(inout) :: c_comps(numdir)
!
      integer (kind = kint) :: nd
!
!
      do nd = 1, numdir
        if (sgs_wg(10) .eq. 0.0d0) then
          c_comps = 0.0d0
        else
          c_comps = sgs_wg(nd) / sgs_wg(nd+9)
        end if
      end do
!
      end subroutine cal_each_components_w_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sum_by_direction_w_dynamic(numdir, sgs_wg, c_fields)
!
!
      integer (kind = kint), intent(in) :: numdir
      real(kind=kreal), intent(inout) ::  sgs_wg(18)
      real(kind = kreal), intent(inout) :: c_fields
!
      integer (kind = kint) :: nd
!
!
      do nd = 2, numdir
        sgs_wg(1 ) = sgs_wg(1 ) + sgs_wg(nd  )
        sgs_wg(10) = sgs_wg(10) + sgs_wg(9+nd)
      end do
!
      if (sgs_wg(10) .eq. 0.0d0) then
        c_fields = 0.0d0
      else
        c_fields = sgs_wg(1) / sgs_wg(10)
      end if
!
      end subroutine sum_by_direction_w_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine ave_by_direction_w_dynamic                             &
     &         (numdir, sgs_wg, c_comps, c_fields)
!
      integer (kind = kint), intent(in) :: numdir
      real(kind=kreal), intent(in) ::  sgs_wg(18)
      real(kind = kreal), intent(in) :: c_comps(numdir)
      real(kind = kreal), intent(inout) :: c_fields
!
      integer (kind = kint) :: nd
      real(kind=kreal) :: dnum_w
!
!
      dnum_w = 0.0d0
      do nd = 1, numdir
          if ( sgs_wg(nd+9) .ne. 0.0d0) dnum_w = dnum_w + 1.0d0
      end do
      if ( dnum_w .eq. 0.0d0) dnum_w = 1.0d0
!
!
      if (numdir .eq. 1) then
        c_fields = c_comps(1)
      else if (numdir .eq. 3) then
          c_fields = ( c_comps(1) + c_comps(2) + c_comps(3) ) / dnum_w
      else if (numdir .eq. 6) then
          c_fields = ( c_comps(1) + c_comps(2) + c_comps(3)             &
     &               + c_comps(4)  +c_comps(5) + c_comps(6) ) / dnum_w
      else if (numdir .eq. 9) then
          c_fields = ( c_comps(1) + c_comps(2) + c_comps(3)             &
     &               + c_comps(4) + c_comps(5) + c_comps(6)             &
     &               + c_comps(7) + c_comps(8) + c_comps(9) ) / dnum_w
      end if
!
      end subroutine ave_by_direction_w_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine ave_by_correlate_w_dynamic(numdir, cor, sgs_wg,        &
     &          c_comps, c_fields)
!
      integer (kind = kint), intent(in) :: numdir
      real(kind = kreal), intent(in) :: cor(numdir)
      real(kind=kreal), intent(in) ::  sgs_wg(18)
!
      real(kind = kreal), intent(in) :: c_comps(numdir)
      real(kind = kreal), intent(inout) :: c_fields
!
      integer (kind = kint) :: nd
      real(kind=kreal) :: dnum_w
!
!
      dnum_w = 0.0d0
      do nd = 1, numdir
          if ( sgs_wg(nd+9) .ne. 0.0d0)  dnum_w = dnum_w + cor(nd)
      end do
      if ( dnum_w .eq. 0.0d0) dnum_w = 1.0d0
!
!
      if (numdir .eq. 1) then
          c_fields = c_comps(1)
      else if (numdir .eq. 3) then
          c_fields = ( c_comps(1) * cor(1) + c_comps(2) * cor(2)        &
     &               + c_comps(3) * cor(3) ) / dnum_w
      else if (numdir .eq. 6) then
          c_fields = (   c_comps(1) * cor(1) + two*c_comps(2) * cor(2)  &
     &             + two*c_comps(3) * cor(3) +     c_comps(4) * cor(4)  &
     &             + two*c_comps(5) * cor(5) +     c_comps(6) * cor(6)) &
     &               / dnum_w
      else if (numdir .eq. 9) then
          c_fields = ( c_comps(1) * cor(1) + c_comps(2) * cor(2)        &
     &               + c_comps(3) * cor(3) + c_comps(4) * cor(4)        &
     &               + c_comps(5) * cor(5) + c_comps(6) * cor(6)        &
     &               + c_comps(7) * cor(7) + c_comps(8) * cor(8)        &
     &               + c_comps(9) * cor(9) ) / dnum_w
      end if
!
      end subroutine ave_by_correlate_w_dynamic
!
! ----------------------------------------------------------------------
!
      end module merge_coefs_whole_dynamic
