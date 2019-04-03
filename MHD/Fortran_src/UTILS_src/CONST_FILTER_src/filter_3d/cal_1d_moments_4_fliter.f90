!
!      module cal_1d_moments_4_fliter
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine s_cal_1d_moments                                     &
!!     &         (num_ref_filter, iref_filter_type, FEM_elen)
!!      subroutine s_cal_1d_moments_4_filter                            &
!!     &         (num_ref_filter, iref_filter_type, FEM_elen, ref_m)
!!        type(gradient_model_data_type), intent(inout) :: FEM_elen
!!        type(reference_moments), intent(inout) :: ref_m
!
      module cal_1d_moments_4_fliter
!
      use m_precision
      use m_constants
!
      use t_filter_elength
!
      use t_ctl_params_4_gen_filter
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
      subroutine s_cal_1d_moments                                       &
     &         (num_ref_filter, iref_filter_type, FEM_elen)
!
      integer(kind = kint), intent(in) :: num_ref_filter
      integer(kind = kint), intent(in)                                  &
     &                      :: iref_filter_type(num_ref_filter)
      type(gradient_model_data_type), intent(inout) :: FEM_elen
!
      integer(kind = kint) :: i
!
!
      allocate( f_mom(0:itwo) )
!
      do i = 1, num_ref_filter
        if      (iref_filter_type(i) .eq. iflag_tophat_filter) then
          call int_tophat_moment_infty                                  &
     &       (itwo, f_mom, FEM_elen%filter_conf%f_width(i) )
        else if (iref_filter_type(i) .eq. iflag_linear_filter) then
          call int_linear_moment_infty                                  &
     &       (itwo, f_mom, FEM_elen%filter_conf%f_width(i) )
        else if (iref_filter_type(i) .eq. iflag_gaussian_filter) then
          call int_gaussian_moment_infty                                &
     &       (itwo, f_mom, FEM_elen%filter_conf%f_width(i) )
        end if
!
        FEM_elen%filter_conf%xmom_1d_org(i,0:itwo) = f_mom(0:itwo)
!
      end do
!
      deallocate( f_mom )
!
      end subroutine s_cal_1d_moments
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_1d_moments_4_filter                              &
     &         (num_ref_filter, iref_filter_type, FEM_elen, ref_m)
!
      use t_reference_moments
!
      integer(kind = kint), intent(in) :: num_ref_filter
      integer(kind = kint), intent(in)                                  &
     &                      :: iref_filter_type(num_ref_filter)
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(reference_moments), intent(inout) :: ref_m
!
      integer(kind = kint) :: num_moment3
!
!
      num_moment3 = 3 * ref_m%max_num_order_1d
!
      if      (iref_filter_type(1) .eq. iflag_tophat_filter) then
        call int_tophat_moment_infty                                    &
     &     (num_moment3, ref_m%ref_moments_1d(0),                       &
     &      FEM_elen%filter_conf%f_width(1) )
      else if (iref_filter_type(1) .eq. iflag_linear_filter) then
        call int_linear_moment_infty                                    &
     &     (num_moment3, ref_m%ref_moments_1d(0),                       &
     &      FEM_elen%filter_conf%f_width(1) )
      else if (iref_filter_type(1) .eq. iflag_gaussian_filter) then
        call int_gaussian_moment_infty                                  &
     &     (num_moment3, ref_m%ref_moments_1d(0),                       &
     &      FEM_elen%filter_conf%f_width(1) )
      end if
!
      end subroutine s_cal_1d_moments_4_filter
!
! -----------------------------------------------------------------------!
      end module cal_1d_moments_4_fliter

