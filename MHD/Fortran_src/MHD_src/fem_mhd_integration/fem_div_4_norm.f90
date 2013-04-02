!
!      module fem_div_4_norm
!
!      Written by H. Matsui
!      Modified by H. Matsui on Aug, 2006
!
!      subroutine fem_div_4_norm_pg(iele_fsmp_stack, n_int, k2,         &
!     &          vect_e, sk1)
!      subroutine fem_rms_flux_pg(iele_fsmp_stack, n_int, k2,           &
!     &          vect_e, sk1)
!
      module fem_div_4_norm
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_jacobians
!
      use fem_skv_div_normal
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_div_4_norm_pg(iele_fsmp_stack, n_int, k2,          &
     &          vect_e, sk1)
!
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vect_e(numele,3)
      real (kind=kreal), intent(inout)  :: sk1(numele)
!
!
      call fem_skv_div_normal_pg(numele, nnod_4_ele,                    &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, n_int,            &
     &          xjac, dwx, k2, vect_e, sk1)
!
      end subroutine fem_div_4_norm_pg
!
! ----------------------------------------------------------------------
!
      subroutine fem_rms_flux_pg(iele_fsmp_stack, n_int, k2,            &
     &          vect_e, sk1)
!
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vect_e(numele,3)
      real (kind=kreal), intent(inout)  :: sk1(numele)
!
!
      call fem_skv_rms_flux_pg(numele, nnod_4_ele,                      &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, n_int,            &
     &          xjac, dwx, k2, vect_e, sk1)
!
      end subroutine fem_rms_flux_pg
!
! ----------------------------------------------------------------------
!
      end module fem_div_4_norm
