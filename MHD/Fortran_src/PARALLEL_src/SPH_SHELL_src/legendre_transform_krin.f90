!>@file   legendre_transform_krin.f90
!!@brief  module legendre_transform_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (innermost loop is spherical hermonics)
!!
!!
!!@verbatim
!!    Backward transforms
!!      subroutine leg_bwd_trans_vector_krin(nb)
!!      subroutine leg_bwd_trans_scalar_krin(nb)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_krin(nb)
!!      subroutine leg_fwd_trans_scalar_krin(nb)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
!
      module legendre_transform_krin
!
      use m_precision
      use m_work_4_sph_trans_spin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_vector_krin(nb)
!
      use legendre_bwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_b_trans_vector_krin(nb)
      call clear_b_trans_vector_krin(nb)
!
      call legendre_b_trans_vector_krin(nb)
!
      call back_b_trans_vector_krin(nb)
!
      end subroutine leg_bwd_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_krin(nb)
!
      use legendre_bwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_b_trans_scalar_krin(nb)
      call clear_b_trans_scalar_krin(nb)
!
      call legendre_b_trans_scalar_krin(nb)
!
      call back_b_trans_scalar_krin(nb)
!
      end subroutine leg_bwd_trans_scalar_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_krin(nb)
!
      use legendre_fwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_f_trans_vector_krin(nb)
      call clear_f_trans_vector_krin(nb)
!
      call legendre_f_trans_vector_krin(nb)
!
      call back_f_trans_vector_krin(nb)
!
      end subroutine leg_fwd_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_krin(nb)
!
      use legendre_fwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_f_trans_scalar_krin(nb)
      call clear_f_trans_scalar_krin(nb)
!
      call legendre_f_trans_scalar_krin(nb)
!
      call back_f_trans_scalar_krin(nb)
!
      end subroutine leg_fwd_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_krin
