!
!     module   m_iccg_parameter
!.......................................................................
!
!     Written by H. Matsui and H. Okuda in 2000
!
!!      subroutine set_control_4_DJDS_solver(DJDS_ctl)
!!        type(DJDS_control), intent(in) :: DJDS_ctl
!
      module   m_iccg_parameter
!
      use m_precision
      use t_iccg_parameter
!
      implicit  none
!
      character (len=kchara) :: precond_4_crank
      character (len=kchara) :: method_4_velo
! 
      real(kind=kreal) :: eps_crank
! 
      type(DJDS_poarameter)  :: DJDS_param1
!
      end module m_iccg_parameter
